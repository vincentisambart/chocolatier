use crate::ast;
use std::borrow::Cow;
use std::collections::HashMap;
use std::fmt::Write;

const BASE_OBJC_TRAIT: &'static str = "ObjCPtr";
const CORE_MOD: &'static str = "core";
const BASE_OBJC_TRAIT_FULL_PATH: &'static str = "crate::core::ObjCPtr";

trait OriginExt {
    fn mod_name(&self) -> Cow<str>;
}

impl OriginExt for ast::Origin {
    fn mod_name(&self) -> Cow<str> {
        use ast::Origin;
        match self {
            Origin::ObjCCore | Origin::System => CORE_MOD.into(),
            Origin::Framework(framework) => snake_case_split(framework)
                .join("_")
                .to_ascii_lowercase()
                .into(),
            Origin::Library(lib) => lib.trim_start_matches("lib").into(),
        }
    }
}

#[derive(Debug)]
struct TypeIndex {
    enums: HashMap<ast::TagId, ast::EnumDef>,
    structs: HashMap<ast::TagId, ast::RecordDef>,
    unions: HashMap<ast::TagId, ast::RecordDef>,
    typedefs: HashMap<String, ast::TypedefDecl>,
    protocols: HashMap<String, ast::ProtocolDef>,
    interfaces: HashMap<String, ast::InterfaceDef>,
    categories: HashMap<String, Vec<ast::CategoryDef>>,
    functions: HashMap<String, ast::FuncDecl>,
}

impl TypeIndex {
    fn from(decls: &[ast::Decl]) -> Self {
        use ast::{Decl, RecordKind};

        let mut enums = HashMap::new();
        let mut structs = HashMap::new();
        let mut unions = HashMap::new();
        let mut typedefs = HashMap::new();
        let mut protocols = HashMap::new();
        let mut interfaces = HashMap::new();
        let mut categories = HashMap::new();
        let mut functions = HashMap::new();

        for decl in decls {
            match decl {
                Decl::ProtocolDef(def) => {
                    protocols.insert(def.name.clone(), def.clone());
                }
                Decl::InterfaceDef(def) => {
                    interfaces.insert(def.name.clone(), def.clone());
                }
                Decl::CategoryDef(def) => {
                    let vec = categories.entry(def.class.clone()).or_insert_with(Vec::new);
                    vec.push(def.clone());
                }
                Decl::RecordDef(def) => match def.kind {
                    RecordKind::Union => {
                        unions.insert(def.id.clone(), def.clone());
                    }
                    RecordKind::Struct => {
                        structs.insert(def.id.clone(), def.clone());
                    }
                },
                Decl::EnumDef(def) => {
                    enums.insert(def.id.clone(), def.clone());
                }
                Decl::TypedefDecl(decl) => {
                    typedefs.insert(decl.name.clone(), decl.clone());
                }
                Decl::FuncDecl(decl) => {
                    functions.insert(decl.name.clone(), decl.clone());
                }
            }
        }

        TypeIndex {
            enums,
            structs,
            unions,
            typedefs,
            protocols,
            interfaces,
            categories,
            functions,
        }
    }

    fn protoc_mod(&self, protocol: &str) -> Cow<str> {
        self.protocols[protocol].origin.as_ref().unwrap().mod_name()
    }

    fn interf_mod(&self, protocol: &str) -> Cow<str> {
        self.interfaces[protocol]
            .origin
            .as_ref()
            .unwrap()
            .mod_name()
    }
}

fn rust_type_name_for_fixed_int(name: &str) -> Option<&'static str> {
    use lazy_static::lazy_static;
    use regex::Regex;

    lazy_static! {
        static ref FIXED_SIZE_INT_RE: Regex = Regex::new(r"(?i)\A([us]?)int([0-9]+)").unwrap();
    }
    match FIXED_SIZE_INT_RE.captures(name) {
        Some(cap) => {
            let signed = match &cap[1] {
                "U" | "u" => false,
                "S" | "s" | "" => true,
                _ => unreachable!(),
            };
            Some(match &cap[2] {
                "8" => {
                    if signed {
                        "i8"
                    } else {
                        "u8"
                    }
                }
                "16" => {
                    if signed {
                        "i16"
                    } else {
                        "u16"
                    }
                }
                "32" => {
                    if signed {
                        "i32"
                    } else {
                        "u32"
                    }
                }
                "64" => {
                    if signed {
                        "i64"
                    } else {
                        "u64"
                    }
                }
                _ => unimplemented!("unrecognized fixed in type {}", name),
            })
        }
        None => None,
    }
}

// TODO: Special handling for CFStringEncoding, NSStringEncoding, SSLCipherSuite, ExtAudioFilePropertyID, NSTextCheckingTypes...
fn rust_type_name_for_enum_underlying(
    underlying: &ast::ObjCType,
    index: &TypeIndex,
) -> &'static str {
    use ast::{NumKind, ObjCType};

    match underlying {
        ObjCType::Typedef(typedef) => match rust_type_name_for_fixed_int(&typedef.name) {
            Some(name) => name,
            None => {
                let objc_type = &index.typedefs[&typedef.name].underlying;
                rust_type_name_for_enum_underlying(&objc_type, index)
            }
        },
        // There are platforms the following is not true (long is 32-bit on Windows),
        // but that should be true on all Apple platforms.
        ObjCType::Num(kind) => match kind {
            NumKind::SChar => "i8",
            NumKind::UChar => "u8",
            NumKind::Int => "i32",
            NumKind::UInt => "u32",
            NumKind::Long => "isize",
            NumKind::ULong => "usize",
            NumKind::Short => "i16",
            NumKind::UShort => "u16",
            NumKind::LongLong => "i64",
            NumKind::ULongLong => "u64",
            NumKind::Float | NumKind::Double | NumKind::LongDouble => {
                panic!("enums should not use floating point as underlying type")
            }
        },
        _ => unimplemented!("unsupported type {:#?}", underlying),
    }
}

struct OutputHandler {
    src_dir: std::path::PathBuf,
    files: HashMap<String, std::fs::File>,
    main_file: std::fs::File,
}

impl OutputHandler {
    fn new() -> Self {
        use std::fs::{DirBuilder, File};

        let generated_dir = std::path::Path::new("generated");
        let src_dir = generated_dir.join("src").to_path_buf();
        let files = HashMap::new();

        DirBuilder::new().recursive(true).create(&src_dir).unwrap();
        let main_file = File::create(src_dir.join("lib.rs")).unwrap();
        Self::write_start_comment(&main_file);

        OutputHandler {
            src_dir,
            files,
            main_file,
        }
    }

    fn write_start_comment(file: &std::fs::File) {
        use std::io::Write;
        writeln!(
            &*file,
            "// File automatically generated by {} {} - Do not modify directly.",
            env!("CARGO_PKG_NAME"),
            env!("CARGO_PKG_VERSION")
        )
        .unwrap();
    }

    fn mod_file(&mut self, module: &str) -> &std::fs::File {
        use std::fs::File;
        use std::io::Write;

        if !self.files.contains_key(module) {
            let file = File::create(self.src_dir.join(module).with_extension("rs")).unwrap();
            Self::write_start_comment(&file);

            if module != CORE_MOD {
                writeln!(&file, "use crate::core::Object;").unwrap();
            }
            writeln!(&file).unwrap();
            self.files.insert(module.to_string(), file);
        }
        &self.files[module]
    }

    fn file_for(&mut self, origin: &ast::Origin) -> &std::fs::File {
        self.mod_file(&origin.mod_name())
    }
}

pub(crate) struct Generator {
    index: TypeIndex,
    decls: Vec<ast::Decl>,
    output_handler: OutputHandler,
}

fn protocol_trait_name(protocol_name: &str) -> String {
    format!("{}Protocol", protocol_name)
}

fn interface_trait_name(class_name: &str) -> String {
    format!("{}Interface", class_name)
}

impl Generator {
    pub(crate) fn new(decls: Vec<ast::Decl>) -> Self {
        let index = TypeIndex::from(&decls);
        let output_handler = OutputHandler::new();
        Generator {
            index,
            decls,
            output_handler,
        }
    }

    fn protoc_rel_path(&self, name: &str, current_mod: &str) -> Cow<str> {
        let mod_name = self.index.protoc_mod(&name);
        let trait_name = protocol_trait_name(name);
        if current_mod == mod_name {
            trait_name.into()
        } else {
            format!("crate::{}::{}", mod_name, trait_name).into()
        }
    }

    fn generate_protocol(&self, def: &ast::ProtocolDef) -> Option<String> {
        let current_mod = self.index.protoc_mod(&def.name);
        let mut inherited_traits: Vec<_> = def
            .inherited_protocols
            .iter()
            .map(|name| self.protoc_rel_path(&name, &current_mod))
            .collect();

        if inherited_traits.is_empty() {
            if current_mod == CORE_MOD {
                inherited_traits.push(BASE_OBJC_TRAIT.into());
            } else {
                inherited_traits.push(BASE_OBJC_TRAIT_FULL_PATH.into());
            }
        }

        let code = format!(
            "pub trait {protocol_trait}: {inherited_traits} {{}}",
            protocol_trait = protocol_trait_name(&def.name),
            inherited_traits = inherited_traits.join(" + "),
        );
        Some(code)
    }

    fn interf_rel_path(&self, name: &str, current_mod: &str) -> Cow<str> {
        let mod_name = self.index.interf_mod(&name);
        let trait_name = interface_trait_name(name);
        if current_mod == mod_name {
            trait_name.into()
        } else {
            format!("crate::{}::{}", mod_name, trait_name).into()
        }
    }

    fn generate_class(&self, def: &ast::InterfaceDef) -> Option<String> {
        let current_mod = self.index.interf_mod(&def.name);
        let struct_name = &def.name;
        let trait_name = interface_trait_name(struct_name);
        let base_objc_trait = if current_mod == CORE_MOD {
            BASE_OBJC_TRAIT
        } else {
            BASE_OBJC_TRAIT_FULL_PATH
        };

        let mut inherited_traits = Vec::new();
        if let Some(ref superclass) = def.superclass {
            inherited_traits.push(self.interf_rel_path(&superclass, &current_mod));
        }
        for protocol in &def.adopted_protocols {
            inherited_traits.push(self.protoc_rel_path(&protocol, &current_mod));
        }
        if inherited_traits.is_empty() {
            inherited_traits.push(base_objc_trait.into());
        }

        let mut code = String::new();

        writeln!(
            &mut code,
            "\
pub trait {trait_name}: {inherited_traits} {{}}

pub struct {struct_name} {{
    raw: std::ptr::NonNull<Object>,
}}

impl {base_objc_trait} for {struct_name} {{
    unsafe fn from_raw_unchecked(raw: std::ptr::NonNull<Object>) -> Self {{
        Self {{ raw }}
    }}

    fn as_raw(&self) -> std::ptr::NonNull<Object> {{
        self.raw
    }}
}}
",
            struct_name = struct_name,
            trait_name = trait_name,
            inherited_traits = inherited_traits.join(" + "),
            base_objc_trait = base_objc_trait,
        )
        .unwrap();

        let mut unprocessed_adopted_protocols: Vec<&str> = Vec::new();
        Extend::<&str>::extend(
            &mut unprocessed_adopted_protocols,
            def.adopted_protocols.iter().map(|name| name.as_ref()),
        );

        {
            let mut current_def = def;
            while let Some(ref superclass) = current_def.superclass {
                let superclass_path = self.interf_rel_path(superclass, &current_mod);
                writeln!(
                    &mut code,
                    "impl {superclass_path} for {struct_name} {{}}",
                    superclass_path = superclass_path,
                    struct_name = struct_name,
                )
                .unwrap();

                current_def = &self.index.interfaces[superclass];
                Extend::<&str>::extend(
                    &mut unprocessed_adopted_protocols,
                    current_def
                        .adopted_protocols
                        .iter()
                        .map(|name| name.as_ref()),
                );
            }
        }

        let mut adopted_protocols: Vec<&str> = Vec::new();
        while let Some(protocol_name) = unprocessed_adopted_protocols.pop() {
            if adopted_protocols.contains(&protocol_name) {
                continue;
            }
            adopted_protocols.push(protocol_name);
            let protocol_def = &self.index.protocols[protocol_name];
            Extend::<&str>::extend(
                &mut unprocessed_adopted_protocols,
                protocol_def
                    .inherited_protocols
                    .iter()
                    .map(|name| name.as_ref()),
            );
        }
        for protoc in adopted_protocols {
            let protoc_trait_path = self.protoc_rel_path(protoc, &current_mod);
            writeln!(
                &mut code,
                "impl {protoc_trait_path} for {struct_name} {{}}",
                protoc_trait_path = protoc_trait_path,
                struct_name = struct_name,
            )
            .unwrap();
        }

        Some(code)
    }

    fn generate_enum(&self, def: &ast::EnumDef) -> Option<String> {
        use ast::TagId;

        // TODO: We should only generate code for enums that are used in the generated code.

        let enum_name = match def.id {
            TagId::Named(ref name) => name,
            TagId::Unnamed(_) => return None,
        };

        // Enums with names starting with an underscore are probably private.
        if enum_name.starts_with('_') {
            return None;
        }

        let value_names =
            cleanup_enum_value_names(enum_name, def.values.iter().map(|value| &value.name));

        let underlying = rust_type_name_for_enum_underlying(&def.underlying, &self.index);
        let mut code = String::new();

        write!(
            &mut code,
            "\
#[repr(transparent)]
#[derive(Copy, Clone, PartialEq, Eq)]
pub struct {struct_name}({underlying});

impl {struct_name} {{
",
            struct_name = enum_name,
            underlying = underlying,
        )
        .unwrap();

        for (cleaned_up_name, value) in value_names.iter().zip(def.values.iter()) {
            let original_value_name = &value.name;
            write!(
                &mut code,
                "    /// {original_value_name}
    const {cleaned_up_name}: Self = {struct_name}({num_val});
",
                original_value_name = original_value_name,
                cleaned_up_name = cleaned_up_name,
                struct_name = enum_name,
                num_val = value.value
            )
            .unwrap();
        }

        write!(
            &mut code,
            "\
}}
impl std::fmt::Debug for {struct_name} {{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {{
        match *self {{
",
            struct_name = enum_name,
        )
        .unwrap();

        for cleaned_up_name in value_names.iter() {
            writeln!(
                &mut code,
                "            {struct_name}::{cleaned_up_name} => f.write_str({cleaned_up_name:?}),",
                struct_name = enum_name,
                cleaned_up_name = cleaned_up_name,
            )
            .unwrap();
        }

        write!(
            &mut code,
            "            _ => write!(f, \"{struct_name}({{}})\", self.0),
        }}
    }}
}}
",
            struct_name = enum_name,
        )
        .unwrap();

        Some(code)
    }

    fn generate_core_base_code(&mut self) {
        use std::io::Write;

        let core = self.output_handler.mod_file(CORE_MOD);

        writeln!(
            &*core,
            r#"
#[repr(C)]
pub struct Object {{
    _private: [u8; 0],
}}

// ARC runtime support - https://clang.llvm.org/docs/AutomaticReferenceCounting.html#runtime-support
#[link(name = "objc", kind = "dylib")]
extern "C" {{
    fn objc_autoreleasePoolPush() -> *const std::ffi::c_void;
    fn objc_autoreleasePoolPop(pool: *const std::ffi::c_void);
    fn objc_release(value: *mut Object);
    fn objc_retain(value: *mut Object) -> *mut Object;
}}

pub trait {base_objc_trait}: Sized {{
    unsafe fn from_raw_unchecked(ptr: std::ptr::NonNull<Object>) -> Self;
    fn as_raw(&self) -> std::ptr::NonNull<Object>;
}}
"#,
            base_objc_trait = BASE_OBJC_TRAIT,
        )
        .unwrap();
    }

    pub(crate) fn generate(&mut self) {
        use ast::Decl;
        use std::io::Write;

        self.generate_core_base_code();

        for decl in &self.decls {
            let gen = match decl {
                Decl::EnumDef(def) => self.generate_enum(&def).map(|code| (&def.origin, code)),
                Decl::ProtocolDef(def) => {
                    self.generate_protocol(&def).map(|code| (&def.origin, code))
                }
                Decl::InterfaceDef(def) => {
                    self.generate_class(&def).map(|code| (&def.origin, code))
                }

                _ => None,
            };
            if let Some((origin, code)) = gen {
                let origin = origin.as_ref().unwrap();
                let mut file = self.output_handler.file_for(origin);
                write!(file, "{}\n\n", code).unwrap();
            }
        }

        for name in self.output_handler.files.keys() {
            writeln!(&self.output_handler.main_file, "mod {};", name).unwrap();
        }
    }
}

fn snake_case_split(text: &str) -> Vec<String> {
    use lazy_static::lazy_static;
    use regex::Regex;

    lazy_static! {
        static ref LOWER_TO_UPPER_RE: Regex = Regex::new(r"([a-z])([A-Z])").unwrap();
        static ref UPPER_TO_LOWER_RE: Regex = Regex::new(r"([A-Z])([A-Z][a-z])").unwrap();
    }

    let text = &LOWER_TO_UPPER_RE.replace_all(text, "${1}_${2}");
    let text = UPPER_TO_LOWER_RE.replace_all(text, "${1}_${2}");
    text.split('_').map(|s| s.to_string()).collect()
}

fn loosely_equal(text1: &str, text2: &str) -> bool {
    text1.trim_end_matches('s') == text2.trim_end_matches('s')
}

fn cleanup_enum_value_names<S: AsRef<str>, Iter: Iterator<Item = S>>(
    enum_name: &str,
    value_names: Iter,
) -> Vec<String> {
    let enum_name_split = snake_case_split(enum_name);
    let value_names_split = value_names
        .map(|name| {
            let mut split_name = snake_case_split(name.as_ref());
            // Ignore "k" at the start of a value name.
            if split_name[0] == "k" {
                split_name.remove(0);
            }
            split_name
        })
        .collect::<Vec<_>>();

    let matching_head_len = value_names_split
        .iter()
        .map(|value_name_split| {
            let matching_len = enum_name_split
                .iter()
                .enumerate()
                .find(|(i, enum_name_part)| {
                    *i >= value_name_split.len()
                        || !loosely_equal(enum_name_part, &value_name_split[*i])
                })
                .map_or(enum_name_split.len(), |(i, _)| i);
            if matching_len == value_name_split.len() {
                // If the whole value name matched, do not include the last word.
                matching_len - 1
            } else {
                matching_len
            }
        })
        .min()
        .unwrap();

    let matching_tail_len = value_names_split
        .iter()
        .map(|value_name_split| {
            let matching_len = enum_name_split
                .iter()
                .rev()
                .enumerate()
                .find(|(i, enum_name_part)| {
                    *i >= value_name_split.len()
                        || !loosely_equal(
                            enum_name_part,
                            &value_name_split[value_name_split.len() - 1 - *i],
                        )
                })
                .map_or(enum_name_split.len(), |(i, _)| i);
            if matching_len == value_name_split.len() {
                // If the whole value name matched, consider nothing matched.
                0
            } else {
                matching_len
            }
        })
        .min()
        .unwrap();

    value_names_split
        .iter()
        .map(|value_name_split| {
            value_name_split[matching_head_len..value_name_split.len() - matching_tail_len]
                .iter()
                .map(|word| word.to_ascii_uppercase())
                .collect::<Vec<String>>()
                .join("_")
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_enum_cleanup() {
        assert_eq!(
            cleanup_enum_value_names(
                "NSQualityOfService",
                [
                    "NSQualityOfServiceUserInteractive",
                    "NSQualityOfServiceUserInitiated",
                    "NSQualityOfServiceUtility",
                    "NSQualityOfServiceBackground",
                    "NSQualityOfServiceDefault",
                ]
                .into_iter()
            ),
            [
                "USER_INTERACTIVE",
                "USER_INITIATED",
                "UTILITY",
                "BACKGROUND",
                "DEFAULT",
            ],
        );

        assert_eq!(
            cleanup_enum_value_names(
                "CFSocketError",
                ["kCFSocketSuccess", "kCFSocketError", "kCFSocketTimeout"].into_iter()
            ),
            ["SUCCESS", "ERROR", "TIMEOUT"],
        );

        assert_eq!(
            cleanup_enum_value_names(
                "CGWindowLevelKey",
                [
                    "kCGBaseWindowLevelKey",
                    "kCGMinimumWindowLevelKey",
                    "kCGDesktopWindowLevelKey",
                    "kCGBackstopMenuLevelKey",
                    "kCGNormalWindowLevelKey",
                    "kCGFloatingWindowLevelKey",
                    "kCGTornOffMenuWindowLevelKey",
                    "kCGDockWindowLevelKey",
                ]
                .into_iter()
            ),
            [
                "BASE_WINDOW",
                "MINIMUM_WINDOW",
                "DESKTOP_WINDOW",
                "BACKSTOP_MENU",
                "NORMAL_WINDOW",
                "FLOATING_WINDOW",
                "TORN_OFF_MENU_WINDOW",
                "DOCK_WINDOW",
            ],
        );

        assert_eq!(
            cleanup_enum_value_names(
                "CMSCertificateChainMode",
                [
                    "kCMSCertificateNone",
                    "kCMSCertificateSignerOnly",
                    "kCMSCertificateChain",
                    "kCMSCertificateChainWithRoot",
                    "kCMSCertificateChainWithRootOrFail",
                ]
                .into_iter()
            ),
            [
                "NONE",
                "SIGNER_ONLY",
                "CHAIN",
                "CHAIN_WITH_ROOT",
                "CHAIN_WITH_ROOT_OR_FAIL",
            ],
        );

        assert_eq!(
            cleanup_enum_value_names(
                "NSDateComponentsFormatterZeroFormattingBehavior",
                [
                    "NSDateComponentsFormatterZeroFormattingBehaviorNone",
                    "NSDateComponentsFormatterZeroFormattingBehaviorDefault",
                    "NSDateComponentsFormatterZeroFormattingBehaviorDropLeading",
                    "NSDateComponentsFormatterZeroFormattingBehaviorDropMiddle",
                    "NSDateComponentsFormatterZeroFormattingBehaviorDropTrailing",
                    "NSDateComponentsFormatterZeroFormattingBehaviorDropAll",
                    "NSDateComponentsFormatterZeroFormattingBehaviorPad",
                ]
                .into_iter()
            ),
            [
                "NONE",
                "DEFAULT",
                "DROP_LEADING",
                "DROP_MIDDLE",
                "DROP_TRAILING",
                "DROP_ALL",
                "PAD",
            ],
        );

        assert_eq!(
            cleanup_enum_value_names(
                "CFStringEncodings",
                [
                    "kCFStringEncodingMacJapanese",
                    "kCFStringEncodingMacChineseTrad",
                    "kCFStringEncodingMacKorean",
                    "kCFStringEncodingMacArabic",
                    "kCFStringEncodingMacHebrew",
                    "kCFStringEncodingMacGreek",
                    "kCFStringEncodingMacCyrillic",
                ]
                .into_iter()
            ),
            [
                "MAC_JAPANESE",
                "MAC_CHINESE_TRAD",
                "MAC_KOREAN",
                "MAC_ARABIC",
                "MAC_HEBREW",
                "MAC_GREEK",
                "MAC_CYRILLIC",
            ],
        );
    }

    #[test]
    fn test_rust_type_name_for_fixed_int() {
        assert_eq!(rust_type_name_for_fixed_int("int8_t"), Some("i8"));
        assert_eq!(rust_type_name_for_fixed_int("uint16"), Some("u16"));
        assert_eq!(rust_type_name_for_fixed_int("uint16_t"), Some("u16"));
        assert_eq!(rust_type_name_for_fixed_int("SInt32"), Some("i32"));
        assert_eq!(rust_type_name_for_fixed_int("int32_t"), Some("i32"));
        assert_eq!(rust_type_name_for_fixed_int("UInt32"), Some("u32"));
        assert_eq!(rust_type_name_for_fixed_int("uint32"), Some("u32"));
        assert_eq!(rust_type_name_for_fixed_int("uint32_t"), Some("u32"));
        assert_eq!(rust_type_name_for_fixed_int("uint64_t"), Some("u64"));
    }
}
