use crate::ast;
use quote::{format_ident, quote};
use std::collections::HashMap;

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
}

impl quote::ToTokens for ast::SignedOrNotInt {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        use ast::SignedOrNotInt;
        use proc_macro2::Literal;
        use quote::TokenStreamExt;

        // Use unsuffixed values before SignedOrNotInt stores everything as 64-bit values even if the enum's underlying type is smaller.
        match self {
            SignedOrNotInt::Signed(i) => tokens.append(Literal::i64_unsuffixed(*i)),
            SignedOrNotInt::Unsigned(u) => tokens.append(Literal::u64_unsuffixed(*u)),
        }
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
        let main_file = File::create(src_dir.join("main.rs")).unwrap();

        OutputHandler {
            src_dir,
            files,
            main_file,
        }
    }

    fn mod_file(&mut self, module: &str) -> &std::fs::File {
        use std::fs::File;

        if !self.files.contains_key(module) {
            let file = File::create(self.src_dir.join(module).with_extension("rs")).unwrap();
            self.files.insert(module.to_string(), file);
        }
        &self.files[module]
    }

    fn file_for(&mut self, origin: &ast::Origin) -> &std::fs::File {
        use ast::Origin;

        match origin {
            Origin::ObjCCore | Origin::System => self.mod_file("core"),
            Origin::Framework(framework) => {
                let mod_name = snake_case_split(framework).join("_").to_ascii_lowercase();
                self.mod_file(&mod_name)
            }
            Origin::Library(lib) => self.mod_file(&lib.trim_start_matches("lib")),
        }
    }
}

pub(crate) struct Generator {
    index: TypeIndex,
    decls: Vec<ast::Decl>,
    output_handler: OutputHandler,
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

        let struct_ident = format_ident!("{}", enum_name);

        let value_names =
            cleanup_enum_value_names(enum_name, def.values.iter().map(|value| &value.name));

        let value_name_idents: Vec<_> = value_names
            .iter()
            .map(|value_name| format_ident!("{}", value_name))
            .collect();

        let original_value_names = def.values.iter().map(|value| &value.name);

        let values = def.values.iter().map(|value| value.value);

        let underlying = format_ident!(
            "{}",
            rust_type_name_for_enum_underlying(&def.underlying, &self.index)
        );
        let debug_default = format!("{}({{}})", enum_name);
        let code = quote! {
            #[repr(transparent)]
            #[derive(Copy, Clone, PartialEq, Eq)]
            struct #struct_ident(#underlying);

            impl #struct_ident {
                #(#[doc = #original_value_names] const #value_name_idents: Self = #struct_ident(#values);)*
            }

            impl std::fmt::Debug for #struct_ident {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    match *self {
                        #(#struct_ident::#value_name_idents => f.write_str(#value_names),)*
                        _ => write!(f, #debug_default, self.0),
                    }
                }
            }
        };

        Some(code.to_string())
    }

    pub(crate) fn generate(&mut self) {
        use ast::Decl;
        use std::io::Write;

        self.output_handler.mod_file("core");

        for decl in &self.decls {
            // let streams = Vec::new();
            let gen = match decl {
                Decl::EnumDef(def) => self.generate_enum(&def).map(|code| (&def.origin, code)),
                _ => None,
            };
            if let Some((origin, code)) = gen {
                let origin = origin.as_ref().unwrap();
                let mut file = self.output_handler.file_for(origin);
                write!(file, "{}\n\n", code).unwrap();
            }
        }

        for name in self.output_handler.files.keys() {
            write!(&self.output_handler.main_file, "mod {};\n", name).unwrap();
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
