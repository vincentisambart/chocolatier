use crate::ast;
use std::borrow::Cow;
use std::collections::HashMap;
use std::fmt::Write;

const BASE_OBJC_TRAIT: &str = "ObjCPtr";
const UNTYPED_OBJC_PTR: &str = "UntypedObjCPtr";
const CORE_MOD: &str = "core";
const BASE_OBJC_TRAIT_FULL_PATH: &str = "crate::core::ObjCPtr";
const UNTYPED_OBJC_PTR_FULL_PATH: &str = "crate::core::UntypedObjCPtr";

trait OriginExt {
    fn mod_name(&self) -> Cow<'_, str>;
}

impl OriginExt for ast::Origin {
    fn mod_name(&self) -> Cow<'_, str> {
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
    fn from(items: &[ast::AttributedItem]) -> Self {
        use ast::{Item, RecordKind};

        let mut enums = HashMap::new();
        let mut structs = HashMap::new();
        let mut unions = HashMap::new();
        let mut typedefs = HashMap::new();
        let mut protocols = HashMap::new();
        let mut interfaces = HashMap::new();
        let mut categories = HashMap::new();
        let mut functions = HashMap::new();
        let mut vars = HashMap::new();

        for item in items {
            match &item.item {
                Item::ProtocolDef(def) => {
                    protocols.insert(def.name.clone(), def.clone());
                }
                Item::InterfaceDef(def) => {
                    interfaces.insert(def.name.clone(), def.clone());
                }
                Item::CategoryDef(def) => {
                    let vec = categories.entry(def.class.clone()).or_insert_with(Vec::new);
                    vec.push(def.clone());
                }
                Item::RecordDef(def) => match def.kind {
                    RecordKind::Union => {
                        unions.insert(def.id.clone(), def.clone());
                    }
                    RecordKind::Struct => {
                        structs.insert(def.id.clone(), def.clone());
                    }
                },
                Item::EnumDef(def) => {
                    enums.insert(def.id.clone(), def.clone());
                }
                Item::TypedefDecl(decl) => {
                    typedefs.insert(decl.name.clone(), decl.clone());
                }
                Item::FuncDecl(decl) => {
                    functions.insert(decl.name.clone(), decl.clone());
                }
                Item::VarDecl(decl) => {
                    vars.insert(decl.name.clone(), decl.clone());
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

    fn protoc_mod(&self, protocol: &str) -> Cow<'_, str> {
        self.protocols[protocol].origin.as_ref().unwrap().mod_name()
    }

    fn interf_mod(&self, protocol: &str) -> Cow<'_, str> {
        self.interfaces[protocol]
            .origin
            .as_ref()
            .unwrap()
            .mod_name()
    }
}

fn rust_type_name_for_fixed_int(name: &str) -> Option<&'static str> {
    use once_cell::sync::Lazy;
    use regex::Regex;

    static FIXED_SIZE_INT_RE: Lazy<Regex> =
        Lazy::new(|| Regex::new(r"(?i)\A([us]?)int([0-9]+)").unwrap());
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
fn rust_type_name_for_enum_underlying(underlying: &ast::Type, index: &TypeIndex) -> &'static str {
    use ast::{NumKind, Type};

    match underlying {
        Type::Typedef(typedef) => match rust_type_name_for_fixed_int(&typedef.name) {
            Some(name) => name,
            None => {
                let objc_type = &index.typedefs[&typedef.name].underlying;
                rust_type_name_for_enum_underlying(&objc_type.ty, index)
            }
        },
        // There are platforms the following is not true (long is 32-bit on Windows),
        // but that should be true on all Apple platforms.
        Type::Num(kind) => match kind {
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
        let src_dir = generated_dir.join("src");
        let files = HashMap::new();

        DirBuilder::new().recursive(true).create(&src_dir).unwrap();
        let main_file = File::create(src_dir.join("lib.rs")).unwrap();
        Self::write_start_comment(&main_file);

        use std::io::Write;
        writeln!(&main_file, "#![allow(dead_code)]").unwrap();
        writeln!(
            &main_file,
            "#![allow(clippy::useless_let_if_seq,clippy::cognitive_complexity)]"
        )
        .unwrap();
        writeln!(&main_file, "#![warn(rust_2018_idioms)]").unwrap();

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
    items: Vec<ast::AttributedItem>,
    output_handler: OutputHandler,
}

fn protocol_trait_name(protocol_name: &str) -> String {
    format!("{}Protocol", protocol_name)
}

fn interface_trait_name(class_name: &str) -> String {
    format!("{}Interface", class_name)
}

fn stable_group_by<T, K: std::hash::Hash + Eq, F: Fn(&T) -> K>(vec: &[T], f: F) -> Vec<Vec<&T>> {
    let mut indices: HashMap<K, usize> = HashMap::new();
    let mut groups: Vec<Vec<&T>> = Vec::new();

    for item in vec {
        let key = f(item);
        if let Some(i) = indices.get(&key) {
            groups[*i].push(item);
        } else {
            let i = groups.len();
            groups.push(vec![item]);
            indices.insert(key, i);
        }
    }

    groups
}

impl Generator {
    pub(crate) fn new(items: Vec<ast::AttributedItem>) -> Self {
        let index = TypeIndex::from(&items);
        let output_handler = OutputHandler::new();
        Generator {
            index,
            items,
            output_handler,
        }
    }

    fn protoc_rel_path(&self, name: &str, current_mod: &str) -> Cow<'_, str> {
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

    fn interf_rel_path(&self, name: &str, current_mod: &str) -> Cow<'_, str> {
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
        let untyped_objc_ptr = if current_mod == CORE_MOD {
            UNTYPED_OBJC_PTR
        } else {
            UNTYPED_OBJC_PTR_FULL_PATH
        };
        let object = if current_mod == CORE_MOD {
            "Object"
        } else {
            "crate::core::Object"
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
    ptr: {untyped_objc_ptr},
}}

impl {base_objc_trait} for {struct_name} {{
    unsafe fn from_raw_unchecked(raw: std::ptr::NonNull<{object}>) -> Self {{
        let ptr = {untyped_objc_ptr}::from_raw_unchecked(raw);
        Self {{ ptr }}
    }}

    fn as_raw(&self) -> std::ptr::NonNull<{object}> {{
        self.ptr.as_raw()
    }}
}}
",
            object = object,
            struct_name = struct_name,
            trait_name = trait_name,
            inherited_traits = inherited_traits.join(" + "),
            base_objc_trait = base_objc_trait,
            untyped_objc_ptr = untyped_objc_ptr,
        )
        .unwrap();

        let mut unprocessed_adopted_protocols: Vec<&str> = Vec::new();
        Extend::<&str>::extend(
            &mut unprocessed_adopted_protocols,
            def.adopted_protocols.iter().map(|name| name.as_str()),
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
                        .map(|name| name.as_str()),
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
                    .map(|name| name.as_str()),
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

    fn generate_enum(&self, def: &ast::EnumDef, attrs: &[ast::Attr]) -> Option<String> {
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

        let is_flag_enum = attrs.contains(&ast::Attr::FlagEnum);

        // Clean-up deprecated and non deprecated names separately, as they can use different naming schemes.
        let mut cleaned_up_names: HashMap<&str, &str> = HashMap::new();
        let (deprecated_values, non_deprecated_values): (Vec<_>, Vec<_>) = def
            .values
            .iter()
            .partition(|v| v.attrs.contains(&ast::Attr::Deprecated));
        let deprecated_names =
            cleanup_enum_value_names(enum_name, deprecated_values.iter().map(|v| &v.name));
        for (name, value) in deprecated_names.iter().zip(deprecated_values.iter()) {
            cleaned_up_names.insert(&value.name, name);
        }
        let non_deprecated_names =
            cleanup_enum_value_names(enum_name, non_deprecated_values.iter().map(|v| &v.name));
        for (name, value) in non_deprecated_names
            .iter()
            .zip(non_deprecated_values.iter())
        {
            cleaned_up_names.insert(&value.name, name);
        }
        let cleaned_up_names = cleaned_up_names;

        let underlying = rust_type_name_for_enum_underlying(&def.underlying.ty, &self.index);
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

        let mut values_per_num_val: HashMap<_, Vec<_>> = HashMap::new();
        for value in def.values.iter() {
            values_per_num_val
                .entry(value.value)
                .or_default()
                .push(value);
        }

        // We have to group by cleaned-up names, as multiple values can have the same name.
        // This can happen if the only difference in the original name was case,
        // or when cleaning of some deprecated and non deprecated names ended up with the same name.
        let grouped_values = stable_group_by(&def.values, |v| {
            cleaned_up_names.get(v.name.as_str()).unwrap()
        });
        for group in grouped_values {
            assert!(group[1..].iter().all(|v| v.value == group[0].value));
            // Taking the first non deprecated one, or the first one if they are all deprecated.
            let value = if let Some(value) = group
                .iter()
                .find(|v| !v.attrs.contains(&ast::Attr::Deprecated))
            {
                value
            } else {
                group[0]
            };
            let original_value_name = value.name.as_str();
            let cleaned_up_name = cleaned_up_names.get(original_value_name).unwrap();

            writeln!(
                &mut code,
                "    /// {original_value_name}",
                original_value_name = original_value_name,
            )
            .unwrap();
            if value.attrs.contains(&ast::Attr::Deprecated) {
                writeln!(&mut code, "    #[deprecated]").unwrap();
            }
            writeln!(
                &mut code,
                "    const {cleaned_up_name}: Self = Self({num_val});",
                cleaned_up_name = cleaned_up_name,
                num_val = value.value
            )
            .unwrap();
        }

        writeln!(&mut code, "}}").unwrap();

        let mut values_for_fmt = Vec::new();
        for num_value in values_per_num_val.keys() {
            let values = values_per_num_val.get(&num_value).unwrap();

            if values.len() > 1 {
                // Multiple enum value have the same numeric value.
                // It only makes sense to have one in the match statement so we have to choose.

                fn has_some_platform_deprecation(enum_value: &ast::EnumValue) -> bool {
                    for attr in enum_value.attrs.iter() {
                        if let ast::Attr::PlatformAvailability(availability) = attr {
                            if availability.iter().any(|avail| avail.deprecated.is_some()) {
                                // The deprecation version can even be in the future (100000) but we don't have any way to check if there is a replacement available.
                                return true;
                            }
                        }
                    }
                    false
                }

                let first_non_deprecated = values.iter().find(|v| {
                    !v.attrs.contains(&ast::Attr::Deprecated) && !has_some_platform_deprecation(v)
                });

                if let Some(non_deprecated) = first_non_deprecated {
                    values_for_fmt.push(*non_deprecated);
                } else {
                    // No value non deprecated so put the first we have.
                    values_for_fmt.push(values[0]);
                }
            } else {
                values_for_fmt.push(values[0]);
            }
        }

        if is_flag_enum {
            write!(
                &mut code,
                "\
impl {struct_name} {{
    fn is_empty(self) -> bool {{
        self.0 == 0
    }}
}}
impl std::ops::BitOr for {struct_name} {{
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {{
        Self(self.0 | rhs.0)
    }}
}}
impl std::ops::BitOrAssign for {struct_name} {{
    fn bitor_assign(&mut self, rhs: Self) {{
        *self = Self(self.0 | rhs.0)
    }}
}}
impl std::ops::BitAnd for {struct_name} {{
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {{
        Self(self.0 & rhs.0)
    }}
}}
impl std::ops::BitAndAssign for {struct_name} {{
    fn bitand_assign(&mut self, rhs: Self) {{
        *self = Self(self.0 & rhs.0)
    }}
}}
impl std::ops::Not for {struct_name} {{
    type Output = Self;

    fn not(self) -> Self::Output {{
        Self(!self.0)
    }}
}}
#[allow(deprecated)]
impl std::fmt::Debug for {struct_name} {{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {{
",
                struct_name = enum_name,
            )
            .unwrap();

            let empty_name =
                if let Some(zero_val) = values_for_fmt.iter().find(|val| val.value.is_zero()) {
                    cleaned_up_names.get(zero_val.name.as_str()).unwrap()
                } else {
                    "(empty)"
                };
            write!(
                &mut code,
                "        if self.is_empty() {{
            return f.write_str({empty_name:?});
        }}
",
                empty_name = empty_name
            )
            .unwrap();

            // Remove values that include other values (for example DEFAULT that is the same as OPTION1 | OPTION2) and the empty value.
            let values_for_fmt: Vec<_> = values_for_fmt
                .iter()
                .filter(|outer_val| {
                    !outer_val.value.is_zero()
                        && !values_for_fmt.iter().any(|inner_val| {
                            if inner_val.value == outer_val.value || inner_val.value.is_zero() {
                                return false;
                            }
                            match (inner_val.value, outer_val.value) {
                                (
                                    ast::SignedOrNotInt::Signed(inner_num),
                                    ast::SignedOrNotInt::Signed(outer_num),
                                ) => ((inner_num & outer_num) == inner_num),
                                (
                                    ast::SignedOrNotInt::Unsigned(inner_num),
                                    ast::SignedOrNotInt::Unsigned(outer_num),
                                ) => ((inner_num & outer_num) == inner_num),
                                _ => panic!("both sides should have the same type"),
                            }
                        })
                })
                .collect();

            if values_for_fmt.is_empty() {
                writeln!(
                    &mut code,
                    "        write!(f, \"{struct_name}({{}})\", self.0)?;",
                    struct_name = enum_name,
                )
                .unwrap()
            } else {
                writeln!(
                    &mut code,
                    "        let mut left = *self;
        let mut is_first = true;"
                )
                .unwrap();

                let mut is_first = true;
                // Iterating from def.values and not from values_for_fmt to keep the order they were originally defined in.
                for value in def.values.iter() {
                    if !values_for_fmt.contains(&&value) {
                        continue;
                    }

                    let original_value_name = value.name.as_str();
                    let cleaned_up_name = cleaned_up_names.get(original_value_name).unwrap();

                    if is_first {
                        write!(
                            &mut code,
                            "        if !(left & Self::{cleaned_up_name}).is_empty() {{
            f.write_str({cleaned_up_name:?})?;
            left &= !Self::{cleaned_up_name};
            is_first = false;
        }}
",
                            cleaned_up_name = cleaned_up_name
                        )
                        .unwrap();
                        is_first = false;
                    } else {
                        write!(
                            &mut code,
                            "        if !(left & Self::{cleaned_up_name}).is_empty() {{
            if !is_first {{
                f.write_str(\" | \")?;
            }}
            f.write_str({cleaned_up_name:?})?;
            left &= !Self::{cleaned_up_name};
            is_first = false;
        }}
",
                            cleaned_up_name = cleaned_up_name
                        )
                        .unwrap();
                    }
                }

                write!(
                    &mut code,
                    "        if !left.is_empty() {{
            if !is_first {{
                f.write_str(\" | \")?;
            }}
            write!(f, \"{struct_name}({{}})\", left.0)?;
        }}
",
                    struct_name = enum_name,
                )
                .unwrap()
            }

            write!(
                &mut code,
                "        Ok(())
    }}
}}
"
            )
            .unwrap();
        } else {
            write!(
                &mut code,
                "\
#[allow(deprecated)]
impl std::fmt::Debug for {struct_name} {{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {{
        match *self {{
",
                struct_name = enum_name,
            )
            .unwrap();

            // Iterating from def.values and not from values_for_fmt to keep the order they were originally defined in.
            for value in def.values.iter() {
                if !values_for_fmt.contains(&&value) {
                    continue;
                }
                let original_value_name = value.name.as_str();
                let cleaned_up_name = cleaned_up_names.get(original_value_name).unwrap();

                writeln!(
                    &mut code,
                    "            Self::{cleaned_up_name} => f.write_str({cleaned_up_name:?}),",
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
        }

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

pub struct {untyped_objc_ptr} {{
    raw: std::ptr::NonNull<Object>,
}}

impl {base_objc_trait} for {untyped_objc_ptr} {{
    unsafe fn from_raw_unchecked(raw: std::ptr::NonNull<Object>) -> Self {{
        Self {{ raw }}
    }}

    fn as_raw(&self) -> std::ptr::NonNull<Object> {{
        self.raw
    }}
}}

impl Drop for {untyped_objc_ptr} {{
    fn drop(&mut self) {{
        unsafe {{
            objc_release(self.as_raw().as_ptr());
        }}
    }}
}}
"#,
            base_objc_trait = BASE_OBJC_TRAIT,
            untyped_objc_ptr = UNTYPED_OBJC_PTR,
        )
        .unwrap();
    }

    pub(crate) fn generate(&mut self) {
        use ast::Item;
        use std::io::Write;

        self.generate_core_base_code();

        for item in &self.items {
            let gen = match &item.item {
                Item::EnumDef(def) => self
                    .generate_enum(&def, &item.attrs)
                    .map(|code| (&def.origin, code)),
                Item::ProtocolDef(def) => {
                    self.generate_protocol(&def).map(|code| (&def.origin, code))
                }
                Item::InterfaceDef(def) => {
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
    use once_cell::sync::Lazy;
    use regex::Regex;

    static LOWER_TO_UPPER_RE: Lazy<Regex> = Lazy::new(|| Regex::new(r"([a-z])([A-Z])").unwrap());
    static UPPER_TO_LOWER_RE: Lazy<Regex> =
        Lazy::new(|| Regex::new(r"([A-Z])([A-Z][a-z])").unwrap());
    // "v" not included because we do not want "v1" to be split in ["v", "1"]
    static BEFORE_NUMBER_RE: Lazy<Regex> = Lazy::new(|| Regex::new(r"([a-uw-z])([0-9])").unwrap());
    static AFTER_NUMBER_RE: Lazy<Regex> = Lazy::new(|| Regex::new(r"([0-9])([A-Z][a-z])").unwrap());
    static OS_RE: Lazy<Regex> =
        Lazy::new(|| Regex::new(r"((?:\A|_)(?:mac|tv|i))_(OS(?:_|\z))").unwrap());

    // We want "NSTextScalingiOS" to be split as ["NS", "Text", "Scaling", "iOS"]
    let text = &text.replace("iOS", "_iOS_");

    let text = &LOWER_TO_UPPER_RE.replace_all(text, "${1}_${2}");
    let text = &UPPER_TO_LOWER_RE.replace_all(text, "${1}_${2}");
    let text = &LOWER_TO_UPPER_RE.replace_all(text, "${1}_${2}");
    let text = &BEFORE_NUMBER_RE.replace_all(text, "${1}_${2}");
    let text = &AFTER_NUMBER_RE.replace_all(text, "${1}_${2}");
    // We want "iOS" or "macOS" to not be split in 2.
    let text = OS_RE.replace_all(text, "${1}${2}");

    text.split('_')
        .filter_map(|s| {
            if s.is_empty() {
                None
            } else {
                Some(s.to_string())
            }
        })
        .collect()
}

fn strip_prefix<'a>(text: &'a str, prefix: &str) -> &'a str {
    if text.starts_with(prefix) {
        &text[prefix.len()..]
    } else {
        text
    }
}

fn strip_suffix<'a>(text: &'a str, suffix: &str) -> &'a str {
    if text.ends_with(suffix) {
        let end_pos = text.len() - suffix.len();
        &text[..end_pos]
    } else {
        text
    }
}

fn loosely_equal(text1: &str, text2: &str) -> bool {
    strip_suffix(text1, "s") == strip_suffix(text2, "s")
}

// The prefixes list does not need to be exhaustive as in most cases cleanup_enum_value_names will do its job without it.
static ENUM_COMMON_PREFIXES: [&str; 2] = ["NS", "AU"];

fn cleanup_enum_value_names<S: AsRef<str>, Iter: Iterator<Item = S>>(
    enum_name: &str,
    value_names: Iter,
) -> Vec<String> {
    let value_names_split = value_names
        .map(|name| {
            let name = name.as_ref();
            let mut name = strip_prefix(name, "k");
            // removing the prefix is mainly useful when we do not know how to split properly, for example "NSUUIDAttributeType".
            for prefix in &ENUM_COMMON_PREFIXES {
                if name.starts_with(prefix) {
                    name = &name[prefix.len()..];
                    break;
                }
            }
            snake_case_split(name)
        })
        .collect::<Vec<_>>();

    if value_names_split.is_empty() {
        return Vec::new();
    }

    let mut cleaned_enum_name = enum_name;
    for prefix in &ENUM_COMMON_PREFIXES {
        if cleaned_enum_name.starts_with(prefix) {
            cleaned_enum_name = &enum_name[prefix.len()..];
            break;
        }
    }

    let enum_name_split = snake_case_split(cleaned_enum_name);

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

    let names = value_names_split
        .iter()
        .map(|value_name_split| {
            value_name_split[matching_head_len..value_name_split.len() - matching_tail_len]
                .iter()
                .map(|word| word.to_ascii_uppercase())
                .collect::<Vec<_>>()
                .join("_")
        })
        .collect();

    additional_manual_cleanup(enum_name, names)
}

fn additional_manual_cleanup(enum_name: &str, mut value_names: Vec<String>) -> Vec<String> {
    fn starts_with_digit(text: &str) -> bool {
        text.chars().next().unwrap().is_ascii_digit()
    }

    fn add_prefix_before_starting_digit(value_names: &mut Vec<String>, prefix: &str) {
        for name in value_names
            .iter_mut()
            .filter(|name| starts_with_digit(name))
        {
            name.insert_str(0, prefix);
        }
    }

    match enum_name {
        "ColorSyncDataDepth" => {
            for name in value_names
                .iter_mut()
                .filter(|name| starts_with_digit(name))
            {
                let mut parts: Vec<_> = name.split("_BIT_").collect();
                assert!(parts.len() == 2);
                parts.swap(0, 1);
                *name = parts.join("_");
                name.push_str("BIT");
            }
        }
        "NSDataBase64EncodingOptions" => {
            for name in value_names
                .iter_mut()
                .filter(|name| starts_with_digit(name))
            {
                let mut parts: Vec<_> = name.split("_CHARACTER_").collect();
                assert!(parts.len() == 2);
                parts.swap(0, 1);
                *name = parts.join("_");
                name.push_str("_CHARACTERS");
            }
        }
        "CGImageByteOrderInfo" => {
            for name in value_names
                .iter_mut()
                .filter(|name| starts_with_digit(name))
            {
                let mut parts: Vec<_> = name.splitn(2, '_').collect();
                assert!(parts.len() == 2);
                parts.swap(0, 1);
                *name = parts.join("_");
            }
        }
        "NSNumberFormatterBehavior" | "NSDateFormatterBehavior" => {
            // 10_0 or 10_4 mean Mac OS X 10.0 and 10.4
            add_prefix_before_starting_digit(&mut value_names, "OSX_V");
        }
        "SMPTETimeType" | "CVSMPTETimeType" => {
            add_prefix_before_starting_digit(&mut value_names, "FPS_");
        }
        "IOSurfaceSubsampling" => {
            add_prefix_before_starting_digit(&mut value_names, "CHROMA_");
        }
        "CIDataMatrixCodeECCVersion" | "MTLLanguageVersion" => {
            add_prefix_before_starting_digit(&mut value_names, "V");
        }
        "MTLReadWriteTextureTier" | "MTLArgumentBuffersTier" => {
            add_prefix_before_starting_digit(&mut value_names, "TIER_");
        }
        "MTLTextureType" => {
            // Has names like "1D", "2D_ARRAY", "CUBE".
            // I could not find any good idea, so for the time being,
            // add a "TEXTURE_" prefix to all names (including those without digits)
            for name in value_names.iter_mut() {
                name.insert_str(0, "TEXTURE_");
            }
        }
        _ => {}
    }

    assert!(
        value_names.iter().all(|name| !starts_with_digit(name)),
        "value names should not start with a digit in enum {}: {:?}",
        enum_name,
        value_names,
    );

    value_names
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
                .iter()
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
                ["kCFSocketSuccess", "kCFSocketError", "kCFSocketTimeout"].iter()
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
                .iter()
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
                .iter()
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
                .iter()
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
                .iter()
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

        assert_eq!(
            cleanup_enum_value_names(
                "NSDataBase64EncodingOptions",
                [
                    "NSDataBase64Encoding64CharacterLineLength",
                    "NSDataBase64Encoding76CharacterLineLength",
                    "NSDataBase64EncodingEndLineWithCarriageReturn",
                    "NSDataBase64EncodingEndLineWithLineFeed",
                ]
                .iter()
            ),
            [
                "LINE_LENGTH_64_CHARACTERS",
                "LINE_LENGTH_76_CHARACTERS",
                "END_LINE_WITH_CARRIAGE_RETURN",
                "END_LINE_WITH_LINE_FEED",
            ],
        );

        assert_eq!(
            cleanup_enum_value_names(
                "ColorSyncDataDepth",
                [
                    "kColorSync1BitGamut",
                    "kColorSync8BitInteger",
                    "kColorSync16BitInteger",
                    "kColorSync16BitFloat",
                    "kColorSync32BitInteger",
                    "kColorSync32BitNamedColorIndex",
                    "kColorSync32BitFloat",
                    "kColorSync10BitInteger",
                ]
                .iter()
            ),
            [
                "GAMUT_1BIT",
                "INTEGER_8BIT",
                "INTEGER_16BIT",
                "FLOAT_16BIT",
                "INTEGER_32BIT",
                "NAMED_COLOR_INDEX_32BIT",
                "FLOAT_32BIT",
                "INTEGER_10BIT",
            ],
        );

        assert_eq!(
            cleanup_enum_value_names(
                "AU3DMixerRenderingFlags",
                [
                    "k3DMixerRenderingFlags_InterAuralDelay",
                    "k3DMixerRenderingFlags_DopplerShift",
                    "k3DMixerRenderingFlags_DistanceAttenuation",
                    "k3DMixerRenderingFlags_DistanceFilter",
                    "k3DMixerRenderingFlags_DistanceDiffusion",
                    "k3DMixerRenderingFlags_LinearDistanceAttenuation",
                    "k3DMixerRenderingFlags_ConstantReverbBlend",
                ]
                .iter()
            ),
            [
                "INTER_AURAL_DELAY",
                "DOPPLER_SHIFT",
                "DISTANCE_ATTENUATION",
                "DISTANCE_FILTER",
                "DISTANCE_DIFFUSION",
                "LINEAR_DISTANCE_ATTENUATION",
                "CONSTANT_REVERB_BLEND",
            ],
        );

        assert_eq!(
            cleanup_enum_value_names(
                "MTLFeatureSet",
                [
                    "MTLFeatureSet_iOS_GPUFamily1_v1",
                    "MTLFeatureSet_iOS_GPUFamily2_v1",
                    "MTLFeatureSet_iOS_GPUFamily1_v2",
                    "MTLFeatureSet_iOS_GPUFamily2_v2",
                    "MTLFeatureSet_iOS_GPUFamily3_v1",
                    "MTLFeatureSet_iOS_GPUFamily1_v3",
                    "MTLFeatureSet_iOS_GPUFamily2_v3",
                    "MTLFeatureSet_iOS_GPUFamily3_v2",
                    "MTLFeatureSet_iOS_GPUFamily1_v4",
                    "MTLFeatureSet_iOS_GPUFamily2_v4",
                    "MTLFeatureSet_iOS_GPUFamily3_v3",
                    "MTLFeatureSet_iOS_GPUFamily4_v1",
                    "MTLFeatureSet_iOS_GPUFamily1_v5",
                    "MTLFeatureSet_iOS_GPUFamily2_v5",
                    "MTLFeatureSet_iOS_GPUFamily3_v4",
                    "MTLFeatureSet_iOS_GPUFamily4_v2",
                    "MTLFeatureSet_macOS_GPUFamily1_v1",
                    "MTLFeatureSet_OSX_GPUFamily1_v1",
                    "MTLFeatureSet_macOS_GPUFamily1_v2",
                    "MTLFeatureSet_OSX_GPUFamily1_v2",
                    "MTLFeatureSet_macOS_ReadWriteTextureTier2",
                    "MTLFeatureSet_OSX_ReadWriteTextureTier2",
                    "MTLFeatureSet_macOS_GPUFamily1_v3",
                    "MTLFeatureSet_macOS_GPUFamily1_v4",
                    "MTLFeatureSet_macOS_GPUFamily2_v1",
                    "MTLFeatureSet_tvOS_GPUFamily1_v1",
                    "MTLFeatureSet_TVOS_GPUFamily1_v1",
                    "MTLFeatureSet_tvOS_GPUFamily1_v2",
                    "MTLFeatureSet_tvOS_GPUFamily1_v3",
                    "MTLFeatureSet_tvOS_GPUFamily1_v4",
                ]
                .iter()
            ),
            [
                "IOS_GPU_FAMILY_1_V1",
                "IOS_GPU_FAMILY_2_V1",
                "IOS_GPU_FAMILY_1_V2",
                "IOS_GPU_FAMILY_2_V2",
                "IOS_GPU_FAMILY_3_V1",
                "IOS_GPU_FAMILY_1_V3",
                "IOS_GPU_FAMILY_2_V3",
                "IOS_GPU_FAMILY_3_V2",
                "IOS_GPU_FAMILY_1_V4",
                "IOS_GPU_FAMILY_2_V4",
                "IOS_GPU_FAMILY_3_V3",
                "IOS_GPU_FAMILY_4_V1",
                "IOS_GPU_FAMILY_1_V5",
                "IOS_GPU_FAMILY_2_V5",
                "IOS_GPU_FAMILY_3_V4",
                "IOS_GPU_FAMILY_4_V2",
                "MACOS_GPU_FAMILY_1_V1",
                "OSX_GPU_FAMILY_1_V1",
                "MACOS_GPU_FAMILY_1_V2",
                "OSX_GPU_FAMILY_1_V2",
                "MACOS_READ_WRITE_TEXTURE_TIER_2",
                "OSX_READ_WRITE_TEXTURE_TIER_2",
                "MACOS_GPU_FAMILY_1_V3",
                "MACOS_GPU_FAMILY_1_V4",
                "MACOS_GPU_FAMILY_2_V1",
                "TVOS_GPU_FAMILY_1_V1",
                "TVOS_GPU_FAMILY_1_V1",
                "TVOS_GPU_FAMILY_1_V2",
                "TVOS_GPU_FAMILY_1_V3",
                "TVOS_GPU_FAMILY_1_V4",
            ],
        );

        assert_eq!(
            cleanup_enum_value_names(
                "NSTextScalingType",
                ["NSTextScalingStandard", "NSTextScalingiOS"].iter()
            ),
            ["STANDARD", "IOS"],
        );

        assert_eq!(
            cleanup_enum_value_names(
                "NSAttributeType",
                [
                    "NSUndefinedAttributeType",
                    "NSInteger16AttributeType",
                    "NSInteger32AttributeType",
                    "NSInteger64AttributeType",
                    "NSDecimalAttributeType",
                    "NSDoubleAttributeType",
                    "NSFloatAttributeType",
                    "NSStringAttributeType",
                    "NSBooleanAttributeType",
                    "NSDateAttributeType",
                    "NSBinaryDataAttributeType",
                    "NSUUIDAttributeType",
                    "NSURIAttributeType",
                    "NSTransformableAttributeType",
                    "NSObjectIDAttributeType",
                ]
                .iter()
            ),
            [
                "UNDEFINED",
                "INTEGER_16",
                "INTEGER_32",
                "INTEGER_64",
                "DECIMAL",
                "DOUBLE",
                "FLOAT",
                "STRING",
                "BOOLEAN",
                "DATE",
                "BINARY_DATA",
                "UUID",
                "URI",
                "TRANSFORMABLE",
                "OBJECT_ID",
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
