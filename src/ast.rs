use bitflags::bitflags;
use clang::{Clang, EntityKind, TypeKind};
use std::collections::{HashMap, HashSet};

// TODO: Try to get:
// - class and method OS version annotations
// - annotations for Swift
// - consume/retained/not retained
// - enum_extensibility(open)
// - exception throwing info (maybe from annotations from Swift)
// - support parsing code for different platforms (iOS, macOS, ...)
// - alignment, packing, sizes (and make sure the size and offset of each item is the same for clang and Rust as bindgen does)
// - instancetype
// - namespacing of ObjC exported from Swift (though that might be fine as we're calling from generated ObjC)
// - const
// - bit fields
// - try getting the best definition of a function (unfortunately libclang's "canonical" seems to just be the first one)
// - struct or type declaration inside interface declaration
// - __attribute__((NSObject)), __attribute__/objc_bridge_mutable((objc_bridge*))
// - __attribute__((ns_consumes_self)) (implicit on all init)
// - __attribute__((cf_returns_*))
// - ignore enum values with a better named replacement (for example in NSCalendarUnit)

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum Origin {
    ObjCCore,
    System,
    Framework(String),
    Library(String),
}

impl Origin {
    fn from_path(path: &str) -> Option<Self> {
        use lazy_static::lazy_static;
        use regex::Regex;

        lazy_static! {
            static ref FRAMEWORK_PATH_RE: Regex =
                Regex::new(r"/([^./]+)\.framework/Headers/[^./]+.h\z").unwrap();
        }
        if let Some(caps) = FRAMEWORK_PATH_RE.captures(path) {
            let framework = caps.get(1).unwrap().as_str().to_owned();
            return Some(Origin::Framework(framework));
        }

        if path.contains("/usr/include/") {
            lazy_static! {
                static ref LIBRARY_PATH_RE: Regex =
                    Regex::new(r"/usr/include/([^./]+)/.+.h\z").unwrap();
            }
            if let Some(caps) = LIBRARY_PATH_RE.captures(path) {
                let library = caps.get(1).unwrap().as_str();
                if library == "objc" {
                    return Some(Origin::ObjCCore);
                } else if library == "sys" || library == "i386" {
                    return Some(Origin::System);
                } else {
                    return Some(Origin::Library(library.to_owned()));
                }
            } else {
                return Some(Origin::System);
            }
        }

        None
    }

    fn from_entity(entity: &clang::Entity) -> Option<Origin> {
        // As preprocess the file in advance using the system compiler, we have to use get_presumed_location().
        let (path, _, _) = entity.get_location()?.get_presumed_location();
        Self::from_path(&path)
    }
}

#[derive(Copy, Clone)]
enum AppleSdk {
    MacOs,
    IOs,
    IOsSimulator,
    TvOs,
    TvOsSimulator,
    WatchOs,
    WatchOsSimulator,
}

impl AppleSdk {
    fn sdk_name(&self) -> &str {
        match *self {
            AppleSdk::MacOs => "macosx",
            AppleSdk::IOs => "iphoneos",
            AppleSdk::IOsSimulator => "iphonesimulator",
            AppleSdk::TvOs => "appletvos",
            AppleSdk::TvOsSimulator => "appletvsimulator",
            AppleSdk::WatchOs => "watchos",
            AppleSdk::WatchOsSimulator => "watchsimulator",
        }
    }
}

fn sdk_path(sdk: AppleSdk) -> String {
    use std::process::Command;

    let output = Command::new("xcrun")
        .args(&["--sdk", sdk.sdk_name(), "--show-sdk-path"])
        .output()
        .expect("xcrun command failed to start");
    assert!(output.status.success());
    String::from_utf8_lossy(&output.stdout).trim().to_string()
}

fn show_type(desc: &str, clang_type: &clang::Type, indent_level: usize) {
    let indent = (0..indent_level)
        .map(|_| "   ")
        .collect::<Vec<&str>>()
        .concat();

    println!("{}{}: {:?}", indent, desc, clang_type);

    if let Some(argument_types) = clang_type.get_argument_types() {
        if !argument_types.is_empty() {
            println!("{}{} arguments types:", indent, desc);
            for (i, arg_type) in argument_types.iter().enumerate() {
                let arg_desc = format!("{} argument type {}", desc, i);
                show_type(&arg_desc, &arg_type, indent_level);
            }
        }
    }

    if let Some(base_type) = clang_type.get_objc_object_base_type() {
        println!("{}{} ObjC base type types: {:?}", indent, desc, base_type);
    }

    let canonical_type = clang_type.get_canonical_type();
    if &canonical_type != clang_type {
        println!("{}{} canonical type: {:?}", indent, desc, canonical_type);
    }

    if let Some(nullability) = clang_type.get_nullability() {
        println!("{}{} nullability: {:?}", indent, desc, nullability);
    }

    if let Some(class_type) = clang_type.get_class_type() {
        println!("{}{} class type: {:?}", indent, desc, class_type);
    }

    let objc_protocol_declarations = clang_type.get_objc_protocol_declarations();
    if !objc_protocol_declarations.is_empty() {
        println!(
            "{}{} objc protocol declarations: {:?}",
            indent, desc, objc_protocol_declarations
        );
    }
    let objc_type_arguments = clang_type.get_objc_type_arguments();
    if !objc_type_arguments.is_empty() {
        println!(
            "{}{} objc type arguments: {:?}",
            indent, desc, objc_type_arguments
        );
    }

    if let Some(pointee_type) = clang_type.get_pointee_type() {
        let pointee_desc = format!("{} pointee", desc);
        show_type(&pointee_desc, &pointee_type, indent_level);
    }

    if let Some(elaborated_type) = clang_type.get_elaborated_type() {
        let pointee_desc = format!("{} elaborated type", desc);
        show_type(&pointee_desc, &elaborated_type, indent_level);
    }

    if let Some(modified_type) = clang_type.get_modified_type() {
        let modified_desc = format!("{} modified", desc);
        show_type(&modified_desc, &modified_type, indent_level);
    }

    if let Some(decl) = clang_type.get_declaration() {
        println!("{}{} decl: {:?}", indent, desc, decl);
    }

    if let Ok(alignment) = clang_type.get_alignof() {
        println!("{}alignment: {:?}", indent, alignment);
    }

    if let Ok(size) = clang_type.get_sizeof() {
        println!("{}size: {:?}", indent, size);
    }

    if let Some(template_argument_types) = clang_type.get_template_argument_types() {
        println!(
            "{}{} template argument type: {:?}",
            indent, desc, template_argument_types
        );
    }

    if let Some(result_type) = clang_type.get_result_type() {
        let result_type_desc = format!("{} result type", desc);
        show_type(&result_type_desc, &result_type, indent_level);
    }

    if let Some(fields) = clang_type.get_fields() {
        println!("{}{} fields:", indent, desc);
        for field in fields {
            show_tree(&field, indent_level + 1);
        }
    }
}

fn show_tree(entity: &clang::Entity, indent_level: usize) {
    let indent = (0..indent_level)
        .map(|_| "   ")
        .collect::<Vec<&str>>()
        .concat();

    if let Some(name) = entity.get_name() {
        println!("{}*** kind: {:?} - {} ***", indent, entity.get_kind(), name);
    } else {
        println!("{}*** kind: {:?} ***", indent, entity.get_kind());
    }
    if entity.get_display_name() != entity.get_name() {
        if let Some(display_name) = entity.get_display_name() {
            println!("{}display name: {:?}", indent, display_name);
        }
    }

    if [
        EntityKind::ObjCInstanceMethodDecl,
        EntityKind::ObjCClassMethodDecl,
        EntityKind::ObjCPropertyDecl,
    ]
    .contains(&entity.get_kind())
    {
        println!("{}objc optional: {:?}", indent, entity.is_objc_optional());
    }

    if let Some(location) = entity.get_location() {
        println!("{}location: {:?}", indent, location);
    }

    if let Some(range) = entity.get_range() {
        println!("{}range: {:?}", indent, range);

        if entity.get_kind() == EntityKind::UnexposedAttr {
            println!("{}tokens: {:?}", indent, range.tokenize());
        }
    }

    if let Some(usr) = entity.get_usr() {
        println!("{}usr: {}", indent, usr.0);
    }

    if let Some(arguments) = entity.get_arguments() {
        if !arguments.is_empty() {
            println!("{}arguments:", indent);
            for arg in arguments {
                show_tree(&arg, indent_level + 1);
            }
        }
    }

    let availability = entity.get_availability();
    if availability != clang::Availability::Available {
        println!("{}availability: {:?}", indent, availability);
    }

    let canonical_entity = entity.get_canonical_entity();
    if &canonical_entity != entity {
        println!("{}canonical entity: {:?}", indent, canonical_entity);
    }

    if let Some(definition) = entity.get_definition() {
        println!("{}definition: {:?}", indent, definition);
    }

    if let Some(external_symbol) = entity.get_external_symbol() {
        println!("{}external symbol: {:?}", indent, external_symbol);
    }

    if let Some(module) = entity.get_module() {
        println!("{}module: {:?}", indent, module);
    }

    if let Some(template) = entity.get_template() {
        println!("{}template: {:?}", indent, template);
    }

    if let Some(template_kind) = entity.get_template_kind() {
        println!("{}template kind: {:?}", indent, template_kind);
    }

    if let Some(template_arguments) = entity.get_template_arguments() {
        println!("{}template_arguments: {:?}", indent, template_arguments);
    }

    if let Some(clang_type) = entity.get_type() {
        show_type("type", &clang_type, indent_level);
    }

    if let Some(enum_underlying_type) = entity.get_enum_underlying_type() {
        show_type("enum underlying type", &enum_underlying_type, indent_level);
    }

    if let Some(typedef_underlying_type) = entity.get_typedef_underlying_type() {
        show_type(
            "typedef underlying type",
            &typedef_underlying_type,
            indent_level,
        );
    }

    if let Some(visibility) = entity.get_visibility() {
        println!("{}visibility: {:?}", indent, visibility);
    }

    if let Some(result_type) = entity.get_result_type() {
        show_type("result type", &result_type, indent_level);
    }

    if let Some(mangled_name) = entity.get_mangled_name() {
        println!("{}mangled_name: {:?}", indent, mangled_name);
    }

    if let Some(objc_ib_outlet_collection_type) = entity.get_objc_ib_outlet_collection_type() {
        println!(
            "{}objc_ib_outlet_collection_type: {:?}",
            indent, objc_ib_outlet_collection_type
        );
    }

    if let Some(objc_type_encoding) = entity.get_objc_type_encoding() {
        println!("{}objc type encoding: {:?}", indent, objc_type_encoding);
    }

    if let Some(objc_selector_index) = entity.get_objc_selector_index() {
        println!("{}objc selector index: {:?}", indent, objc_selector_index);
    }

    if let Some(objc_qualifiers) = entity.get_objc_qualifiers() {
        println!("{}objc qualifiers: {:?}", indent, objc_qualifiers);
    }

    if let Some(objc_attributes) = entity.get_objc_attributes() {
        println!("{}objc attributes: {:?}", indent, objc_attributes);
    }

    // Seems to crash...
    // if let Some(objc_receiver_type) = entity.get_objc_receiver_type() {
    //     println!("{}objc receiver type: {:?}", indent, objc_receiver_type);
    // }

    // Getting a "pointer being freed was not allocated" when trying to use it...
    // if let Some(platform_availability) = entity.get_platform_availability() {
    //     if !platform_availability.is_empty() {
    //         println!(
    //             "{}platform availability: {:?}",
    //             indent, platform_availability
    //         );
    //     }
    // }

    if let Some(reference) = entity.get_reference() {
        println!("{}reference: {:?}", indent, reference);
    }

    if let Some(storage_class) = entity.get_storage_class() {
        println!("{}storage class: {:?}", indent, storage_class);
    }

    if let Ok(offset_of_field) = entity.get_offset_of_field() {
        println!("{}offset of field: {:?}", indent, offset_of_field);
    }

    // if let Some(semantic_parent) = entity.get_semantic_parent() {
    //     println!("{}semantic parent: {:?}", indent, semantic_parent);
    // }

    // if let Some(lexical_parent) = entity.get_lexical_parent() {
    //     println!("{}lexical parent: {:?}", indent, lexical_parent);
    // }

    let children = entity.get_children();
    if !children.is_empty() {
        println!("{}children:", indent);
        for child in children {
            show_tree(&child, indent_level + 1);
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub(crate) enum Nullability {
    NonNull,
    Nullable,
    Unspecified,
}

impl From<clang::Nullability> for Nullability {
    fn from(nul: clang::Nullability) -> Self {
        match nul {
            clang::Nullability::NonNull => Nullability::NonNull,
            clang::Nullability::Nullable => Nullability::Nullable,
            clang::Nullability::Unspecified => Nullability::Unspecified,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
struct IdObjPtr {
    protocols: Vec<String>,
}

#[derive(Clone, Debug, PartialEq)]
struct SomeInstanceObjPtr {
    interface: String,
    protocols: Vec<String>,
    type_args: Vec<ObjCTypeArg>,
}

#[derive(Clone, Debug, PartialEq)]
enum ObjPtrKind {
    Class,
    Id(IdObjPtr),
    SomeInstance(SomeInstanceObjPtr),
    Block(CallableDesc),
    TypeParam(String),
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct ObjPtr {
    kind: ObjPtrKind,
    nullability: Nullability,
}

impl ObjPtr {
    fn with_nullability(self, nullability: Nullability) -> Self {
        ObjPtr {
            kind: self.kind,
            nullability,
        }
    }
}

fn parm_decl_children<'a>(entity: &clang::Entity<'a>) -> impl Iterator<Item = clang::Entity<'a>> {
    entity
        .get_children()
        .into_iter()
        .filter(|child| child.get_kind() == EntityKind::ParmDecl)
}

#[derive(Clone, Debug, PartialEq)]
struct Field {
    name: Option<String>,
    objc_type: ObjCType,
}

impl Field {
    fn from_entity(entity: &clang::Entity, unnamed_tag_ids: &TagIdMap) -> Self {
        assert_eq!(entity.get_kind(), EntityKind::FieldDecl);
        Field {
            name: entity.get_name(),
            objc_type: ObjCType::from_type(
                &entity.get_type().unwrap(),
                &mut parm_decl_children(entity),
                unnamed_tag_ids,
            ),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub(crate) enum TagId {
    Named(String),
    Unnamed(u32),
}

impl TagId {
    fn from_entity(decl: &clang::Entity, unnamed_tag_ids: &TagIdMap) -> Self {
        assert!([
            EntityKind::StructDecl,
            EntityKind::UnionDecl,
            EntityKind::EnumDecl,
        ]
        .contains(&decl.get_kind()));

        if let Some(name) = decl.get_name() {
            TagId::Named(name)
        } else {
            TagId::Unnamed(
                *unnamed_tag_ids
                    .get(&decl.get_usr().unwrap())
                    .expect("all unnamed tag should have an id assigned"),
            )
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
enum TagKind {
    Union,
    Struct,
    Enum,
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct TagRef {
    id: TagId,
    kind: TagKind,
}

impl TagRef {
    fn from_type(clang_type: &clang::Type, unnamed_tag_ids: &TagIdMap) -> Self {
        let decl = clang_type.get_declaration().unwrap();

        let kind = match decl.get_kind() {
            EntityKind::StructDecl => TagKind::Struct,
            EntityKind::UnionDecl => TagKind::Union,
            EntityKind::EnumDecl => TagKind::Enum,
            unexpected_kind => panic!(
                "Unexpected kind for tag declaration {:?}: {:?}",
                unexpected_kind, clang_type
            ),
        };

        let id = TagId::from_entity(&decl, unnamed_tag_ids);

        TagRef { id, kind }
    }
}

bitflags! {
    struct ParamAttrs: u8 {
        const CONSUMED = 0b0000_0001;
        // noescape should really be for function and block pointers
        // (or typedefs of those), but with the AST libclang provides,
        // it's way easier to handle it as a parameter attribute.
        const NOESCAPE = 0b0000_0010;
    }
}

impl ParamAttrs {
    fn from_decl(decl: &clang::Entity) -> Self {
        assert_eq!(decl.get_kind(), EntityKind::ParmDecl);
        let mut attrs = Self::empty();
        for child in decl.get_children() {
            match child.get_kind() {
                EntityKind::NSConsumed => attrs.insert(Self::CONSUMED),
                EntityKind::UnexposedAttr => {
                    if let Some(range) = child.get_range() {
                        let tokens = range.tokenize();
                        if tokens.len() == 1 && tokens[0].get_spelling() == "noescape" {
                            attrs.insert(Self::NOESCAPE);
                        }
                    }
                }
                _ => (),
            }
        }
        attrs
    }
}

#[derive(Clone, Debug, PartialEq)]
struct Param {
    name: Option<String>,
    objc_type: ObjCType,
    attrs: ParamAttrs,
}

bitflags! {
    struct CallableAttrs: u8 {
        const RETURNS_RETAINED = 0b0000_0001;
    }
}

impl CallableAttrs {
    fn from_decl(decl: &clang::Entity) -> Self {
        let mut attrs = Self::empty();
        for child in decl.get_children() {
            match child.get_kind() {
                EntityKind::NSReturnsRetained => attrs.insert(Self::RETURNS_RETAINED),
                _ => (),
            }
        }
        attrs
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct CallableDesc {
    result: Box<ObjCType>,
    params: Option<Vec<Param>>,
    is_variadic: bool,
    attrs: CallableAttrs,
}

impl CallableDesc {
    fn from_type<'a>(
        clang_type: &clang::Type,
        base_parm_decls: &mut impl Iterator<Item = clang::Entity<'a>>,
        unnamed_tag_ids: &TagIdMap,
    ) -> Self {
        if clang_type.get_kind() == TypeKind::Attributed {
            let mut callable = Self::from_type(
                &clang_type.get_modified_type().unwrap(),
                base_parm_decls,
                unnamed_tag_ids,
            );
            let display_name = clang_type.get_display_name();
            // Yes, that is very ugly but I could not find a better way.
            if display_name.contains("__attribute__((ns_returns_retained))") {
                callable.attrs.insert(CallableAttrs::RETURNS_RETAINED);
            } else {
                panic!(
                    "Have to check what this Attributed is for - {:?}",
                    clang_type
                );
            }
            return callable;
        }

        // result must be processed before parameters due to the order the entities are in base_parm_decls.
        let result = Box::new(ObjCType::from_type(
            &clang_type.get_result_type().unwrap(),
            base_parm_decls,
            unnamed_tag_ids,
        ));

        let params = match clang_type.get_kind() {
            TypeKind::FunctionNoPrototype => None,
            TypeKind::FunctionPrototype => Some({
                clang_type
                    .get_argument_types()
                    .unwrap()
                    .iter()
                    .map(|_| {
                        // Not using the value of argument types, only their count.
                        // The param decl taken from the entity seems to always have better information.
                        let parm_decl = base_parm_decls.next().unwrap();
                        let objc_type = ObjCType::from_type(
                            &parm_decl.get_type().unwrap(),
                            &mut parm_decl_children(&parm_decl),
                            unnamed_tag_ids,
                        );
                        let attrs = ParamAttrs::from_decl(&parm_decl);
                        Param {
                            name: parm_decl.get_name(),
                            objc_type,
                            attrs,
                        }
                    })
                    .collect()
            }),
            unexpected_kind => panic!(
                "Unexpected kind for function declaration {:?}: {:?}",
                unexpected_kind, clang_type
            ),
        };

        CallableDesc {
            result,
            params,
            is_variadic: clang_type.is_variadic(),
            attrs: CallableAttrs::empty(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct TypedefRef {
    pub(crate) name: String,
}

impl TypedefRef {
    fn from_type(clang_type: &clang::Type) -> Self {
        assert_eq!(clang_type.get_kind(), TypeKind::Typedef);
        let name = clang_type.get_display_name();
        TypedefRef { name }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct Pointer {
    pointee: Box<ObjCType>,
    nullability: Nullability,
}

impl Pointer {
    fn with_nullability(self, nullability: Nullability) -> Self {
        Pointer {
            pointee: self.pointee,
            nullability,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct Array {
    size: Option<usize>,
    element: Box<ObjCType>,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub(crate) enum SignedOrNotInt {
    Signed(i64),
    Unsigned(u64),
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct EnumValue {
    pub(crate) name: String,
    pub(crate) value: SignedOrNotInt,
}

fn type_signedness(clang_type: &clang::Type) -> Option<Signedness> {
    match clang_type.get_kind() {
        TypeKind::SChar
        | TypeKind::CharS
        | TypeKind::Short
        | TypeKind::Int
        | TypeKind::Long
        | TypeKind::LongLong
        | TypeKind::Float
        | TypeKind::Double
        | TypeKind::LongDouble => Some(Signedness::Signed),
        TypeKind::UChar
        | TypeKind::CharU
        | TypeKind::UShort
        | TypeKind::UInt
        | TypeKind::ULong
        | TypeKind::ULongLong => Some(Signedness::Unsigned),
        TypeKind::Attributed => type_signedness(&clang_type.get_modified_type().unwrap()),
        TypeKind::Typedef => type_signedness(
            &clang_type
                .get_declaration()
                .unwrap()
                .get_typedef_underlying_type()
                .unwrap(),
        ),
        TypeKind::Elaborated => type_signedness(&clang_type.get_elaborated_type().unwrap()),
        _ => None,
    }
}

#[derive(Clone, Debug, PartialEq)]
enum ObjCTypeArg {
    ObjPtr(ObjPtr),
    Typedef(TypedefRef),
}

#[derive(Copy, Clone, Debug, PartialEq)]
enum Signedness {
    Signed,
    Unsigned,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub(crate) enum NumKind {
    SChar,
    UChar,
    Short,
    UShort,
    Int,
    UInt,
    Long,
    ULong,
    LongLong,
    ULongLong,
    Float,
    Double,
    LongDouble,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub(crate) enum Unsupported {
    Vector,
    Unexposed,
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum ObjCType {
    Void,
    Bool,
    Num(NumKind),
    Tag(TagRef),
    Typedef(TypedefRef),
    Pointer(Pointer),
    Function(CallableDesc),
    ObjPtr(ObjPtr),
    /// `SEL` in Objective-C. A special type of (non-object) pointer.
    ObjCSel(Nullability),
    Array(Array),
    Unsupported(Unsupported),
}

impl ObjCType {
    fn from_type<'a>(
        clang_type: &clang::Type,
        base_parm_decls: &mut impl Iterator<Item = clang::Entity<'a>>,
        unnamed_tag_ids: &TagIdMap,
    ) -> Self {
        match clang_type.get_kind() {
            TypeKind::Void => Self::Void,
            // SChar is "signed char", CharS is "char" when it is signed by default.
            TypeKind::SChar | TypeKind::CharS => Self::Num(NumKind::SChar),
            // UChar is "unsigned char", CharU is "char" when it is unsigned by default.
            TypeKind::UChar | TypeKind::CharU => Self::Num(NumKind::UChar),
            TypeKind::Short => Self::Num(NumKind::Short),
            TypeKind::UShort => Self::Num(NumKind::UShort),
            TypeKind::Int => Self::Num(NumKind::Int),
            TypeKind::UInt => Self::Num(NumKind::UInt),
            TypeKind::Long => Self::Num(NumKind::Long),
            TypeKind::ULong => Self::Num(NumKind::ULong),
            TypeKind::LongLong => Self::Num(NumKind::LongLong),
            TypeKind::ULongLong => Self::Num(NumKind::ULongLong),
            TypeKind::Float => Self::Num(NumKind::Float),
            TypeKind::Double => Self::Num(NumKind::Double),
            TypeKind::LongDouble => Self::Num(NumKind::LongDouble),
            TypeKind::Pointer => {
                let pointee_type = clang_type.get_pointee_type().unwrap();
                Self::Pointer(Pointer {
                    pointee: Box::new(Self::from_type(
                        &pointee_type,
                        base_parm_decls,
                        unnamed_tag_ids,
                    )),
                    nullability: Nullability::Unspecified,
                })
            }
            TypeKind::ObjCObjectPointer => {
                let pointee_type = {
                    let mut pointee = clang_type.get_pointee_type().unwrap();
                    loop {
                        match pointee.get_kind() {
                            // TODO: Check what's the difference between ObjCObject and ObjCInterface
                            TypeKind::ObjCObject | TypeKind::ObjCInterface => break pointee,
                            // Attributed is for example when __kindof is used.
                            TypeKind::Attributed => {
                                pointee = pointee.get_modified_type().unwrap();
                            }
                            unexpected_kind => {
                                panic!("unexpected pointee kind {:?}", unexpected_kind)
                            }
                        }
                    }
                };
                let protocols = pointee_type
                    .get_objc_protocol_declarations()
                    .iter()
                    .map(|decl| {
                        assert_eq!(decl.get_kind(), EntityKind::ObjCProtocolDecl);
                        decl.get_name().unwrap()
                    })
                    .collect();

                let type_args: Vec<ObjCTypeArg> = pointee_type
                    .get_objc_type_arguments()
                    .iter()
                    .map(|arg| {
                        match Self::from_type(arg, &mut Vec::new().into_iter(), unnamed_tag_ids) {
                            Self::ObjPtr(ptr) => ObjCTypeArg::ObjPtr(ptr),
                            Self::Typedef(typedef) => ObjCTypeArg::Typedef(typedef),
                            unexpected => {
                                panic!("Type arguments not expected to be {:?}", unexpected)
                            }
                        }
                    })
                    .collect();

                let base_type = pointee_type.get_objc_object_base_type().unwrap();

                if base_type.get_kind() == TypeKind::ObjCId {
                    assert!(type_args.is_empty());
                    assert_eq!(base_type.get_display_name(), "id");
                    Self::ObjPtr(ObjPtr {
                        kind: ObjPtrKind::Id(IdObjPtr { protocols }),
                        nullability: Nullability::Unspecified,
                    })
                } else {
                    Self::ObjPtr(ObjPtr {
                        kind: ObjPtrKind::SomeInstance(SomeInstanceObjPtr {
                            interface: base_type.get_display_name(),
                            protocols,
                            type_args,
                        }),
                        nullability: Nullability::Unspecified,
                    })
                }
            }
            TypeKind::Attributed => {
                let modified = Self::from_type(
                    &clang_type.get_modified_type().unwrap(),
                    base_parm_decls,
                    unnamed_tag_ids,
                );
                if let Some(nullability) = clang_type.get_nullability() {
                    let nullability = nullability.into();
                    match modified {
                        Self::Pointer(ptr) => Self::Pointer(ptr.with_nullability(nullability)),
                        Self::ObjPtr(ptr) => Self::ObjPtr(ptr.with_nullability(nullability)),
                        Self::ObjCSel(_) => Self::ObjCSel(nullability),
                        _ => modified,
                    }
                } else {
                    modified
                }
            }
            TypeKind::ObjCTypeParam => Self::ObjPtr(ObjPtr {
                kind: ObjPtrKind::TypeParam(clang_type.get_display_name()),
                nullability: Nullability::Unspecified,
            }),
            TypeKind::ObjCId => Self::ObjPtr(ObjPtr {
                kind: ObjPtrKind::Id(IdObjPtr {
                    protocols: Vec::new(),
                }),
                nullability: Nullability::Unspecified,
            }),
            TypeKind::ObjCSel => Self::ObjCSel(Nullability::Unspecified),
            TypeKind::ObjCClass => Self::ObjPtr(ObjPtr {
                kind: ObjPtrKind::Class,
                nullability: Nullability::Unspecified,
            }),
            TypeKind::Typedef => {
                let decl = clang_type.get_declaration().unwrap();
                if decl.get_kind() == EntityKind::TemplateTypeParameter {
                    Self::ObjPtr(ObjPtr {
                        kind: ObjPtrKind::TypeParam(clang_type.get_display_name()),
                        nullability: Nullability::Unspecified,
                    })
                } else {
                    Self::Typedef(TypedefRef::from_type(&clang_type))
                }
            }
            TypeKind::Elaborated => Self::from_type(
                &clang_type.get_elaborated_type().unwrap(),
                base_parm_decls,
                unnamed_tag_ids,
            ),
            TypeKind::Record | TypeKind::Enum => {
                Self::Tag(TagRef::from_type(&clang_type, unnamed_tag_ids))
            }
            TypeKind::FunctionNoPrototype | TypeKind::FunctionPrototype => Self::Function(
                CallableDesc::from_type(&clang_type, base_parm_decls, unnamed_tag_ids),
            ),
            TypeKind::ConstantArray => Self::Array(Array {
                size: Some(clang_type.get_size().unwrap()),
                element: Box::new(Self::from_type(
                    &clang_type.get_element_type().unwrap(),
                    base_parm_decls,
                    unnamed_tag_ids,
                )),
            }),
            TypeKind::IncompleteArray => Self::Array(Array {
                size: None,
                element: Box::new(Self::from_type(
                    &clang_type.get_element_type().unwrap(),
                    base_parm_decls,
                    unnamed_tag_ids,
                )),
            }),
            TypeKind::BlockPointer => Self::ObjPtr(ObjPtr {
                kind: ObjPtrKind::Block(CallableDesc::from_type(
                    &clang_type.get_pointee_type().unwrap(),
                    base_parm_decls,
                    unnamed_tag_ids,
                )),
                nullability: Nullability::Unspecified,
            }),
            TypeKind::Bool => Self::Bool,
            TypeKind::Vector => Self::Unsupported(Unsupported::Vector),
            TypeKind::Unexposed => Self::Unsupported(Unsupported::Unexposed),
            unknown_kind => panic!("Unhandled type kind {:?}: {:?}", unknown_kind, clang_type),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum FileKind {
    None,
    Main,
    Some(String),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct Location {
    file_kind: FileKind,
    line: u32,
    column: u32,
}

impl Location {
    fn from_entity(entity: &clang::Entity) -> Self {
        let source_location = entity.get_location().unwrap();
        // let location = source_location.get_file_location();
        let location = source_location.get_spelling_location();

        let file_kind = if let Some(file) = location.file {
            // For some reason, source_location.is_in_main_file() doesn't seem to work properly so do it ourselves.
            let tu_file = entity
                .get_translation_unit()
                .get_entity()
                .get_range()
                .unwrap()
                .get_start()
                .get_file_location()
                .file
                .unwrap();
            if tu_file == file {
                FileKind::Main
            } else {
                FileKind::Some(file.get_path().to_str().unwrap().to_string())
            }
        } else {
            FileKind::None
        };

        let line = location.line;
        let column = location.column;

        Location {
            file_kind,
            line,
            column,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
struct ObjCParam {
    name: String,
    objc_type: ObjCType,
    attrs: ParamAttrs,
}

impl ObjCParam {
    fn from_entity(decl: &clang::Entity, unnamed_tag_ids: &TagIdMap) -> Self {
        assert_eq!(decl.get_kind(), EntityKind::ParmDecl);
        let name = decl.get_name().unwrap();
        let objc_type = ObjCType::from_type(
            &decl.get_type().unwrap(),
            &mut parm_decl_children(decl),
            unnamed_tag_ids,
        );
        let attrs = ParamAttrs::from_decl(&decl);
        ObjCParam {
            name,
            objc_type,
            attrs,
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
enum ObjCMethodKind {
    Class,
    Instance,
}

#[derive(Clone, Debug, PartialEq)]
struct ObjCMethod {
    name: String,
    kind: ObjCMethodKind,
    params: Vec<ObjCParam>,
    result: ObjCType,
    attrs: CallableAttrs,
}

impl ObjCMethod {
    fn from_entity(entity: &clang::Entity, unnamed_tag_ids: &TagIdMap) -> Self {
        let kind = match entity.get_kind() {
            EntityKind::ObjCClassMethodDecl => ObjCMethodKind::Class,
            EntityKind::ObjCInstanceMethodDecl => ObjCMethodKind::Instance,
            _ => panic!("entity should be either a class or instance method"),
        };

        let params = entity
            .get_arguments()
            .unwrap()
            .iter()
            .map(|arg| ObjCParam::from_entity(arg, unnamed_tag_ids))
            .collect();

        let result = ObjCType::from_type(
            &entity.get_result_type().unwrap(),
            &mut parm_decl_children(entity),
            unnamed_tag_ids,
        );

        let attrs = CallableAttrs::from_decl(entity);

        ObjCMethod {
            name: entity.get_name().unwrap(),
            kind,
            params,
            result,
            attrs,
        }
    }
}

fn is_generated_from_property(method_entity: &clang::Entity) -> bool {
    assert!([
        EntityKind::ObjCInstanceMethodDecl,
        EntityKind::ObjCClassMethodDecl,
    ]
    .contains(&method_entity.get_kind()));
    let parent = method_entity.get_semantic_parent().unwrap();
    assert!([
        EntityKind::ObjCInterfaceDecl,
        EntityKind::ObjCProtocolDecl,
        EntityKind::ObjCCategoryDecl,
    ]
    .contains(&parent.get_kind()));
    let method_location = method_entity.get_location().unwrap();
    parent
        .get_children()
        .into_iter()
        .filter(|sibling| sibling.get_kind() == EntityKind::ObjCPropertyDecl)
        .any(|sibling| sibling.get_location().unwrap() == method_location)
}

#[derive(Copy, Clone, Debug, PartialEq)]
enum PropOwnership {
    Strong,
    Weak,
    Copy,
    Assign,
    Retain,
    UnsafeUnretained,
}

#[derive(Clone, Debug, PartialEq)]
struct Property {
    name: String,
    value: ObjCType,
    is_atomic: bool,
    is_writable: bool,
    is_class: bool,
    ownership: PropOwnership,
    getter: Option<String>,
    setter: Option<String>,
}

impl Property {
    fn from_entity(entity: &clang::Entity, unnamed_tag_ids: &TagIdMap) -> Self {
        assert_eq!(entity.get_kind(), EntityKind::ObjCPropertyDecl);

        let name = entity.get_name().unwrap();
        let value = ObjCType::from_type(
            &entity.get_type().unwrap(),
            &mut parm_decl_children(&entity),
            unnamed_tag_ids,
        );
        let attributes = entity
            .get_objc_attributes()
            .unwrap_or_else(|| clang::ObjCAttributes {
                readonly: false,
                getter: false,
                assign: false,
                readwrite: false,
                retain: false,
                copy: false,
                nonatomic: false,
                setter: false,
                atomic: false,
                weak: false,
                strong: false,
                unsafe_retained: false,
                class: false,
            });
        let is_atomic = {
            if attributes.atomic {
                assert!(!attributes.nonatomic);
                true
            } else {
                !attributes.nonatomic
            }
        };
        let is_writable = attributes.readwrite;
        if is_writable {
            assert!(!attributes.readonly);
        }
        let is_class = attributes.class;
        let ownership = {
            if attributes.strong {
                assert!(!attributes.weak);
                assert!(!attributes.copy);
                assert!(!attributes.assign);
                assert!(!attributes.retain);
                assert!(!attributes.unsafe_retained);
                PropOwnership::Strong
            } else if attributes.weak {
                assert!(!attributes.copy);
                assert!(!attributes.assign);
                assert!(!attributes.retain);
                assert!(!attributes.unsafe_retained);
                PropOwnership::Weak
            } else if attributes.copy {
                assert!(!attributes.assign);
                assert!(!attributes.retain);
                assert!(!attributes.unsafe_retained);
                PropOwnership::Copy
            } else if attributes.assign {
                assert!(!attributes.retain);
                assert!(!attributes.unsafe_retained);
                PropOwnership::Assign
            } else if attributes.retain {
                assert!(!attributes.unsafe_retained);
                PropOwnership::Retain
            } else if attributes.unsafe_retained {
                // TODO: The name in the clang crate has to be fixed (it's unsafe_unretained, not unsafe_retained)
                PropOwnership::UnsafeUnretained
            } else {
                PropOwnership::Strong
            }
        };
        let getter;
        let setter;
        if attributes.getter || attributes.setter {
            let parent = entity.get_semantic_parent().unwrap();
            let property_location = entity.get_location().unwrap();
            let methods_at_same_location: Vec<clang::Entity> = parent
                .get_children()
                .into_iter()
                .filter(|sibling| {
                    [
                        EntityKind::ObjCInstanceMethodDecl,
                        EntityKind::ObjCClassMethodDecl,
                    ]
                    .contains(&sibling.get_kind())
                        && sibling.get_location().unwrap() == property_location
                })
                .collect();
            if attributes.getter {
                getter = Some(
                    methods_at_same_location
                        .iter()
                        .find(|method| method.get_arguments().unwrap().is_empty())
                        .unwrap()
                        .get_name()
                        .unwrap(),
                );
            } else {
                getter = None;
            }
            if attributes.setter {
                setter = Some(
                    methods_at_same_location
                        .iter()
                        .find(|method| !method.get_arguments().unwrap().is_empty())
                        .unwrap()
                        .get_name()
                        .unwrap(),
                );
            } else {
                setter = None;
            }
        } else {
            getter = None;
            setter = None;
        };

        Property {
            name,
            value,
            is_atomic,
            is_writable,
            is_class,
            ownership,
            getter,
            setter,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct InterfaceDef {
    pub(crate) name: String,
    superclass: Option<String>,
    adopted_protocols: Vec<String>,
    template_params: Vec<String>,
    methods: Vec<ObjCMethod>,
    properties: Vec<Property>,
    origin: Option<Origin>,
}

impl InterfaceDef {
    fn from_entity(entity: &clang::Entity, unnamed_tag_ids: &TagIdMap) -> Self {
        assert_eq!(entity.get_kind(), EntityKind::ObjCInterfaceDecl);
        let name = entity.get_name().unwrap();
        let children = entity.get_children();

        let superclass = children
            .iter()
            .find(|child| child.get_kind() == EntityKind::ObjCSuperClassRef)
            .map(|child| child.get_name().unwrap());
        let adopted_protocols = children
            .iter()
            .filter(|child| child.get_kind() == EntityKind::ObjCProtocolRef)
            .map(|child| child.get_name().unwrap())
            .collect();
        let template_params = children
            .iter()
            .filter(|child| child.get_kind() == EntityKind::TemplateTypeParameter)
            .map(|child| child.get_name().unwrap())
            .collect();
        let methods = children
            .iter()
            .filter(|child| {
                [
                    EntityKind::ObjCInstanceMethodDecl,
                    EntityKind::ObjCClassMethodDecl,
                ]
                .contains(&child.get_kind())
            })
            // Methods generated from property don't have children with the type info we want
            // so for the time being just ignore them.
            .filter(|method| !is_generated_from_property(method))
            .map(|method| ObjCMethod::from_entity(method, unnamed_tag_ids))
            .collect();
        let properties = children
            .iter()
            .filter(|child| child.get_kind() == EntityKind::ObjCPropertyDecl)
            .map(|prop| Property::from_entity(prop, unnamed_tag_ids))
            .collect();
        let origin = Origin::from_entity(entity);

        InterfaceDef {
            name,
            superclass,
            adopted_protocols,
            template_params,
            methods,
            properties,
            origin,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct CategoryDef {
    pub(crate) name: Option<String>,
    pub(crate) class: String,
    adopted_protocols: Vec<String>,
    methods: Vec<ObjCMethod>,
    properties: Vec<Property>,
    origin: Option<Origin>,
}

impl CategoryDef {
    fn from_entity(entity: &clang::Entity, unnamed_tag_ids: &TagIdMap) -> Self {
        assert_eq!(entity.get_kind(), EntityKind::ObjCCategoryDecl);
        let children = entity.get_children();

        let name = entity.get_name();

        let class = children
            .iter()
            .find(|child| child.get_kind() == EntityKind::ObjCClassRef)
            .unwrap()
            .get_name()
            .unwrap();

        let adopted_protocols = children
            .iter()
            .filter(|child| child.get_kind() == EntityKind::ObjCProtocolRef)
            .map(|child| child.get_name().unwrap())
            .collect();

        let methods = children
            .iter()
            .filter(|child| {
                [
                    EntityKind::ObjCInstanceMethodDecl,
                    EntityKind::ObjCClassMethodDecl,
                ]
                .contains(&child.get_kind())
            })
            // Methods generated from property don't have children with the type info we want
            // so for the time being just ignore them.
            .filter(|method| !is_generated_from_property(method))
            .map(|method| ObjCMethod::from_entity(method, unnamed_tag_ids))
            .collect();

        let properties = children
            .iter()
            .filter(|child| child.get_kind() == EntityKind::ObjCPropertyDecl)
            .map(|prop| Property::from_entity(prop, unnamed_tag_ids))
            .collect();

        let origin = Origin::from_entity(entity);

        CategoryDef {
            name,
            class,
            adopted_protocols,
            methods,
            properties,
            origin,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
struct ProtocolMethod {
    method: ObjCMethod,
    is_optional: bool,
}

#[derive(Clone, Debug, PartialEq)]
struct ProtocolProperty {
    property: Property,
    is_optional: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct ProtocolDef {
    pub(crate) name: String,
    inherited_protocols: Vec<String>,
    methods: Vec<ProtocolMethod>,
    properties: Vec<ProtocolProperty>,
    origin: Option<Origin>,
}

impl ProtocolDef {
    fn from_entity(entity: &clang::Entity, unnamed_tag_ids: &TagIdMap) -> Self {
        assert_eq!(entity.get_kind(), EntityKind::ObjCProtocolDecl);
        let children = entity.get_children();

        let inherited_protocols = children
            .iter()
            .filter(|child| child.get_kind() == EntityKind::ObjCProtocolRef)
            .map(|child| child.get_name().unwrap())
            .collect();

        let methods = children
            .iter()
            .filter(|child| {
                [
                    EntityKind::ObjCInstanceMethodDecl,
                    EntityKind::ObjCClassMethodDecl,
                ]
                .contains(&child.get_kind())
            })
            // Methods generated from property don't have children with the type info we want
            // so for the time being just ignore them.
            .filter(|child| !is_generated_from_property(child))
            .map(|child| ProtocolMethod {
                method: ObjCMethod::from_entity(child, unnamed_tag_ids),
                is_optional: child.is_objc_optional(),
            })
            .collect();

        let properties = children
            .iter()
            .filter(|child| child.get_kind() == EntityKind::ObjCPropertyDecl)
            .map(|child| ProtocolProperty {
                property: Property::from_entity(child, unnamed_tag_ids),
                is_optional: child.is_objc_optional(),
            })
            .collect();

        let origin = Origin::from_entity(entity);

        ProtocolDef {
            name: entity.get_name().unwrap(),
            inherited_protocols,
            methods,
            properties,
            origin,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct TypedefDecl {
    pub(crate) name: String,
    pub(crate) underlying: ObjCType,
    pub(crate) origin: Option<Origin>,
}

impl TypedefDecl {
    fn from_entity(decl: &clang::Entity, unnamed_tag_ids: &TagIdMap) -> Self {
        assert_eq!(decl.get_kind(), EntityKind::TypedefDecl);

        let name = decl.get_name().unwrap();
        let underlying = ObjCType::from_type(
            &decl.get_typedef_underlying_type().unwrap(),
            &mut parm_decl_children(&decl),
            unnamed_tag_ids,
        );
        let origin = Origin::from_entity(decl);

        TypedefDecl {
            name,
            underlying,
            origin,
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub(crate) enum RecordKind {
    Union,
    Struct,
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct RecordDef {
    pub(crate) id: TagId,
    pub(crate) kind: RecordKind,
    fields: Vec<Field>,
    origin: Option<Origin>,
}

impl RecordDef {
    fn from_entity(decl: &clang::Entity, unnamed_tag_ids: &TagIdMap) -> Self {
        let id = TagId::from_entity(decl, unnamed_tag_ids);

        let kind = match decl.get_kind() {
            EntityKind::StructDecl => RecordKind::Struct,
            EntityKind::UnionDecl => RecordKind::Union,
            unexpected => panic!("Record declaration is not expected to be {:?}", unexpected),
        };

        assert!(
            decl.get_definition().is_some(),
            "An record definition should have fields definition"
        );
        let fields = decl
            .get_type()
            .unwrap()
            .get_fields()
            .unwrap()
            .iter()
            .map(|field| Field::from_entity(field, unnamed_tag_ids))
            .collect();

        let origin = Origin::from_entity(decl);

        RecordDef {
            id,
            kind,
            fields,
            origin,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct FuncDecl {
    pub(crate) name: String,
    desc: CallableDesc,
    origin: Option<Origin>,
}

impl FuncDecl {
    fn from_entity(decl: &clang::Entity, unnamed_tag_ids: &TagIdMap) -> Self {
        assert_eq!(decl.get_kind(), EntityKind::FunctionDecl);
        let clang_type = decl.get_type().unwrap();

        let name = decl.get_name().unwrap();
        let desc =
            CallableDesc::from_type(&clang_type, &mut parm_decl_children(&decl), unnamed_tag_ids);
        let origin = Origin::from_entity(decl);

        FuncDecl { name, desc, origin }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct EnumDef {
    pub(crate) id: TagId,
    pub(crate) underlying: ObjCType,
    pub(crate) values: Vec<EnumValue>,
    pub(crate) origin: Option<Origin>,
}

impl EnumDef {
    fn from_entity(decl: &clang::Entity, unnamed_tag_ids: &TagIdMap) -> Self {
        assert_eq!(decl.get_kind(), EntityKind::EnumDecl);

        let id = TagId::from_entity(decl, unnamed_tag_ids);

        let underlying = ObjCType::from_type(
            &decl.get_enum_underlying_type().unwrap(),
            &mut parm_decl_children(&decl),
            unnamed_tag_ids,
        );

        assert!(decl.get_definition().is_some());
        let signedness = type_signedness(&decl.get_enum_underlying_type().unwrap())
            .expect("The underlying type of an enum should have a signedness");
        let values = decl
            .get_children()
            .into_iter()
            .filter(|child| child.get_kind() == EntityKind::EnumConstantDecl)
            .map(|decl| {
                let values = decl.get_enum_constant_value().unwrap();

                EnumValue {
                    name: decl.get_name().unwrap(),
                    value: match signedness {
                        Signedness::Signed => SignedOrNotInt::Signed(values.0),
                        Signedness::Unsigned => SignedOrNotInt::Unsigned(values.1),
                    },
                }
            })
            .collect();
        let origin = Origin::from_entity(decl);

        EnumDef {
            id,
            underlying,
            values,
            origin,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum Decl {
    ProtocolDef(ProtocolDef),
    InterfaceDef(InterfaceDef),
    CategoryDef(CategoryDef),
    RecordDef(RecordDef),
    EnumDef(EnumDef),
    TypedefDecl(TypedefDecl),
    FuncDecl(FuncDecl),
    // VarDecl(VarDecl),
}

#[derive(Debug)]
pub(crate) enum ParseError {
    SourceError(clang::SourceError),
    CompilationError(String),
}

impl From<clang::SourceError> for ParseError {
    fn from(err: clang::SourceError) -> Self {
        ParseError::SourceError(err)
    }
}

type TagIdMap = HashMap<clang::Usr, u32>;

fn preprocess_objc(source: &str) -> String {
    use std::io::Write;
    use std::process::{Command, Stdio};

    let mut child = Command::new("xcrun")
        .args(&[
            "clang",
            "-E",
            "-x",
            "objective-c",
            "-fobjc-arc",
            "-isysroot",
            &sdk_path(AppleSdk::MacOs),
            "-", // read from stdin
        ])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .expect("clang command failed to start");

    {
        let stdin = child.stdin.as_mut().expect("Failed to open stdin");
        stdin
            .write_all(source.as_bytes())
            .expect("Failed to write to stdin");
    }

    let output = child.wait_with_output().expect("Failed to read stdout");
    assert!(output.status.success());
    String::from_utf8_lossy(&output.stdout).to_string()
}

/// Prints the full clang AST.
///
/// Should only be used for debugging purpose. Definitions with cycles can end up in a stack overflow.
pub(crate) fn print_full_clang_ast(source: &str) {
    let clang = Clang::new().expect("Could not load libclang");

    // Preprocess before to get more easily interesting tokens coming from #defines.
    let source: &str = &preprocess_objc(source);

    // The documentation says that files specified as unsaved must exist so create a dummy temporary empty file
    let file = tempfile::NamedTempFile::new().unwrap();
    let index = clang::Index::new(&clang, false, true);
    let mut parser = index.parser(file.path());
    configure_parser(&mut parser);

    parser.unsaved(&[clang::Unsaved::new(file.path(), source)]);
    let tu = parser.parse().expect("source should build cleanly");
    show_tree(&tu.get_entity(), 0);
}

fn configure_parser(parser: &mut clang::Parser) {
    parser.arguments(&[
        "-x",
        "objective-c", // The file doesn't have an Objective-C extension so set the language explicitely (for some reason -ObjC doesn't work properly)
        "-fobjc-arc",
        "-isysroot",
        &sdk_path(AppleSdk::MacOs),
    ]);
    parser.skip_function_bodies(true);
    parser.include_attributed_types(true); // Needed to get nullability
    parser.visit_implicit_attributes(true); // TODO: Check if needed
}

pub(crate) fn ast_from_str(source: &str) -> Result<Vec<Decl>, ParseError> {
    use clang::diagnostic::Severity;

    let clang = Clang::new().expect("Could not load libclang");

    // Preprocess before to get more easily interesting tokens coming from #defines.
    let source: &str = &preprocess_objc(source);

    // The documentation says that files specified as unsaved must exist so create a dummy temporary empty file
    let file = tempfile::NamedTempFile::new().unwrap();
    let index = clang::Index::new(&clang, false, true);
    let mut parser = index.parser(file.path());
    configure_parser(&mut parser);

    parser.unsaved(&[clang::Unsaved::new(file.path(), source)]);
    let tu = parser.parse()?;
    // The parser will try to parse as much as possible, even with errors.
    // In that case, we still want fail because some information will be missing anyway.
    let diagnostics = tu.get_diagnostics();
    let mut errors = diagnostics.iter().filter(|diagnostic| {
        let severity = diagnostic.get_severity();
        [Severity::Error, Severity::Fatal].contains(&severity)
    });
    if let Some(error) = errors.next() {
        return Err(ParseError::CompilationError(error.get_text()));
    }

    let mut decls: Vec<Decl> = Vec::new();

    let tu_entity = tu.get_entity();

    let mut next_unique_id: u32 = 1;
    let mut unnamed_tag_ids = TagIdMap::new();
    // Make unique identifiers for tags (struct/union/enum) that have no name.
    // Not using USRs as I'm not sure they are supposed to be stable between different clang versions.
    // Also not using locations as due to the the processor their might not be unique.
    tu_entity.visit_children(|entity, _| {
        match entity.get_kind() {
            EntityKind::StructDecl | EntityKind::UnionDecl | EntityKind::EnumDecl
                if entity.get_name().is_none() => {}
            _ => return clang::EntityVisitResult::Recurse,
        }

        if entity.get_definition().unwrap() != entity {
            return clang::EntityVisitResult::Recurse;
        }

        let usr = entity.get_usr().unwrap();
        if unnamed_tag_ids.contains_key(&usr) {
            return clang::EntityVisitResult::Recurse;
        }
        unnamed_tag_ids.insert(usr, next_unique_id);

        next_unique_id += 1;

        clang::EntityVisitResult::Recurse
    });
    let unnamed_tag_ids = unnamed_tag_ids;

    let mut visited: HashSet<clang::Usr> = HashSet::new();
    tu_entity.visit_children(|entity, _| {
        let usr = entity.get_usr();
        if let Some(usr) = entity.get_usr() {
            if visited.contains(&usr) {
                return clang::EntityVisitResult::Recurse;
            }
        }
        let decl = match entity.get_kind() {
            EntityKind::ObjCInterfaceDecl => Some(Decl::InterfaceDef(InterfaceDef::from_entity(
                &entity,
                &unnamed_tag_ids,
            ))),
            EntityKind::ObjCProtocolDecl => Some(Decl::ProtocolDef(ProtocolDef::from_entity(
                &entity,
                &unnamed_tag_ids,
            ))),
            EntityKind::ObjCCategoryDecl => Some(Decl::CategoryDef(CategoryDef::from_entity(
                &entity,
                &unnamed_tag_ids,
            ))),
            EntityKind::TypedefDecl => Some(Decl::TypedefDecl(TypedefDecl::from_entity(
                &entity,
                &unnamed_tag_ids,
            ))),
            EntityKind::FunctionDecl => Some(Decl::FuncDecl(FuncDecl::from_entity(
                &entity,
                &unnamed_tag_ids,
            ))),
            EntityKind::StructDecl | EntityKind::UnionDecl => {
                // We only care about definition of structs or unions, not their declaration.
                // (details of unnamed ones are included directly in the types that include them)
                if let Some(def) = entity.get_definition() {
                    if def == entity {
                        Some(Decl::RecordDef(RecordDef::from_entity(
                            &entity,
                            &unnamed_tag_ids,
                        )))
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            EntityKind::EnumDecl => {
                // We only care about definition of enums, not their declaration.
                // But contrarily to struct and enums, we do care about unnamed ones as they are used to declare constants.
                if let Some(def) = entity.get_definition() {
                    if def == entity {
                        Some(Decl::EnumDef(EnumDef::from_entity(
                            &entity,
                            &unnamed_tag_ids,
                        )))
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            _ => None,
        };

        if let Some(decl) = decl {
            decls.push(decl);
            visited.insert(usr.unwrap());
        }

        clang::EntityVisitResult::Recurse
    });
    Ok(decls)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_guess_origin() {
        assert_eq!(Origin::from_path(""), None);
        assert_eq!(
            Origin::from_path("/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.13.sdk/System/Library/Frameworks/Foundation.framework/Headers/NSValue.h"),
            Some(Origin::Framework("Foundation".to_owned())),
        );
        assert_eq!(
            Origin::from_path("/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.13.sdk/System/Library/Frameworks/Metal.framework/Headers/MTLCaptureManager.h"),
            Some(Origin::Framework("Metal".to_owned())),
        );
        assert_eq!(
            Origin::from_path("/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.13.sdk/usr/include/objc/NSObject.h"),
            Some(Origin::ObjCCore),
        );
        assert_eq!(
            Origin::from_path("/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.13.sdk/usr/include/dispatch/object.h"),
            Some(Origin::Library("dispatch".to_owned())),
        );
        assert_eq!(
            Origin::from_path("/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.13.sdk/usr/include/dispatch/queue.h"),
            Some(Origin::Library("dispatch".to_owned())),
        );
        assert_eq!(
            Origin::from_path("/usr/include/i386/_types.h"),
            Some(Origin::System)
        );
        assert_eq!(
            Origin::from_path("/usr/include/_ctype.h"),
            Some(Origin::System)
        );
        assert_eq!(
            Origin::from_path("/usr/include/sys/_types/_wchar_t.h"),
            Some(Origin::System)
        );

        let source = r#"
            # 1 "/System/Library/Frameworks/Foundation.framework/Headers/Foundation.h" 1 3
            typedef int I;
        "#;

        let expected_decls = vec![Decl::TypedefDecl(TypedefDecl {
            name: "I".to_string(),
            underlying: ObjCType::Num(NumKind::Int),
            origin: Some(Origin::Framework("Foundation".to_string())),
        })];

        let parsed_decls = ast_from_str(source).unwrap();
        assert_eq!(parsed_decls, expected_decls);
    }

    #[test]
    fn test_parameter_adopting_protocols() {
        let source = "
            @protocol P1, P2;
            @class B;
            @interface A
            - (void)foo:(id<P1, P2>)x;
            + (void)bar:(B<P2>* _Nonnull)y;
            - (id<P1, P2>)foobar:(B<P2>*)z;
            @end
        ";

        let expected_decls = vec![Decl::InterfaceDef(InterfaceDef {
            name: "A".to_string(),
            superclass: None,
            adopted_protocols: vec![],
            template_params: vec![],
            methods: vec![
                ObjCMethod {
                    name: "foo:".to_string(),
                    kind: ObjCMethodKind::Instance,
                    params: vec![ObjCParam {
                        name: "x".to_string(),
                        objc_type: ObjCType::ObjPtr(ObjPtr {
                            kind: ObjPtrKind::Id(IdObjPtr {
                                protocols: vec!["P1".to_string(), "P2".to_string()],
                            }),
                            nullability: Nullability::Unspecified,
                        }),
                        attrs: ParamAttrs::empty(),
                    }],
                    result: ObjCType::Void,
                    attrs: CallableAttrs::empty(),
                },
                ObjCMethod {
                    name: "bar:".to_string(),
                    kind: ObjCMethodKind::Class,
                    params: vec![ObjCParam {
                        name: "y".to_string(),
                        objc_type: ObjCType::ObjPtr(ObjPtr {
                            kind: ObjPtrKind::SomeInstance(SomeInstanceObjPtr {
                                interface: "B".to_string(),
                                protocols: vec!["P2".to_string()],
                                type_args: vec![],
                            }),
                            nullability: Nullability::NonNull,
                        }),
                        attrs: ParamAttrs::empty(),
                    }],
                    result: ObjCType::Void,
                    attrs: CallableAttrs::empty(),
                },
                ObjCMethod {
                    name: "foobar:".to_string(),
                    kind: ObjCMethodKind::Instance,
                    params: vec![ObjCParam {
                        name: "z".to_string(),
                        objc_type: ObjCType::ObjPtr(ObjPtr {
                            kind: ObjPtrKind::SomeInstance(SomeInstanceObjPtr {
                                interface: "B".to_string(),
                                protocols: vec!["P2".to_string()],
                                type_args: vec![],
                            }),
                            nullability: Nullability::Unspecified,
                        }),
                        attrs: ParamAttrs::empty(),
                    }],
                    result: ObjCType::ObjPtr(ObjPtr {
                        kind: ObjPtrKind::Id(IdObjPtr {
                            protocols: vec!["P1".to_string(), "P2".to_string()],
                        }),
                        nullability: Nullability::Unspecified,
                    }),
                    attrs: CallableAttrs::empty(),
                },
            ],
            properties: vec![],
            origin: None,
        })];

        let parsed_decls = ast_from_str(source).unwrap();
        assert_eq!(parsed_decls, expected_decls);
    }

    #[test]
    fn test_superclass() {
        let source = "
            @interface A
            @end
            @interface B: A
            @end
        ";

        let expected_decls = vec![
            Decl::InterfaceDef(InterfaceDef {
                name: "A".to_string(),
                superclass: None,
                adopted_protocols: vec![],
                template_params: vec![],
                methods: vec![],
                properties: vec![],
                origin: None,
            }),
            Decl::InterfaceDef(InterfaceDef {
                name: "B".to_string(),
                superclass: Some("A".to_string()),
                adopted_protocols: vec![],
                template_params: vec![],
                methods: vec![],
                properties: vec![],
                origin: None,
            }),
        ];

        let parsed_decls = ast_from_str(source).unwrap();
        assert_eq!(parsed_decls, expected_decls);
    }

    #[test]
    fn test_template_params() {
        let source = "
            @interface A
            @end
            @interface B<X, Y, Z>: A
            - (X _Nonnull)x;
            @end
        ";

        let expected_decls = vec![
            Decl::InterfaceDef(InterfaceDef {
                name: "A".to_string(),
                superclass: None,
                adopted_protocols: vec![],
                template_params: vec![],
                methods: vec![],
                properties: vec![],
                origin: None,
            }),
            Decl::InterfaceDef(InterfaceDef {
                name: "B".to_string(),
                superclass: Some("A".to_string()),
                adopted_protocols: vec![],
                template_params: vec!["X".to_string(), "Y".to_string(), "Z".to_string()],
                methods: vec![ObjCMethod {
                    name: "x".to_string(),
                    kind: ObjCMethodKind::Instance,
                    params: vec![],
                    result: ObjCType::ObjPtr(ObjPtr {
                        kind: ObjPtrKind::TypeParam("X".to_string()),
                        nullability: Nullability::NonNull,
                    }),
                    attrs: CallableAttrs::empty(),
                }],
                properties: vec![],
                origin: None,
            }),
        ];

        let parsed_decls = ast_from_str(source).unwrap();
        assert_eq!(parsed_decls, expected_decls);
    }

    #[test]
    fn test_protocol() {
        let source = "
            @protocol B
            @end
            @protocol C, D;
            @protocol A
            - (void)x;
            @optional
            + (void)y;
            - (int)z;
            @end
        ";

        let expected_decls = vec![
            Decl::ProtocolDef(ProtocolDef {
                name: "B".to_string(),
                inherited_protocols: vec![],
                methods: vec![],
                properties: vec![],
                origin: None,
            }),
            Decl::ProtocolDef(ProtocolDef {
                name: "A".to_string(),
                inherited_protocols: vec![],
                methods: vec![
                    ProtocolMethod {
                        method: ObjCMethod {
                            name: "x".to_string(),
                            kind: ObjCMethodKind::Instance,
                            params: vec![],
                            result: ObjCType::Void,
                            attrs: CallableAttrs::empty(),
                        },
                        is_optional: false,
                    },
                    ProtocolMethod {
                        method: ObjCMethod {
                            name: "y".to_string(),
                            kind: ObjCMethodKind::Class,
                            params: vec![],
                            result: ObjCType::Void,
                            attrs: CallableAttrs::empty(),
                        },
                        is_optional: true,
                    },
                    ProtocolMethod {
                        method: ObjCMethod {
                            name: "z".to_string(),
                            kind: ObjCMethodKind::Instance,
                            params: vec![],
                            result: ObjCType::Num(NumKind::Int),
                            attrs: CallableAttrs::empty(),
                        },
                        is_optional: true,
                    },
                ],
                properties: vec![],
                origin: None,
            }),
        ];

        let parsed_decls = ast_from_str(source).unwrap();
        assert_eq!(parsed_decls, expected_decls);
    }

    #[test]
    fn test_category() {
        let source = "
            @protocol P;
            @interface A
            @end
            @interface A (Categ) <P>
            - (void)foo;
            @end
            @interface A ()
            - (void)firstUnnamedCategoryMethod;
            @end
            @interface A ()
            - (void)secondUnnamedCategoryMethod;
            @property int propertyOnUnnamedCategory;
            @end
        ";

        let expected_decls = vec![
            Decl::InterfaceDef(InterfaceDef {
                name: "A".to_string(),
                superclass: None,
                adopted_protocols: vec![],
                template_params: vec![],
                methods: vec![],
                properties: vec![],
                origin: None,
            }),
            Decl::CategoryDef(CategoryDef {
                name: Some("Categ".to_string()),
                class: "A".to_string(),
                adopted_protocols: vec!["P".to_string()],
                methods: vec![ObjCMethod {
                    name: "foo".to_string(),
                    kind: ObjCMethodKind::Instance,
                    params: vec![],
                    result: ObjCType::Void,
                    attrs: CallableAttrs::empty(),
                }],
                properties: vec![],
                origin: None,
            }),
            Decl::CategoryDef(CategoryDef {
                name: None,
                class: "A".to_string(),
                adopted_protocols: vec![],
                methods: vec![ObjCMethod {
                    name: "firstUnnamedCategoryMethod".to_string(),
                    kind: ObjCMethodKind::Instance,
                    params: vec![],
                    result: ObjCType::Void,
                    attrs: CallableAttrs::empty(),
                }],
                properties: vec![],
                origin: None,
            }),
            Decl::CategoryDef(CategoryDef {
                name: None,
                class: "A".to_string(),
                adopted_protocols: vec![],
                methods: vec![ObjCMethod {
                    name: "secondUnnamedCategoryMethod".to_string(),
                    kind: ObjCMethodKind::Instance,
                    params: vec![],
                    result: ObjCType::Void,
                    attrs: CallableAttrs::empty(),
                }],
                properties: vec![Property {
                    name: "propertyOnUnnamedCategory".to_string(),
                    value: ObjCType::Num(NumKind::Int),
                    is_atomic: true,
                    is_writable: false,
                    is_class: false,
                    ownership: PropOwnership::Strong,
                    getter: None,
                    setter: None,
                }],
                origin: None,
            }),
        ];

        let parsed_decls = ast_from_str(source).unwrap();
        assert_eq!(parsed_decls, expected_decls);
    }

    #[test]
    fn test_id() {
        let source = "
            @protocol P;
            @interface A
            - (id)x;
            - (id<P>)y;
            - (id<P> _Nonnull)z;
            @end
        ";

        let expected_decls = vec![Decl::InterfaceDef(InterfaceDef {
            name: "A".to_string(),
            superclass: None,
            adopted_protocols: vec![],
            template_params: vec![],
            methods: vec![
                ObjCMethod {
                    name: "x".to_string(),
                    kind: ObjCMethodKind::Instance,
                    params: vec![],
                    result: ObjCType::ObjPtr(ObjPtr {
                        kind: ObjPtrKind::Id(IdObjPtr { protocols: vec![] }),
                        nullability: Nullability::Unspecified,
                    }),
                    attrs: CallableAttrs::empty(),
                },
                ObjCMethod {
                    name: "y".to_string(),
                    kind: ObjCMethodKind::Instance,
                    params: vec![],
                    result: ObjCType::ObjPtr(ObjPtr {
                        kind: ObjPtrKind::Id(IdObjPtr {
                            protocols: vec!["P".to_string()],
                        }),
                        nullability: Nullability::Unspecified,
                    }),
                    attrs: CallableAttrs::empty(),
                },
                ObjCMethod {
                    name: "z".to_string(),
                    kind: ObjCMethodKind::Instance,
                    params: vec![],
                    result: ObjCType::ObjPtr(ObjPtr {
                        kind: ObjPtrKind::Id(IdObjPtr {
                            protocols: vec!["P".to_string()],
                        }),
                        nullability: Nullability::NonNull,
                    }),
                    attrs: CallableAttrs::empty(),
                },
            ],
            properties: vec![],
            origin: None,
        })];

        let parsed_decls = ast_from_str(source).unwrap();
        assert_eq!(parsed_decls, expected_decls);
    }

    #[test]
    fn test_typedef() {
        let source = "
            typedef int I;
            @interface A
            - (I)foo;
            @end
        ";

        let expected_decls = vec![
            Decl::TypedefDecl(TypedefDecl {
                name: "I".to_string(),
                underlying: ObjCType::Num(NumKind::Int),
                origin: None,
            }),
            Decl::InterfaceDef(InterfaceDef {
                name: "A".to_string(),
                superclass: None,
                adopted_protocols: vec![],
                template_params: vec![],
                methods: vec![ObjCMethod {
                    name: "foo".to_string(),
                    kind: ObjCMethodKind::Instance,
                    params: vec![],
                    result: ObjCType::Typedef(TypedefRef {
                        name: "I".to_string(),
                    }),
                    attrs: CallableAttrs::empty(),
                }],
                properties: vec![],
                origin: None,
            }),
        ];

        let parsed_decls = ast_from_str(source).unwrap();
        assert_eq!(parsed_decls, expected_decls);
    }

    #[test]
    fn test_struct() {
        let source = "
            typedef struct S { int x; } T;
            @interface A
            - (struct S)standardStruct;
            - (struct { float f; union { int i; double d; }; })inlineUnnamedStruct;
            - (T *)pointerToStructTypedef;
            - (struct Undeclared *)pointerToUndeclaredStruct;
            - (struct DeclaredAfterwards *)pointerToStructDeclaredAfterwards;
            @end
            struct DeclaredAfterwards { char c; };
        ";

        let expected_decls = vec![
            Decl::RecordDef(RecordDef {
                id: TagId::Named("S".to_string()),
                fields: vec![Field {
                    name: Some("x".to_string()),
                    objc_type: ObjCType::Num(NumKind::Int),
                }],
                kind: RecordKind::Struct,
                origin: None,
            }),
            Decl::TypedefDecl(TypedefDecl {
                name: "T".to_string(),
                underlying: ObjCType::Tag(TagRef {
                    id: TagId::Named("S".to_string()),
                    kind: TagKind::Struct,
                }),
                origin: None,
            }),
            Decl::InterfaceDef(InterfaceDef {
                name: "A".to_string(),
                superclass: None,
                adopted_protocols: vec![],
                template_params: vec![],
                methods: vec![
                    ObjCMethod {
                        name: "standardStruct".to_string(),
                        kind: ObjCMethodKind::Instance,
                        params: vec![],
                        result: ObjCType::Tag(TagRef {
                            id: TagId::Named("S".to_string()),
                            kind: TagKind::Struct,
                        }),
                        attrs: CallableAttrs::empty(),
                    },
                    ObjCMethod {
                        name: "inlineUnnamedStruct".to_string(),
                        kind: ObjCMethodKind::Instance,
                        params: vec![],
                        result: ObjCType::Tag(TagRef {
                            id: TagId::Unnamed(1),
                            kind: TagKind::Struct,
                        }),
                        attrs: CallableAttrs::empty(),
                    },
                    ObjCMethod {
                        name: "pointerToStructTypedef".to_string(),
                        kind: ObjCMethodKind::Instance,
                        params: vec![],
                        result: ObjCType::Pointer(Pointer {
                            pointee: Box::new(ObjCType::Typedef(TypedefRef {
                                name: "T".to_string(),
                            })),
                            nullability: Nullability::Unspecified,
                        }),
                        attrs: CallableAttrs::empty(),
                    },
                    ObjCMethod {
                        name: "pointerToUndeclaredStruct".to_string(),
                        kind: ObjCMethodKind::Instance,
                        params: vec![],
                        result: ObjCType::Pointer(Pointer {
                            pointee: Box::new(ObjCType::Tag(TagRef {
                                id: TagId::Named("Undeclared".to_string()),
                                kind: TagKind::Struct,
                            })),
                            nullability: Nullability::Unspecified,
                        }),
                        attrs: CallableAttrs::empty(),
                    },
                    ObjCMethod {
                        name: "pointerToStructDeclaredAfterwards".to_string(),
                        kind: ObjCMethodKind::Instance,
                        params: vec![],
                        result: ObjCType::Pointer(Pointer {
                            pointee: Box::new(ObjCType::Tag(TagRef {
                                id: TagId::Named("DeclaredAfterwards".to_string()),
                                kind: TagKind::Struct,
                            })),
                            nullability: Nullability::Unspecified,
                        }),
                        attrs: CallableAttrs::empty(),
                    },
                ],
                properties: vec![],
                origin: None,
            }),
            Decl::RecordDef(RecordDef {
                id: TagId::Unnamed(1),
                kind: RecordKind::Struct,
                fields: vec![
                    Field {
                        name: Some("f".to_string()),
                        objc_type: ObjCType::Num(NumKind::Float),
                    },
                    Field {
                        name: None,
                        objc_type: ObjCType::Tag(TagRef {
                            id: TagId::Unnamed(2),
                            kind: TagKind::Union,
                        }),
                    },
                ],
                origin: None,
            }),
            Decl::RecordDef(RecordDef {
                id: TagId::Unnamed(2),
                kind: RecordKind::Union,
                fields: vec![
                    Field {
                        name: Some("i".to_string()),
                        objc_type: ObjCType::Num(NumKind::Int),
                    },
                    Field {
                        name: Some("d".to_string()),
                        objc_type: ObjCType::Num(NumKind::Double),
                    },
                ],
                origin: None,
            }),
            Decl::RecordDef(RecordDef {
                id: TagId::Named("DeclaredAfterwards".to_string()),
                fields: vec![Field {
                    name: Some("c".to_string()),
                    objc_type: ObjCType::Num(NumKind::SChar),
                }],
                kind: RecordKind::Struct,
                origin: None,
            }),
        ];

        let parsed_decls = ast_from_str(source).unwrap();
        assert_eq!(parsed_decls, expected_decls);
    }

    #[test]
    fn test_function_pointers() {
        let source = "
            typedef void (*T)(int typedefParam);
            @interface A
            - (int(*)())returningFunctionPointerWithUndefinedParameters;
            - (int(*)(float, ...))returningFunctionPointerVariadic;
            - (int(*)(void))returningFunctionPointerWithNoParameters;
            - (T)returningFunctionPointerTypedef;
            - (char (*(*)(double innerParam))(float outerParam))returningFunctionPointerReturningFunctionPointer;
            - (A *(*)(short returnedFunctionParameter))takingTypedef:(T)typedefParam andFunctionPointersTakingFunctionPointers:(A *(*)(float someFloat, int (*functionPointerParam)(char someChar)))complicatedParam;
            @end
        ";

        let expected_decls = vec![
            Decl::TypedefDecl(TypedefDecl {
                name: "T".to_string(),
                underlying: ObjCType::Pointer(Pointer {
                    pointee: Box::new(ObjCType::Function(CallableDesc {
                        result: Box::new(ObjCType::Void),
                        params: Some(vec![Param {
                            name: Some("typedefParam".to_string()),
                            objc_type: ObjCType::Num(NumKind::Int),
                            attrs: ParamAttrs::empty(),
                        }]),
                        is_variadic: false,
                        attrs: CallableAttrs::empty(),
                    })),
                    nullability: Nullability::Unspecified,
                }),
                origin: None,
            }),
            Decl::InterfaceDef(InterfaceDef {
                name: "A".to_string(),
                superclass: None,
                adopted_protocols: vec![],
                template_params: vec![],
                methods: vec![
                    // - (int(*)())returningFunctionPointerWithUndefinedParameters;
                    ObjCMethod {
                        name: "returningFunctionPointerWithUndefinedParameters".to_string(),
                        kind: ObjCMethodKind::Instance,
                        params: vec![],
                        result: ObjCType::Pointer(Pointer {
                            pointee: Box::new(ObjCType::Function(CallableDesc {
                                result: Box::new(ObjCType::Num(NumKind::Int)),
                                params: None,
                                is_variadic: true,
                                attrs: CallableAttrs::empty(),
                            })),
                            nullability: Nullability::Unspecified,
                        }),
                        attrs: CallableAttrs::empty(),
                    },
                    // - (int(*)(float, ...))returningFunctionPointerVariadic;
                    ObjCMethod {
                        name: "returningFunctionPointerVariadic".to_string(),
                        kind: ObjCMethodKind::Instance,
                        params: vec![],
                        result: ObjCType::Pointer(Pointer {
                            pointee: Box::new(ObjCType::Function(CallableDesc {
                                result: Box::new(ObjCType::Num(NumKind::Int)),
                                params: Some(vec![Param {
                                    name: None,
                                    objc_type: ObjCType::Num(NumKind::Float),
                                    attrs: ParamAttrs::empty(),
                                }]),
                                is_variadic: true,
                                attrs: CallableAttrs::empty(),
                            })),
                            nullability: Nullability::Unspecified,
                        }),
                        attrs: CallableAttrs::empty(),
                    },
                    // - (int(*)(void))returningFunctionPointerWithNoParameters;
                    ObjCMethod {
                        name: "returningFunctionPointerWithNoParameters".to_string(),
                        kind: ObjCMethodKind::Instance,
                        params: vec![],
                        result: ObjCType::Pointer(Pointer {
                            pointee: Box::new(ObjCType::Function(CallableDesc {
                                result: Box::new(ObjCType::Num(NumKind::Int)),
                                params: Some(vec![]),
                                is_variadic: false,
                                attrs: CallableAttrs::empty(),
                            })),
                            nullability: Nullability::Unspecified,
                        }),
                        attrs: CallableAttrs::empty(),
                    },
                    // - (T)returningFunctionPointerTypedef;
                    ObjCMethod {
                        name: "returningFunctionPointerTypedef".to_string(),
                        kind: ObjCMethodKind::Instance,
                        params: vec![],
                        result: ObjCType::Typedef(TypedefRef {
                            name: "T".to_string(),
                        }),
                        attrs: CallableAttrs::empty(),
                    },
                    // - (char (*(*)(double innerParam))(float outerParam))returningFunctionPointerReturningFunctionPointer;
                    ObjCMethod {
                        name: "returningFunctionPointerReturningFunctionPointer".to_string(),
                        kind: ObjCMethodKind::Instance,
                        params: vec![],
                        result: ObjCType::Pointer(Pointer {
                            pointee: Box::new(ObjCType::Function(CallableDesc {
                                result: Box::new(ObjCType::Pointer(Pointer {
                                    pointee: Box::new(ObjCType::Function(CallableDesc {
                                        result: Box::new(ObjCType::Num(NumKind::SChar)),
                                        params: Some(vec![Param {
                                            name: Some("outerParam".to_string()),
                                            objc_type: ObjCType::Num(NumKind::Float),
                                            attrs: ParamAttrs::empty(),
                                        }]),
                                        is_variadic: false,
                                        attrs: CallableAttrs::empty(),
                                    })),
                                    nullability: Nullability::Unspecified,
                                })),
                                params: Some(vec![Param {
                                    name: Some("innerParam".to_string()),
                                    objc_type: ObjCType::Num(NumKind::Double),
                                    attrs: ParamAttrs::empty(),
                                }]),
                                is_variadic: false,
                                attrs: CallableAttrs::empty(),
                            })),
                            nullability: Nullability::Unspecified,
                        }),
                        attrs: CallableAttrs::empty(),
                    },
                    // - (A *(*)(short returnedFunctionParameter))takingTypedef:(T)typedefParam andFunctionPointersTakingFunctionPointers:(A *(*)(float someFloat, int (*functionPointerParam)(char someChar)))complicatedParam;
                    ObjCMethod {
                        name: "takingTypedef:andFunctionPointersTakingFunctionPointers:"
                            .to_string(),
                        kind: ObjCMethodKind::Instance,
                        params: vec![
                            ObjCParam {
                                name: "typedefParam".to_string(),
                                objc_type: ObjCType::Typedef(TypedefRef {
                                    name: "T".to_string(),
                                }),
                                attrs: ParamAttrs::empty(),
                            },
                            ObjCParam {
                                name: "complicatedParam".to_string(),
                                objc_type: ObjCType::Pointer(Pointer {
                                    pointee: Box::new(ObjCType::Function(CallableDesc {
                                        result: Box::new(ObjCType::ObjPtr(ObjPtr {
                                            kind: ObjPtrKind::SomeInstance(SomeInstanceObjPtr {
                                                interface: "A".to_string(),
                                                protocols: vec![],
                                                type_args: vec![],
                                            }),
                                            nullability: Nullability::Unspecified,
                                        })),
                                        params: Some(vec![
                                            Param {
                                                name: Some("someFloat".to_string()),
                                                objc_type: ObjCType::Num(NumKind::Float),
                                                attrs: ParamAttrs::empty(),
                                            },
                                            Param {
                                                name: Some("functionPointerParam".to_string()),
                                                objc_type: ObjCType::Pointer(Pointer {
                                                    pointee: Box::new(ObjCType::Function(
                                                        CallableDesc {
                                                            result: Box::new(ObjCType::Num(
                                                                NumKind::Int,
                                                            )),
                                                            params: Some(vec![Param {
                                                                name: Some("someChar".to_string()),
                                                                objc_type: ObjCType::Num(
                                                                    NumKind::SChar,
                                                                ),
                                                                attrs: ParamAttrs::empty(),
                                                            }]),
                                                            is_variadic: false,
                                                            attrs: CallableAttrs::empty(),
                                                        },
                                                    )),
                                                    nullability: Nullability::Unspecified,
                                                }),
                                                attrs: ParamAttrs::empty(),
                                            },
                                        ]),
                                        is_variadic: false,
                                        attrs: CallableAttrs::empty(),
                                    })),
                                    nullability: Nullability::Unspecified,
                                }),
                                attrs: ParamAttrs::empty(),
                            },
                        ],
                        result: ObjCType::Pointer(Pointer {
                            pointee: Box::new(ObjCType::Function(CallableDesc {
                                result: Box::new(ObjCType::ObjPtr(ObjPtr {
                                    kind: ObjPtrKind::SomeInstance(SomeInstanceObjPtr {
                                        interface: "A".to_string(),
                                        protocols: vec![],
                                        type_args: vec![],
                                    }),
                                    nullability: Nullability::Unspecified,
                                })),
                                params: Some(vec![Param {
                                    name: Some("returnedFunctionParameter".to_string()),
                                    objc_type: ObjCType::Num(NumKind::Short),
                                    attrs: ParamAttrs::empty(),
                                }]),
                                is_variadic: false,
                                attrs: CallableAttrs::empty(),
                            })),
                            nullability: Nullability::Unspecified,
                        }),
                        attrs: CallableAttrs::empty(),
                    },
                ],
                properties: vec![],
                origin: None,
            }),
        ];

        let parsed_decls = ast_from_str(source).unwrap();
        assert_eq!(parsed_decls, expected_decls);
    }

    #[test]
    fn test_method_for_selector() {
        // Taken from system headers
        let source = "
            typedef struct objc_class *Class;
            struct objc_object {
                Class _Nonnull isa __attribute__((deprecated));
            };
            typedef struct objc_object *id;
            typedef struct objc_selector *SEL;
            typedef id _Nullable (*IMP)(id _Nonnull, SEL _Nonnull, ...);

            @protocol P
            - (IMP)methodForSelector:(SEL)aSelector;
            @end
        ";

        let expected_decls = vec![
            Decl::TypedefDecl(TypedefDecl {
                name: "Class".to_string(),
                underlying: ObjCType::Pointer(Pointer {
                    pointee: Box::new(ObjCType::Tag(TagRef {
                        id: TagId::Named("objc_class".to_string()),
                        kind: TagKind::Struct,
                    })),
                    nullability: Nullability::Unspecified,
                }),
                origin: None,
            }),
            Decl::RecordDef(RecordDef {
                id: TagId::Named("objc_object".to_string()),
                fields: vec![Field {
                    name: Some("isa".to_string()),
                    objc_type: ObjCType::ObjPtr(ObjPtr {
                        kind: ObjPtrKind::Class,
                        nullability: Nullability::NonNull,
                    }),
                }],
                kind: RecordKind::Struct,
                origin: None,
            }),
            Decl::TypedefDecl(TypedefDecl {
                name: "id".to_string(),
                underlying: ObjCType::Pointer(Pointer {
                    pointee: Box::new(ObjCType::Tag(TagRef {
                        id: TagId::Named("objc_object".to_string()),
                        kind: TagKind::Struct,
                    })),
                    nullability: Nullability::Unspecified,
                }),
                origin: None,
            }),
            Decl::TypedefDecl(TypedefDecl {
                name: "SEL".to_string(),
                underlying: ObjCType::Pointer(Pointer {
                    pointee: Box::new(ObjCType::Tag(TagRef {
                        id: TagId::Named("objc_selector".to_string()),
                        kind: TagKind::Struct,
                    })),
                    nullability: Nullability::Unspecified,
                }),
                origin: None,
            }),
            Decl::TypedefDecl(TypedefDecl {
                name: "IMP".to_string(),
                underlying: ObjCType::Pointer(Pointer {
                    pointee: Box::new(ObjCType::Function(CallableDesc {
                        result: Box::new(ObjCType::ObjPtr(ObjPtr {
                            kind: ObjPtrKind::Id(IdObjPtr { protocols: vec![] }),
                            nullability: Nullability::Nullable,
                        })),
                        params: Some(vec![
                            Param {
                                name: None,
                                objc_type: ObjCType::ObjPtr(ObjPtr {
                                    kind: ObjPtrKind::Id(IdObjPtr { protocols: vec![] }),
                                    nullability: Nullability::NonNull,
                                }),
                                attrs: ParamAttrs::empty(),
                            },
                            Param {
                                name: None,
                                objc_type: ObjCType::ObjCSel(Nullability::NonNull),
                                attrs: ParamAttrs::empty(),
                            },
                        ]),
                        is_variadic: true,
                        attrs: CallableAttrs::empty(),
                    })),
                    nullability: Nullability::Unspecified,
                }),
                origin: None,
            }),
            Decl::ProtocolDef(ProtocolDef {
                name: "P".to_string(),
                inherited_protocols: vec![],
                methods: vec![ProtocolMethod {
                    method: ObjCMethod {
                        name: "methodForSelector:".to_string(),
                        kind: ObjCMethodKind::Instance,
                        params: vec![ObjCParam {
                            name: "aSelector".to_string(),
                            objc_type: ObjCType::ObjCSel(Nullability::Unspecified),
                            attrs: ParamAttrs::empty(),
                        }],
                        result: ObjCType::Typedef(TypedefRef {
                            name: "IMP".to_string(),
                        }),
                        attrs: CallableAttrs::empty(),
                    },
                    is_optional: false,
                }],
                properties: vec![],
                origin: None,
            }),
        ];

        let parsed_decls = ast_from_str(source).unwrap();
        assert_eq!(parsed_decls, expected_decls);
    }

    #[test]
    fn test_nslog() {
        // NSLog seems to be already known by the compiler so handled a bit differently by it.
        let source = "
            @class NSString;
            extern void NSLog(NSString *format, ...);
        ";

        let expected_decls = vec![Decl::FuncDecl(FuncDecl {
            name: "NSLog".to_string(),
            desc: CallableDesc {
                result: Box::new(ObjCType::Void),
                params: Some(vec![Param {
                    name: Some("format".to_string()),
                    objc_type: ObjCType::ObjPtr(ObjPtr {
                        kind: ObjPtrKind::SomeInstance(SomeInstanceObjPtr {
                            interface: "NSString".to_string(),
                            protocols: vec![],
                            type_args: vec![],
                        }),
                        nullability: Nullability::Unspecified,
                    }),
                    attrs: ParamAttrs::empty(),
                }]),
                is_variadic: true,
                attrs: CallableAttrs::empty(),
            },
            origin: None,
        })];

        let parsed_decls = ast_from_str(source).unwrap();
        assert_eq!(parsed_decls, expected_decls);
    }

    #[test]
    fn test_param_attributes() {
        let source = "
            @interface I
            - (void)methodWithConsumedParam:(id) __attribute((ns_consumed)) consumedParam;
            - (void)methodWithNoescapeBlock:(void (__attribute__((noescape)) ^)(void))block;
            @end
            void function_with_consumed_param(__attribute((ns_consumed)) id consumedParam);
            void function_with_noescape_block(void (__attribute__((noescape)) ^)(void));
        ";

        let expected_decls = vec![
            Decl::InterfaceDef(InterfaceDef {
                name: "I".to_string(),
                superclass: None,
                adopted_protocols: vec![],
                template_params: vec![],
                methods: vec![
                    ObjCMethod {
                        name: "methodWithConsumedParam:".to_string(),
                        kind: ObjCMethodKind::Instance,
                        params: vec![ObjCParam {
                            name: "consumedParam".to_string(),
                            objc_type: ObjCType::ObjPtr(ObjPtr {
                                kind: ObjPtrKind::Id(IdObjPtr { protocols: vec![] }),
                                nullability: Nullability::Unspecified,
                            }),
                            attrs: ParamAttrs::CONSUMED,
                        }],
                        result: ObjCType::Void,
                        attrs: CallableAttrs::empty(),
                    },
                    ObjCMethod {
                        name: "methodWithNoescapeBlock:".to_string(),
                        kind: ObjCMethodKind::Instance,
                        params: vec![ObjCParam {
                            name: "block".to_string(),
                            objc_type: ObjCType::ObjPtr(ObjPtr {
                                kind: ObjPtrKind::Block(CallableDesc {
                                    result: Box::new(ObjCType::Void),
                                    params: Some(vec![]),
                                    is_variadic: false,
                                    attrs: CallableAttrs::empty(),
                                }),
                                nullability: Nullability::Unspecified,
                            }),
                            attrs: ParamAttrs::NOESCAPE,
                        }],
                        result: ObjCType::Void,
                        attrs: CallableAttrs::empty(),
                    },
                ],
                properties: vec![],
                origin: None,
            }),
            Decl::FuncDecl(FuncDecl {
                name: "function_with_consumed_param".to_string(),
                desc: CallableDesc {
                    result: Box::new(ObjCType::Void),
                    params: Some(vec![Param {
                        name: Some("consumedParam".to_string()),
                        objc_type: ObjCType::ObjPtr(ObjPtr {
                            kind: ObjPtrKind::Id(IdObjPtr { protocols: vec![] }),
                            nullability: Nullability::Unspecified,
                        }),
                        attrs: ParamAttrs::CONSUMED,
                    }]),
                    is_variadic: false,
                    attrs: CallableAttrs::empty(),
                },
                origin: None,
            }),
            Decl::FuncDecl(FuncDecl {
                name: "function_with_noescape_block".to_string(),
                desc: CallableDesc {
                    result: Box::new(ObjCType::Void),
                    params: Some(vec![Param {
                        name: None,
                        objc_type: ObjCType::ObjPtr(ObjPtr {
                            kind: ObjPtrKind::Block(CallableDesc {
                                result: Box::new(ObjCType::Void),
                                params: Some(vec![]),
                                is_variadic: false,
                                attrs: CallableAttrs::empty(),
                            }),
                            nullability: Nullability::Unspecified,
                        }),
                        attrs: ParamAttrs::NOESCAPE,
                    }]),
                    is_variadic: false,
                    attrs: CallableAttrs::empty(),
                },
                origin: None,
            }),
        ];

        let parsed_decls = ast_from_str(source).unwrap();
        assert_eq!(parsed_decls, expected_decls);
    }

    #[test]
    fn test_return_attributes() {
        // NSLog seems to be already known by the compiler so handled a bit differently by it.
        let source = "
            @interface I
            - (id)methodWithRetainedReturn __attribute__((__ns_returns_retained__));
            @end
            __attribute__((ns_returns_retained)) id function_with_retained_return(void);
        ";

        let expected_decls = vec![
            Decl::InterfaceDef(InterfaceDef {
                name: "I".to_string(),
                superclass: None,
                adopted_protocols: vec![],
                template_params: vec![],
                methods: vec![ObjCMethod {
                    name: "methodWithRetainedReturn".to_string(),
                    kind: ObjCMethodKind::Instance,
                    params: vec![],
                    result: ObjCType::ObjPtr(ObjPtr {
                        kind: ObjPtrKind::Id(IdObjPtr { protocols: vec![] }),
                        nullability: Nullability::Unspecified,
                    }),
                    attrs: CallableAttrs::RETURNS_RETAINED,
                }],
                properties: vec![],
                origin: None,
            }),
            Decl::FuncDecl(FuncDecl {
                name: "function_with_retained_return".to_string(),
                desc: CallableDesc {
                    result: Box::new(ObjCType::ObjPtr(ObjPtr {
                        kind: ObjPtrKind::Id(IdObjPtr { protocols: vec![] }),
                        nullability: Nullability::Unspecified,
                    })),
                    params: Some(vec![]),
                    is_variadic: false,
                    attrs: CallableAttrs::RETURNS_RETAINED,
                },
                origin: None,
            }),
        ];

        let parsed_decls = ast_from_str(source).unwrap();
        assert_eq!(parsed_decls, expected_decls);
    }
}
