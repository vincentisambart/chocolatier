use crate::clang::{self, CursorKind, TypeKind};
use std::collections::{HashMap, HashSet};

// TODO: Try to get:
// - class and method OS version annotations
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
pub enum Origin {
    ObjCCore,
    System,
    Framework(String),
    Library(String),
}

impl Origin {
    fn from_path(path: &str) -> Option<Self> {
        use once_cell::sync::Lazy;
        use regex::Regex;

        static FRAMEWORK_PATH_RE: Lazy<Regex> =
            Lazy::new(|| Regex::new(r"/([^./]+)\.framework/Headers/.+.h\z").unwrap());

        if let Some(caps) = FRAMEWORK_PATH_RE.captures(path) {
            let framework = caps.get(1).unwrap().as_str().to_owned();
            return Some(Origin::Framework(framework));
        }

        if path.contains("/usr/include/") {
            static LIBRARY_PATH_RE: Lazy<Regex> =
                Lazy::new(|| Regex::new(r"/usr/include/([^./]+)/.+.h\z").unwrap());
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

    fn from_cursor(cursor: &clang::Cursor) -> Option<Origin> {
        // As preprocess the file in advance using the system compiler, we have to use presumed_location().
        let path = cursor.location()?.presumed_location().file;
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

    if let Some(argument_types) = clang_type.argument_types() {
        if argument_types.len() > 0 {
            println!("{}{} arguments types:", indent, desc);
            for (i, arg_type) in argument_types.enumerate() {
                let arg_desc = format!("{} argument type {}", desc, i);
                show_type(&arg_desc, &arg_type, indent_level);
            }
        }
    }

    if let Some(base_type) = clang_type.objc_object_base_type() {
        println!("{}{} ObjC base type types: {:?}", indent, desc, base_type);
    }

    let canonical_type = clang_type.canonical_type();
    if &canonical_type != clang_type {
        println!("{}{} canonical type: {:?}", indent, desc, canonical_type);
    }

    if let Some(nullability) = clang_type.nullability() {
        println!("{}{} nullability: {:?}", indent, desc, nullability);
    }

    if let Some(class_type) = clang_type.class_type() {
        println!("{}{} class type: {:?}", indent, desc, class_type);
    }

    let objc_protocol_decls = clang_type.objc_protocol_decls();
    if objc_protocol_decls.len() > 0 {
        println!(
            "{}{} objc protocol declarations: {:?}",
            indent,
            desc,
            objc_protocol_decls.collect::<Vec<_>>()
        );
    }
    let objc_type_arg_types = clang_type.objc_type_arg_types();
    if objc_type_arg_types.len() > 0 {
        println!(
            "{}{} objc type arguments: {:?}",
            indent,
            desc,
            objc_type_arg_types.collect::<Vec<_>>()
        );
    }

    if let Some(pointee_type) = clang_type.pointee_type() {
        let pointee_desc = format!("{} pointee", desc);
        show_type(&pointee_desc, &pointee_type, indent_level);
    }

    if let Some(elaborated_type) = clang_type.named_type() {
        let pointee_desc = format!("{} elaborated type", desc);
        show_type(&pointee_desc, &elaborated_type, indent_level);
    }

    if let Some(modified_type) = clang_type.modified_type() {
        let modified_desc = format!("{} modified", desc);
        show_type(&modified_desc, &modified_type, indent_level);
    }

    if let Some(decl) = clang_type.declaration() {
        println!("{}{} decl: {:?}", indent, desc, decl);
    }

    if let Some(alignment) = clang_type.align_of() {
        println!("{}alignment: {:?}", indent, alignment);
    }

    if let Some(size) = clang_type.size_of() {
        println!("{}size: {:?}", indent, size);
    }

    if let Some(template_arguments) = clang_type.template_arguments() {
        println!(
            "{}{} template arguments: {:?}",
            indent,
            desc,
            template_arguments.collect::<Vec<_>>()
        );
    }

    if let Some(result_type) = clang_type.result_type() {
        let result_type_desc = format!("{} result type", desc);
        show_type(&result_type_desc, &result_type, indent_level);
    }

    if let Some(fields) = clang_type.fields() {
        println!("{}{} fields:", indent, desc);
        for field in fields {
            show_tree(&field, indent_level + 1);
        }
    }
}

fn show_tree(cursor: &clang::Cursor, indent_level: usize) {
    let indent = (0..indent_level)
        .map(|_| "   ")
        .collect::<Vec<&str>>()
        .concat();

    if let Some(name) = cursor.spelling() {
        println!("{}*** kind: {:?} - {} ***", indent, cursor.kind(), name);
    } else {
        println!("{}*** kind: {:?} ***", indent, cursor.kind());
    }
    if cursor.display_name() != cursor.spelling() {
        if let Some(display_name) = cursor.display_name() {
            println!("{}display name: {:?}", indent, display_name);
        }
    }

    if [
        CursorKind::ObjCInstanceMethodDecl,
        CursorKind::ObjCClassMethodDecl,
        CursorKind::ObjCPropertyDecl,
    ]
    .contains(&cursor.kind())
    {
        println!("{}objc optional: {:?}", indent, cursor.is_objc_optional());
    }

    if let Some(location) = cursor.location() {
        println!("{}location: {:?}", indent, location);
    }

    if let Some(range) = cursor.extent() {
        println!("{}range: {:?}", indent, range);

        if cursor.kind() == CursorKind::UnexposedAttr {
            println!("{}tokens: {:?}", indent, range.tokenize());
        }
    }

    if let Some(usr) = cursor.usr() {
        println!("{}usr: {}", indent, usr);
    }

    if let Some(arguments) = cursor.arguments() {
        if arguments.len() > 0 {
            println!("{}arguments ({}):", indent, arguments.len());
            for arg in arguments {
                show_tree(&arg, indent_level + 1);
            }
        }
    }

    let availability = cursor.availability();
    if availability != clang::Availability::Available {
        println!("{}availability: {:?}", indent, availability);
    }

    let canonical_cursor = cursor.canonical_cursor();
    if &canonical_cursor != cursor {
        println!("{}canonical cursor: {:?}", indent, canonical_cursor);
    }

    if let Some(definition) = cursor.definition() {
        println!("{}definition: {:?}", indent, definition);
    }

    if let Some(template) = cursor.template() {
        println!("{}template: {:?}", indent, template);
    }

    if let Some(template_kind) = cursor.template_kind() {
        println!("{}template kind: {:?}", indent, template_kind);
    }

    if let Some(clang_type) = cursor.type_() {
        show_type("type", &clang_type, indent_level);
    }

    if let Some(enum_underlying_type) = cursor.enum_decl_int_type() {
        show_type("enum underlying type", &enum_underlying_type, indent_level);
    }

    if let Some(typedef_underlying_type) = cursor.typedef_decl_underlying_type() {
        show_type(
            "typedef underlying type",
            &typedef_underlying_type,
            indent_level,
        );
    }

    if let Some(result_type) = cursor.result_type() {
        show_type("result type", &result_type, indent_level);
    }

    if let Some(mangling) = cursor.mangling() {
        println!("{}mangling: {:?}", indent, mangling);
    }

    if let Some(ib_outlet_collection_type) = cursor.ib_outlet_collection_type() {
        println!(
            "{}ib_outlet_collection_type: {:?}",
            indent, ib_outlet_collection_type
        );
    }

    if let Some(objc_type_encoding) = cursor.objc_type_encoding() {
        println!("{}objc type encoding: {:?}", indent, objc_type_encoding);
    }

    if let Some(objc_selector_index) = cursor.objc_selector_index() {
        println!("{}objc selector index: {:?}", indent, objc_selector_index);
    }

    if let Some(objc_attributes) = cursor.objc_attributes() {
        println!("{}objc attributes: {:?}", indent, objc_attributes);
    }

    // Seems to crash...
    // if let Some(objc_receiver_type) = cursor.get_objc_receiver_type() {
    //     println!("{}objc receiver type: {:?}", indent, objc_receiver_type);
    // }

    if let Some(platform_availability) = cursor.platform_availability() {
        println!(
            "{}platform availability: {:?}",
            indent, platform_availability
        );
    }

    if let Some(reference) = cursor.referenced() {
        println!("{}reference: {:?}", indent, reference);
    }

    if let Some(storage_class) = cursor.storage_class() {
        println!("{}storage class: {:?}", indent, storage_class);
    }

    if let Some(offset_of_field) = cursor.offset_of_field() {
        println!("{}offset of field: {:?}", indent, offset_of_field);
    }

    // if let Some(semantic_parent) = cursor.semantic_parent() {
    //     println!("{}semantic parent: {:?}", indent, semantic_parent);
    // }

    // if let Some(lexical_parent) = cursor.get_lexical_parent() {
    //     println!("{}lexical parent: {:?}", indent, lexical_parent);
    // }

    let children = cursor.children();
    if !children.is_empty() {
        println!("{}children:", indent);
        for child in children {
            show_tree(&child, indent_level + 1);
        }
    }
}

pub use clang::PlatformAvailability;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Attr {
    Consumed,
    Noescape,
    ReturnsRetained,
    Unavailable,
    Deprecated,
    PlatformAvailability(Vec<PlatformAvailability>),
    SwiftName(String),
}

impl Attr {
    fn from_decl(decl: &clang::Cursor) -> Vec<Self> {
        let mut attrs: Vec<_> = decl
            .children()
            .iter()
            .filter_map(|child| match child.kind() {
                CursorKind::NSConsumed => Some(Self::Consumed),
                CursorKind::NSReturnsRetained => Some(Self::ReturnsRetained),
                CursorKind::UnexposedAttr => {
                    let extent = child
                        .extent()
                        .expect("could not get extent of unexposed attr");
                    let tokens = extent.tokenize();
                    match tokens[0].spelling().as_str() {
                        "noescape" => {
                            assert!(
                                tokens.len() == 1,
                                "unexpected tokens for \"noescape\" unexposed attr: {:?}",
                                tokens
                            );
                            Some(Self::Noescape)
                        }
                        "unavailable" => {
                            assert!(
                                tokens.len() == 1 || tokens.len() == 4,
                                "unexpected tokens for \"unavailable\" unexposed attr: {:?}",
                                tokens
                            );
                            // There can be a message but for the time being just ignore it.
                            Some(Self::Unavailable)
                        }
                        "swift_name" => {
                            // We expect to have 4 tokens here: "swift_name" "(" name ")"
                            assert!(
                                tokens.len() == 4,
                                "unexpected tokens for \"swift_name\" unexposed attr: {:?}",
                                tokens
                            );
                            let name_token = &tokens[2];
                            assert!(
                                name_token.kind() == clang::TokenKind::Literal,
                                "unexpected tokens for \"swift_name\" unexposed attr: {:?}",
                                tokens
                            );
                            // spelling() returns an escaped string, with '"' at the start and end.
                            // The Swift name should have no other need of escaping, so that should be enough.
                            let spelling = name_token.spelling();
                            let name = spelling.trim_matches('"');
                            assert!(!name.contains('\\'), "unexpected Swift name {:?}", name);
                            Some(Self::SwiftName(name.into()))
                        }
                        _ => None,
                    }
                }
                _ => None,
            })
            .collect();

        if decl.availability() == clang::Availability::Deprecated {
            attrs.push(Self::Deprecated);
        }
        if let Some(availability) = decl.platform_availability() {
            attrs.push(Self::PlatformAvailability(availability));
        }

        attrs
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Nullability {
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
pub struct ObjPtr {
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

fn parm_decl_children<'a>(cursor: &clang::Cursor<'a>) -> impl Iterator<Item = clang::Cursor<'a>> {
    cursor
        .children()
        .into_iter()
        .filter(|child| child.kind() == CursorKind::ParmDecl)
}

#[derive(Clone, Debug, PartialEq)]
struct Field {
    name: Option<String>,
    objc_type: ObjCType,
}

impl Field {
    fn from_cursor(cursor: &clang::Cursor, unnamed_tag_ids: &TagIdMap) -> Self {
        assert_eq!(cursor.kind(), CursorKind::FieldDecl);
        Field {
            name: cursor.spelling(),
            objc_type: ObjCType::from_type(
                &cursor.type_().unwrap(),
                &mut parm_decl_children(cursor),
                unnamed_tag_ids,
            ),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum TagId {
    Named(String),
    Unnamed(u32),
}

impl TagId {
    fn from_cursor(decl: &clang::Cursor, unnamed_tag_ids: &TagIdMap) -> Self {
        assert!([
            CursorKind::StructDecl,
            CursorKind::UnionDecl,
            CursorKind::EnumDecl,
        ]
        .contains(&decl.kind()));

        if let Some(name) = decl.spelling() {
            TagId::Named(name)
        } else {
            TagId::Unnamed(
                *unnamed_tag_ids
                    .get(&decl.usr().unwrap())
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
pub struct TagRef {
    id: TagId,
    kind: TagKind,
}

impl TagRef {
    fn from_type(clang_type: &clang::Type, unnamed_tag_ids: &TagIdMap) -> Self {
        let decl = clang_type.declaration().unwrap();

        let kind = match decl.kind() {
            CursorKind::StructDecl => TagKind::Struct,
            CursorKind::UnionDecl => TagKind::Union,
            CursorKind::EnumDecl => TagKind::Enum,
            unexpected_kind => panic!(
                "Unexpected kind for tag declaration {:?}: {:?}",
                unexpected_kind, clang_type
            ),
        };

        let id = TagId::from_cursor(&decl, unnamed_tag_ids);

        TagRef { id, kind }
    }
}

#[derive(Clone, Debug, PartialEq)]
struct Param {
    name: Option<String>,
    objc_type: ObjCType,
    attrs: Vec<Attr>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct CallableDesc {
    result: Box<ObjCType>,
    params: Option<Vec<Param>>,
    is_variadic: bool,
    attrs: Vec<Attr>,
}

impl CallableDesc {
    fn from_type<'a>(
        clang_type: &clang::Type,
        base_parm_decls: &mut impl Iterator<Item = clang::Cursor<'a>>,
        unnamed_tag_ids: &TagIdMap,
    ) -> Self {
        if clang_type.kind() == TypeKind::Attributed {
            let mut callable = Self::from_type(
                &clang_type.modified_type().unwrap(),
                base_parm_decls,
                unnamed_tag_ids,
            );
            let spelling = clang_type.spelling();
            // Yes, that is very ugly but I could not find a better way.
            if spelling.contains("__attribute__((ns_returns_retained))") {
                callable.attrs.push(Attr::ReturnsRetained);
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
            &clang_type.result_type().unwrap(),
            base_parm_decls,
            unnamed_tag_ids,
        ));

        let params = match clang_type.kind() {
            TypeKind::FunctionNoProto => None,
            TypeKind::FunctionProto => Some({
                clang_type
                    .argument_types()
                    .unwrap()
                    .map(|_| {
                        // Not using the value of argument types, only their count.
                        // The param decl taken from the cursor seems to always have better information.
                        let parm_decl = base_parm_decls.next().unwrap();
                        let objc_type = ObjCType::from_type(
                            &parm_decl.type_().unwrap(),
                            &mut parm_decl_children(&parm_decl),
                            unnamed_tag_ids,
                        );
                        let attrs = Attr::from_decl(&parm_decl);
                        Param {
                            name: parm_decl.spelling(),
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
            is_variadic: clang_type.is_variadic_function(),
            attrs: Vec::new(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypedefRef {
    pub name: String,
}

impl TypedefRef {
    fn from_type(clang_type: &clang::Type) -> Self {
        assert_eq!(clang_type.kind(), TypeKind::Typedef);
        let name = clang_type.spelling();
        TypedefRef { name }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Pointer {
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
pub struct Array {
    size: Option<usize>,
    element: Box<ObjCType>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum SignedOrNotInt {
    Signed(i64),
    Unsigned(u64),
}

impl std::fmt::Display for SignedOrNotInt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SignedOrNotInt::Signed(i) => i.fmt(f),
            SignedOrNotInt::Unsigned(u) => u.fmt(f),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct EnumValue {
    pub name: String,
    pub value: SignedOrNotInt,
    pub attrs: Vec<Attr>,
}

fn type_signedness(clang_type: &clang::Type) -> Option<Signedness> {
    match clang_type.kind() {
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
        TypeKind::Attributed => type_signedness(&clang_type.modified_type().unwrap()),
        TypeKind::Typedef => type_signedness(
            &clang_type
                .declaration()
                .unwrap()
                .typedef_decl_underlying_type()
                .unwrap(),
        ),
        TypeKind::Elaborated => type_signedness(&clang_type.named_type().unwrap()),
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
pub enum NumKind {
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
pub enum Unsupported {
    Vector,
    Unexposed,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ObjCType {
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
        base_parm_decls: &mut impl Iterator<Item = clang::Cursor<'a>>,
        unnamed_tag_ids: &TagIdMap,
    ) -> Self {
        match clang_type.kind() {
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
                let pointee_type = clang_type.pointee_type().unwrap();
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
                    let mut pointee = clang_type.pointee_type().unwrap();
                    loop {
                        match pointee.kind() {
                            // TODO: Check what's the difference between ObjCObject and ObjCInterface
                            TypeKind::ObjCObject | TypeKind::ObjCInterface => break pointee,
                            // Attributed is for example when __kindof is used.
                            TypeKind::Attributed => {
                                pointee = pointee.modified_type().unwrap();
                            }
                            unexpected_kind => {
                                panic!("unexpected pointee kind {:?}", unexpected_kind)
                            }
                        }
                    }
                };
                let protocols = pointee_type
                    .objc_protocol_decls()
                    .map(|decl| {
                        assert_eq!(decl.kind(), CursorKind::ObjCProtocolDecl);
                        decl.spelling().unwrap()
                    })
                    .collect();

                let type_args: Vec<ObjCTypeArg> = pointee_type
                    .objc_type_arg_types()
                    .map(|arg| {
                        match Self::from_type(&arg, &mut Vec::new().into_iter(), unnamed_tag_ids) {
                            Self::ObjPtr(ptr) => ObjCTypeArg::ObjPtr(ptr),
                            Self::Typedef(typedef) => ObjCTypeArg::Typedef(typedef),
                            unexpected => {
                                panic!("Type arguments not expected to be {:?}", unexpected)
                            }
                        }
                    })
                    .collect();

                let base_type = pointee_type.objc_object_base_type().unwrap();

                if base_type.kind() == TypeKind::ObjCId {
                    assert!(type_args.is_empty());
                    assert_eq!(base_type.spelling(), "id");
                    Self::ObjPtr(ObjPtr {
                        kind: ObjPtrKind::Id(IdObjPtr { protocols }),
                        nullability: Nullability::Unspecified,
                    })
                } else {
                    Self::ObjPtr(ObjPtr {
                        kind: ObjPtrKind::SomeInstance(SomeInstanceObjPtr {
                            interface: base_type.spelling(),
                            protocols,
                            type_args,
                        }),
                        nullability: Nullability::Unspecified,
                    })
                }
            }
            TypeKind::Attributed => {
                let modified = Self::from_type(
                    &clang_type.modified_type().unwrap(),
                    base_parm_decls,
                    unnamed_tag_ids,
                );
                if let Some(nullability) = clang_type.nullability() {
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
                kind: ObjPtrKind::TypeParam(clang_type.spelling()),
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
                let decl = clang_type.declaration().unwrap();
                if decl.kind() == CursorKind::TemplateTypeParameter {
                    Self::ObjPtr(ObjPtr {
                        kind: ObjPtrKind::TypeParam(clang_type.spelling()),
                        nullability: Nullability::Unspecified,
                    })
                } else {
                    Self::Typedef(TypedefRef::from_type(&clang_type))
                }
            }
            TypeKind::Elaborated => Self::from_type(
                &clang_type.named_type().unwrap(),
                base_parm_decls,
                unnamed_tag_ids,
            ),
            TypeKind::Record | TypeKind::Enum => {
                Self::Tag(TagRef::from_type(&clang_type, unnamed_tag_ids))
            }
            TypeKind::FunctionNoProto | TypeKind::FunctionProto => Self::Function(
                CallableDesc::from_type(&clang_type, base_parm_decls, unnamed_tag_ids),
            ),
            TypeKind::ConstantArray => Self::Array(Array {
                size: Some(clang_type.num_elements().unwrap()),
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
                    &clang_type.pointee_type().unwrap(),
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
    fn from_cursor(cursor: &clang::Cursor) -> Self {
        let source_location = cursor.location().unwrap();
        // let location = source_location.get_file_location();
        let location = source_location.spelling_location();

        let file_kind = if let Some(file) = location.file {
            // For some reason, source_location.is_in_main_file() doesn't seem to work properly so do it ourselves.
            let tu_file = cursor
                .tu()
                .cursor()
                .extent()
                .unwrap()
                .start()
                .file_location()
                .file
                .unwrap();
            if tu_file == file {
                FileKind::Main
            } else {
                FileKind::Some(file.file_name())
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
pub struct ObjCParam {
    pub name: String,
    pub objc_type: ObjCType,
    pub attrs: Vec<Attr>,
}

impl ObjCParam {
    fn from_cursor(decl: &clang::Cursor, unnamed_tag_ids: &TagIdMap) -> Self {
        assert_eq!(decl.kind(), CursorKind::ParmDecl);
        let name = decl.spelling().unwrap();
        let objc_type = ObjCType::from_type(
            &decl.type_().unwrap(),
            &mut parm_decl_children(decl),
            unnamed_tag_ids,
        );
        let attrs = Attr::from_decl(&decl);
        ObjCParam {
            name,
            objc_type,
            attrs,
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ObjCMethodKind {
    Class,
    Instance,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ObjCMethod {
    pub name: String,
    pub kind: ObjCMethodKind,
    pub params: Vec<ObjCParam>,
    pub result: ObjCType,
    pub attrs: Vec<Attr>,
}

impl ObjCMethod {
    fn from_cursor(cursor: &clang::Cursor, unnamed_tag_ids: &TagIdMap) -> Self {
        let kind = match cursor.kind() {
            CursorKind::ObjCClassMethodDecl => ObjCMethodKind::Class,
            CursorKind::ObjCInstanceMethodDecl => ObjCMethodKind::Instance,
            _ => panic!("cursor should be either a class or instance method"),
        };

        let params = cursor
            .arguments()
            .unwrap()
            .map(|arg| ObjCParam::from_cursor(&arg, unnamed_tag_ids))
            .collect();

        let result = ObjCType::from_type(
            &cursor.result_type().unwrap(),
            &mut parm_decl_children(cursor),
            unnamed_tag_ids,
        );

        let attrs = Attr::from_decl(cursor);

        ObjCMethod {
            name: cursor.spelling().unwrap(),
            kind,
            params,
            result,
            attrs,
        }
    }
}

fn is_generated_from_property(method_cursor: &clang::Cursor) -> bool {
    assert!([
        CursorKind::ObjCInstanceMethodDecl,
        CursorKind::ObjCClassMethodDecl,
    ]
    .contains(&method_cursor.kind()));
    let parent = method_cursor.semantic_parent().unwrap();
    assert!([
        CursorKind::ObjCInterfaceDecl,
        CursorKind::ObjCProtocolDecl,
        CursorKind::ObjCCategoryDecl,
    ]
    .contains(&parent.kind()));
    let method_location = method_cursor.location().unwrap();
    parent
        .children()
        .into_iter()
        .filter(|sibling| sibling.kind() == CursorKind::ObjCPropertyDecl)
        .any(|sibling| sibling.location().unwrap() == method_location)
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum PropOwnership {
    Strong,
    Weak,
    Copy,
    Assign,
    Retain,
    UnsafeUnretained,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Property {
    pub name: String,
    pub value: ObjCType,
    pub is_atomic: bool,
    pub is_writable: bool,
    pub is_class: bool,
    pub ownership: PropOwnership,
    pub getter: Option<String>,
    pub setter: Option<String>,
}

impl Property {
    fn from_cursor(cursor: &clang::Cursor, unnamed_tag_ids: &TagIdMap) -> Self {
        assert_eq!(cursor.kind(), CursorKind::ObjCPropertyDecl);

        let name = cursor.spelling().unwrap();
        let value = ObjCType::from_type(
            &cursor.type_().unwrap(),
            &mut parm_decl_children(&cursor),
            unnamed_tag_ids,
        );

        use clang::ObjCAttributes;
        let attributes = cursor
            .objc_attributes()
            .unwrap_or_else(|| ObjCAttributes::empty());
        let is_atomic = {
            if attributes.contains(ObjCAttributes::ATOMIC) {
                assert!(!attributes.contains(ObjCAttributes::NONATOMIC));
                true
            } else {
                !attributes.contains(ObjCAttributes::NONATOMIC)
            }
        };
        let is_writable = attributes.contains(ObjCAttributes::READWRITE);
        if is_writable {
            assert!(!attributes.contains(ObjCAttributes::READONLY));
        }
        let is_class = attributes.contains(ObjCAttributes::CLASS);
        let ownership = {
            if attributes.contains(ObjCAttributes::STRONG) {
                assert!(!attributes.contains(ObjCAttributes::WEAK));
                assert!(!attributes.contains(ObjCAttributes::COPY));
                assert!(!attributes.contains(ObjCAttributes::ASSIGN));
                assert!(!attributes.contains(ObjCAttributes::RETAIN));
                assert!(!attributes.contains(ObjCAttributes::UNSAFE_UNRETAINED));
                PropOwnership::Strong
            } else if attributes.contains(ObjCAttributes::WEAK) {
                assert!(!attributes.contains(ObjCAttributes::COPY));
                assert!(!attributes.contains(ObjCAttributes::ASSIGN));
                assert!(!attributes.contains(ObjCAttributes::RETAIN));
                assert!(!attributes.contains(ObjCAttributes::UNSAFE_UNRETAINED));
                PropOwnership::Weak
            } else if attributes.contains(ObjCAttributes::COPY) {
                assert!(!attributes.contains(ObjCAttributes::ASSIGN));
                assert!(!attributes.contains(ObjCAttributes::RETAIN));
                assert!(!attributes.contains(ObjCAttributes::UNSAFE_UNRETAINED));
                PropOwnership::Copy
            } else if attributes.contains(ObjCAttributes::ASSIGN) {
                assert!(!attributes.contains(ObjCAttributes::RETAIN));
                assert!(!attributes.contains(ObjCAttributes::UNSAFE_UNRETAINED));
                PropOwnership::Assign
            } else if attributes.contains(ObjCAttributes::RETAIN) {
                assert!(!attributes.contains(ObjCAttributes::UNSAFE_UNRETAINED));
                PropOwnership::Retain
            } else if attributes.contains(ObjCAttributes::UNSAFE_UNRETAINED) {
                PropOwnership::UnsafeUnretained
            } else {
                PropOwnership::Strong
            }
        };
        let getter;
        let setter;
        if attributes.contains(ObjCAttributes::GETTER)
            || attributes.contains(ObjCAttributes::SETTER)
        {
            let parent = cursor.semantic_parent().unwrap();
            let property_location = cursor.location().unwrap();
            let methods_at_same_location: Vec<clang::Cursor> = parent
                .children()
                .into_iter()
                .filter(|sibling| {
                    [
                        CursorKind::ObjCInstanceMethodDecl,
                        CursorKind::ObjCClassMethodDecl,
                    ]
                    .contains(&sibling.kind())
                        && sibling.location().unwrap() == property_location
                })
                .collect();
            if attributes.contains(ObjCAttributes::GETTER) {
                getter = Some(
                    methods_at_same_location
                        .iter()
                        .find(|method| method.arguments().unwrap().len() == 0)
                        .unwrap()
                        .spelling()
                        .unwrap(),
                );
            } else {
                getter = None;
            }
            if attributes.contains(ObjCAttributes::SETTER) {
                setter = Some(
                    methods_at_same_location
                        .iter()
                        .find(|method| method.arguments().unwrap().len() > 0)
                        .unwrap()
                        .spelling()
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
pub struct InterfaceDef {
    pub name: String,
    pub superclass: Option<String>,
    pub adopted_protocols: Vec<String>,
    pub template_params: Vec<String>,
    pub methods: Vec<ObjCMethod>,
    pub properties: Vec<Property>,
    pub origin: Option<Origin>,
}

impl InterfaceDef {
    fn from_cursor(cursor: &clang::Cursor, unnamed_tag_ids: &TagIdMap) -> Self {
        assert_eq!(cursor.kind(), CursorKind::ObjCInterfaceDecl);
        let name = cursor.spelling().unwrap();
        let children = cursor.children();

        let superclass = children
            .iter()
            .find(|child| child.kind() == CursorKind::ObjCSuperClassRef)
            .map(|child| child.spelling().unwrap());
        let adopted_protocols = children
            .iter()
            .filter(|child| child.kind() == CursorKind::ObjCProtocolRef)
            .map(|child| child.spelling().unwrap())
            .collect();
        let template_params = children
            .iter()
            .filter(|child| child.kind() == CursorKind::TemplateTypeParameter)
            .map(|child| child.spelling().unwrap())
            .collect();
        let methods = children
            .iter()
            .filter(|child| {
                [
                    CursorKind::ObjCInstanceMethodDecl,
                    CursorKind::ObjCClassMethodDecl,
                ]
                .contains(&child.kind())
            })
            // Methods generated from property don't have children with the type info we want
            // so for the time being just ignore them.
            .filter(|method| !is_generated_from_property(method))
            .map(|method| ObjCMethod::from_cursor(method, unnamed_tag_ids))
            .collect();
        let properties = children
            .iter()
            .filter(|child| child.kind() == CursorKind::ObjCPropertyDecl)
            .map(|prop| Property::from_cursor(prop, unnamed_tag_ids))
            .collect();
        let origin = Origin::from_cursor(cursor);

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
pub struct CategoryDef {
    pub name: Option<String>,
    pub class: String,
    adopted_protocols: Vec<String>,
    methods: Vec<ObjCMethod>,
    properties: Vec<Property>,
    origin: Option<Origin>,
}

impl CategoryDef {
    fn from_cursor(cursor: &clang::Cursor, unnamed_tag_ids: &TagIdMap) -> Self {
        assert_eq!(cursor.kind(), CursorKind::ObjCCategoryDecl);
        let children = cursor.children();

        let name = cursor.spelling();

        let class = children
            .iter()
            .find(|child| child.kind() == CursorKind::ObjCClassRef)
            .unwrap()
            .spelling()
            .unwrap();

        let adopted_protocols = children
            .iter()
            .filter(|child| child.kind() == CursorKind::ObjCProtocolRef)
            .map(|child| child.spelling().unwrap())
            .collect();

        let methods = children
            .iter()
            .filter(|child| {
                [
                    CursorKind::ObjCInstanceMethodDecl,
                    CursorKind::ObjCClassMethodDecl,
                ]
                .contains(&child.kind())
            })
            // Methods generated from property don't have children with the type info we want
            // so for the time being just ignore them.
            .filter(|method| !is_generated_from_property(method))
            .map(|method| ObjCMethod::from_cursor(method, unnamed_tag_ids))
            .collect();

        let properties = children
            .iter()
            .filter(|child| child.kind() == CursorKind::ObjCPropertyDecl)
            .map(|prop| Property::from_cursor(prop, unnamed_tag_ids))
            .collect();

        let origin = Origin::from_cursor(cursor);

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
pub struct ProtocolMethod {
    pub method: ObjCMethod,
    pub is_optional: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ProtocolProperty {
    pub property: Property,
    pub is_optional: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ProtocolDef {
    pub name: String,
    pub inherited_protocols: Vec<String>,
    pub methods: Vec<ProtocolMethod>,
    pub properties: Vec<ProtocolProperty>,
    pub origin: Option<Origin>,
}

impl ProtocolDef {
    fn from_cursor(cursor: &clang::Cursor, unnamed_tag_ids: &TagIdMap) -> Self {
        assert_eq!(cursor.kind(), CursorKind::ObjCProtocolDecl);
        let children = cursor.children();

        let inherited_protocols = children
            .iter()
            .filter(|child| child.kind() == CursorKind::ObjCProtocolRef)
            .map(|child| child.spelling().unwrap())
            .collect();

        let methods = children
            .iter()
            .filter(|child| {
                [
                    CursorKind::ObjCInstanceMethodDecl,
                    CursorKind::ObjCClassMethodDecl,
                ]
                .contains(&child.kind())
            })
            // Methods generated from property don't have children with the type info we want
            // so for the time being just ignore them.
            .filter(|child| !is_generated_from_property(child))
            .map(|child| ProtocolMethod {
                method: ObjCMethod::from_cursor(child, unnamed_tag_ids),
                is_optional: child.is_objc_optional(),
            })
            .collect();

        let properties = children
            .iter()
            .filter(|child| child.kind() == CursorKind::ObjCPropertyDecl)
            .map(|child| ProtocolProperty {
                property: Property::from_cursor(child, unnamed_tag_ids),
                is_optional: child.is_objc_optional(),
            })
            .collect();

        let origin = Origin::from_cursor(cursor);

        ProtocolDef {
            name: cursor.spelling().unwrap(),
            inherited_protocols,
            methods,
            properties,
            origin,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypedefDecl {
    pub name: String,
    pub underlying: ObjCType,
    pub origin: Option<Origin>,
}

impl TypedefDecl {
    fn from_cursor(decl: &clang::Cursor, unnamed_tag_ids: &TagIdMap) -> Self {
        assert_eq!(decl.kind(), CursorKind::TypedefDecl);

        let name = decl.spelling().unwrap();
        let underlying = ObjCType::from_type(
            &decl.typedef_decl_underlying_type().unwrap(),
            &mut parm_decl_children(&decl),
            unnamed_tag_ids,
        );
        let origin = Origin::from_cursor(decl);

        TypedefDecl {
            name,
            underlying,
            origin,
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum RecordKind {
    Union,
    Struct,
}

#[derive(Clone, Debug, PartialEq)]
pub struct RecordDef {
    pub id: TagId,
    pub kind: RecordKind,
    fields: Vec<Field>,
    origin: Option<Origin>,
}

impl RecordDef {
    fn from_cursor(decl: &clang::Cursor, unnamed_tag_ids: &TagIdMap) -> Self {
        let id = TagId::from_cursor(decl, unnamed_tag_ids);

        let kind = match decl.kind() {
            CursorKind::StructDecl => RecordKind::Struct,
            CursorKind::UnionDecl => RecordKind::Union,
            unexpected => panic!("Record declaration is not expected to be {:?}", unexpected),
        };

        assert!(
            decl.definition().is_some(),
            "An record definition should have fields definition"
        );
        let fields = decl
            .type_()
            .unwrap()
            .fields()
            .unwrap()
            .iter()
            .map(|field| Field::from_cursor(field, unnamed_tag_ids))
            .collect();

        let origin = Origin::from_cursor(decl);

        RecordDef {
            id,
            kind,
            fields,
            origin,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct FuncDecl {
    pub name: String,
    desc: CallableDesc,
    origin: Option<Origin>,
}

impl FuncDecl {
    fn from_cursor(decl: &clang::Cursor, unnamed_tag_ids: &TagIdMap) -> Self {
        assert_eq!(decl.kind(), CursorKind::FunctionDecl);
        let clang_type = decl.type_().unwrap();

        let name = decl.spelling().unwrap();
        let desc =
            CallableDesc::from_type(&clang_type, &mut parm_decl_children(&decl), unnamed_tag_ids);
        let origin = Origin::from_cursor(decl);

        FuncDecl { name, desc, origin }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct EnumDef {
    pub id: TagId,
    pub underlying: ObjCType,
    pub values: Vec<EnumValue>,
    pub attrs: Vec<Attr>,
    pub origin: Option<Origin>,
}

impl EnumDef {
    fn from_cursor(decl: &clang::Cursor, unnamed_tag_ids: &TagIdMap) -> Self {
        assert_eq!(decl.kind(), CursorKind::EnumDecl);

        let id = TagId::from_cursor(decl, unnamed_tag_ids);

        let underlying = ObjCType::from_type(
            &decl.enum_decl_int_type().unwrap(),
            &mut parm_decl_children(&decl),
            unnamed_tag_ids,
        );

        assert!(decl.definition().is_some());
        let signedness = type_signedness(&decl.enum_decl_int_type().unwrap())
            .expect("The underlying type of an enum should have a signedness");
        let values = decl
            .children()
            .into_iter()
            .filter(|child| child.kind() == CursorKind::EnumConstantDecl)
            .map(|decl| {
                let values = decl.enum_constant_value().unwrap();

                EnumValue {
                    name: decl.spelling().unwrap(),
                    value: match signedness {
                        Signedness::Signed => SignedOrNotInt::Signed(values.0),
                        Signedness::Unsigned => SignedOrNotInt::Unsigned(values.1),
                    },
                    attrs: Attr::from_decl(&decl),
                }
            })
            .collect();
        let attrs = Attr::from_decl(decl);
        let origin = Origin::from_cursor(decl);

        EnumDef {
            id,
            underlying,
            values,
            attrs,
            origin,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Decl {
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
pub enum ParseError {
    ClangError(clang::ClangError),
    CompilationError(String),
}

impl From<clang::ClangError> for ParseError {
    fn from(err: clang::ClangError) -> Self {
        ParseError::ClangError(err)
    }
}

type TagIdMap = HashMap<String, u32>;

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
pub fn print_full_clang_ast(source: &str) {
    // Preprocess before to get more easily interesting tokens coming from #defines.
    let source: &str = &preprocess_objc(source);

    // The documentation says that files specified as unsaved must exist so create a dummy temporary empty file
    let file = tempfile::NamedTempFile::new().unwrap();
    let index = clang::Index::new(false, true);
    let (args, options) = parser_configuration();

    let tu = index
        .parse(
            &args,
            file.path(),
            &[clang::UnsavedFile::new(file.path(), source)],
            options,
        )
        .expect("source should build cleanly");
    show_tree(&tu.cursor(), 0);
}

fn parser_configuration() -> (Vec<String>, clang::TuOptions) {
    use clang::TuOptions;
    (
        vec![
            "-x".to_string(),
            "objective-c".to_string(), // The file doesn't have an Objective-C extension so set the language explicitely (for some reason -ObjC doesn't work properly)
            "-fobjc-arc".to_string(),
            "-isysroot".to_string(),
            sdk_path(AppleSdk::MacOs),
        ],
        TuOptions::SKIP_FUNCTION_BODIES
            | TuOptions::INCLUDE_ATTRIBUTED_TYPES // Needed to get nullability
            | TuOptions::VISIT_IMPLICIT_ATTRIBUTES, // TODO: Check if needed
    )
}

pub fn ast_from_str(source: &str) -> Result<Vec<Decl>, ParseError> {
    // Preprocess before to get more easily interesting tokens coming from #defines.
    let source: &str = &preprocess_objc(source);

    // The documentation says that files specified as unsaved must exist so create a dummy temporary empty file
    let file = tempfile::NamedTempFile::new().unwrap();
    let index = clang::Index::new(false, true);
    let (args, options) = parser_configuration();

    let tu = index.parse(
        &args,
        file.path(),
        &[clang::UnsavedFile::new(file.path(), source)],
        options,
    )?;

    // The parser will try to parse as much as possible, even with errors.
    // In that case, we still want fail because some information will be missing anyway.
    let diagnostics = tu.diagnostics();
    let mut errors = diagnostics.filter(|diagnostic| {
        let severity = diagnostic.severity();
        [
            clang::DiagnosticSeverity::Error,
            clang::DiagnosticSeverity::Fatal,
        ]
        .contains(&severity)
    });
    if let Some(error) = errors.next() {
        return Err(ParseError::CompilationError(error.spelling()));
    }

    let mut decls: Vec<Decl> = Vec::new();

    let tu_cursor = tu.cursor();

    let mut next_unique_id: u32 = 1;
    let mut unnamed_tag_ids = TagIdMap::new();
    // Make unique identifiers for tags (struct/union/enum) that have no name.
    // Not using USRs as I'm not sure they are supposed to be stable between different clang versions.
    // Also not using locations as due to the the preprocessor they might not be unique.
    tu_cursor.visit_children(|cursor, _| {
        match cursor.kind() {
            CursorKind::StructDecl | CursorKind::UnionDecl | CursorKind::EnumDecl
                if cursor.spelling().is_none() => {}
            _ => return clang::ChildVisitResult::Recurse,
        }

        if cursor.definition().unwrap() != cursor {
            return clang::ChildVisitResult::Recurse;
        }

        let usr = cursor.usr().unwrap();
        if unnamed_tag_ids.contains_key(&usr) {
            return clang::ChildVisitResult::Recurse;
        }
        unnamed_tag_ids.insert(usr, next_unique_id);

        next_unique_id += 1;

        clang::ChildVisitResult::Recurse
    });
    let unnamed_tag_ids = unnamed_tag_ids;

    let mut visited: HashSet<String> = HashSet::new();
    tu_cursor.visit_children(|cursor, _| {
        let usr = cursor.usr();
        if let Some(usr) = cursor.usr() {
            if visited.contains(&usr) {
                return clang::ChildVisitResult::Recurse;
            }
        }
        let decl = match cursor.kind() {
            CursorKind::ObjCInterfaceDecl => Some(Decl::InterfaceDef(InterfaceDef::from_cursor(
                &cursor,
                &unnamed_tag_ids,
            ))),
            CursorKind::ObjCProtocolDecl => Some(Decl::ProtocolDef(ProtocolDef::from_cursor(
                &cursor,
                &unnamed_tag_ids,
            ))),
            CursorKind::ObjCCategoryDecl => Some(Decl::CategoryDef(CategoryDef::from_cursor(
                &cursor,
                &unnamed_tag_ids,
            ))),
            CursorKind::TypedefDecl => Some(Decl::TypedefDecl(TypedefDecl::from_cursor(
                &cursor,
                &unnamed_tag_ids,
            ))),
            CursorKind::FunctionDecl => Some(Decl::FuncDecl(FuncDecl::from_cursor(
                &cursor,
                &unnamed_tag_ids,
            ))),
            CursorKind::StructDecl | CursorKind::UnionDecl => {
                // We only care about definition of structs or unions, not their declaration.
                // (details of unnamed ones are included directly in the types that include them)
                if let Some(def) = cursor.definition() {
                    if def == cursor {
                        Some(Decl::RecordDef(RecordDef::from_cursor(
                            &cursor,
                            &unnamed_tag_ids,
                        )))
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            CursorKind::EnumDecl => {
                // We only care about definition of enums, not their declaration.
                // But contrarily to struct and enums, we do care about unnamed ones as they are used to declare constants.
                if let Some(def) = cursor.definition() {
                    if def == cursor {
                        Some(Decl::EnumDef(EnumDef::from_cursor(
                            &cursor,
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

        clang::ChildVisitResult::Recurse
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
        assert_eq!(
            Origin::from_path("/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/System/Library/Frameworks/IOKit.framework/Headers/graphics/IOGraphicsTypes.h"),
            Some(Origin::Framework("IOKit".to_owned())),
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
                        attrs: vec![],
                    }],
                    result: ObjCType::Void,
                    attrs: vec![],
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
                        attrs: vec![],
                    }],
                    result: ObjCType::Void,
                    attrs: vec![],
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
                        attrs: vec![],
                    }],
                    result: ObjCType::ObjPtr(ObjPtr {
                        kind: ObjPtrKind::Id(IdObjPtr {
                            protocols: vec!["P1".to_string(), "P2".to_string()],
                        }),
                        nullability: Nullability::Unspecified,
                    }),
                    attrs: vec![],
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
                    attrs: vec![],
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
                            attrs: vec![],
                        },
                        is_optional: false,
                    },
                    ProtocolMethod {
                        method: ObjCMethod {
                            name: "y".to_string(),
                            kind: ObjCMethodKind::Class,
                            params: vec![],
                            result: ObjCType::Void,
                            attrs: vec![],
                        },
                        is_optional: true,
                    },
                    ProtocolMethod {
                        method: ObjCMethod {
                            name: "z".to_string(),
                            kind: ObjCMethodKind::Instance,
                            params: vec![],
                            result: ObjCType::Num(NumKind::Int),
                            attrs: vec![],
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
                    attrs: vec![],
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
                    attrs: vec![],
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
                    attrs: vec![],
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
                    attrs: vec![],
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
                    attrs: vec![],
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
                    attrs: vec![],
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
                    attrs: vec![],
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
                        attrs: vec![],
                    },
                    ObjCMethod {
                        name: "inlineUnnamedStruct".to_string(),
                        kind: ObjCMethodKind::Instance,
                        params: vec![],
                        result: ObjCType::Tag(TagRef {
                            id: TagId::Unnamed(1),
                            kind: TagKind::Struct,
                        }),
                        attrs: vec![],
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
                        attrs: vec![],
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
                        attrs: vec![],
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
                        attrs: vec![],
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
                            attrs: vec![],
                        }]),
                        is_variadic: false,
                        attrs: vec![],
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
                                attrs: vec![],
                            })),
                            nullability: Nullability::Unspecified,
                        }),
                        attrs: vec![],
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
                                    attrs: vec![],
                                }]),
                                is_variadic: true,
                                attrs: vec![],
                            })),
                            nullability: Nullability::Unspecified,
                        }),
                        attrs: vec![],
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
                                attrs: vec![],
                            })),
                            nullability: Nullability::Unspecified,
                        }),
                        attrs: vec![],
                    },
                    // - (T)returningFunctionPointerTypedef;
                    ObjCMethod {
                        name: "returningFunctionPointerTypedef".to_string(),
                        kind: ObjCMethodKind::Instance,
                        params: vec![],
                        result: ObjCType::Typedef(TypedefRef {
                            name: "T".to_string(),
                        }),
                        attrs: vec![],
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
                                            attrs: vec![],
                                        }]),
                                        is_variadic: false,
                                        attrs: vec![],
                                    })),
                                    nullability: Nullability::Unspecified,
                                })),
                                params: Some(vec![Param {
                                    name: Some("innerParam".to_string()),
                                    objc_type: ObjCType::Num(NumKind::Double),
                                    attrs: vec![],
                                }]),
                                is_variadic: false,
                                attrs: vec![],
                            })),
                            nullability: Nullability::Unspecified,
                        }),
                        attrs: vec![],
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
                                attrs: vec![],
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
                                                attrs: vec![],
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
                                                                attrs: vec![],
                                                            }]),
                                                            is_variadic: false,
                                                            attrs: vec![],
                                                        },
                                                    )),
                                                    nullability: Nullability::Unspecified,
                                                }),
                                                attrs: vec![],
                                            },
                                        ]),
                                        is_variadic: false,
                                        attrs: vec![],
                                    })),
                                    nullability: Nullability::Unspecified,
                                }),
                                attrs: vec![],
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
                                    attrs: vec![],
                                }]),
                                is_variadic: false,
                                attrs: vec![],
                            })),
                            nullability: Nullability::Unspecified,
                        }),
                        attrs: vec![],
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
                                attrs: vec![],
                            },
                            Param {
                                name: None,
                                objc_type: ObjCType::ObjCSel(Nullability::NonNull),
                                attrs: vec![],
                            },
                        ]),
                        is_variadic: true,
                        attrs: vec![],
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
                            attrs: vec![],
                        }],
                        result: ObjCType::Typedef(TypedefRef {
                            name: "IMP".to_string(),
                        }),
                        attrs: vec![],
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
                    attrs: vec![],
                }]),
                is_variadic: true,
                attrs: vec![],
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
                            attrs: vec![Attr::Consumed],
                        }],
                        result: ObjCType::Void,
                        attrs: vec![],
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
                                    attrs: vec![],
                                }),
                                nullability: Nullability::Unspecified,
                            }),
                            attrs: vec![Attr::Noescape],
                        }],
                        result: ObjCType::Void,
                        attrs: vec![],
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
                        attrs: vec![Attr::Consumed],
                    }]),
                    is_variadic: false,
                    attrs: vec![],
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
                                attrs: vec![],
                            }),
                            nullability: Nullability::Unspecified,
                        }),
                        attrs: vec![Attr::Noescape],
                    }]),
                    is_variadic: false,
                    attrs: vec![],
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
        let source = r#"
            @interface I
            - (id)methodWithRetainedReturn __attribute__((__ns_returns_retained__));
            - (void)unavailableMethod __attribute__((unavailable("not available in automatic reference counting mode")));
            @end
            __attribute__((ns_returns_retained)) id function_with_retained_return(void);
        "#;

        let expected_decls = vec![
            Decl::InterfaceDef(InterfaceDef {
                name: "I".to_string(),
                superclass: None,
                adopted_protocols: vec![],
                template_params: vec![],
                methods: vec![
                    ObjCMethod {
                        name: "methodWithRetainedReturn".to_string(),
                        kind: ObjCMethodKind::Instance,
                        params: vec![],
                        result: ObjCType::ObjPtr(ObjPtr {
                            kind: ObjPtrKind::Id(IdObjPtr { protocols: vec![] }),
                            nullability: Nullability::Unspecified,
                        }),
                        attrs: vec![Attr::ReturnsRetained],
                    },
                    ObjCMethod {
                        name: "unavailableMethod".to_string(),
                        kind: ObjCMethodKind::Instance,
                        params: vec![],
                        result: ObjCType::Void,
                        attrs: vec![Attr::Unavailable],
                    },
                ],
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
                    attrs: vec![Attr::ReturnsRetained],
                },
                origin: None,
            }),
        ];

        let parsed_decls = ast_from_str(source).unwrap();
        assert_eq!(parsed_decls, expected_decls);
    }

    #[test]
    fn test_swift_name() {
        let source = r#"
            enum CIDataMatrixCodeECCVersion : long
            {
                CIDataMatrixCodeECCVersion000 __attribute__((swift_name("v000"))) = 0,
                CIDataMatrixCodeECCVersion050 __attribute__((swift_name("v050"))) = 50,
                CIDataMatrixCodeECCVersion080 __attribute__((swift_name("v080"))) = 80,
            } __attribute__((swift_name("CIDataMatrixCodeDescriptor.ECCVersion")));
        "#;

        let expected_decls = vec![Decl::EnumDef(EnumDef {
            id: TagId::Named("CIDataMatrixCodeECCVersion".to_string()),
            underlying: ObjCType::Num(NumKind::Long),
            values: vec![
                EnumValue {
                    name: "CIDataMatrixCodeECCVersion000".to_string(),
                    value: SignedOrNotInt::Signed(0),
                    attrs: vec![Attr::SwiftName("v000".to_string())],
                },
                EnumValue {
                    name: "CIDataMatrixCodeECCVersion050".to_string(),
                    value: SignedOrNotInt::Signed(50),
                    attrs: vec![Attr::SwiftName("v050".to_string())],
                },
                EnumValue {
                    name: "CIDataMatrixCodeECCVersion080".to_string(),
                    value: SignedOrNotInt::Signed(80),
                    attrs: vec![Attr::SwiftName("v080".to_string())],
                },
            ],
            attrs: vec![Attr::SwiftName(
                "CIDataMatrixCodeDescriptor.ECCVersion".to_string(),
            )],
            origin: None,
        })];

        let parsed_decls = ast_from_str(source).unwrap();
        assert_eq!(parsed_decls, expected_decls);
    }
}
