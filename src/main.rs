#![allow(dead_code)]

use clang::{Clang, EntityKind, TypeKind};

// TODO: Try to get:
// - class and method OS version annotations
// - annotations for Swift
// - consume/retained/not retained
// - properties
// - exception throwing info (maybe from annotations from Swift)
// - support parsing code for different platforms (iOS, macOS, ...)
// - alignment, packing (and make sure the size and offset of each item is the same for clang and Rust as bindgen does)
// - instancetype
// - block pointers
// - namespacing of ObjC exported from Swift (though that might be fine as we're calling from generated ObjC)
// - arrays

#[derive(Debug, PartialEq)]
enum Origin {
    ObjCCore,
    Framework(String),
    Library(String),
    Unknown,
}

fn guess_origin(path: &str) -> Origin {
    use lazy_static::lazy_static;
    use regex::Regex;

    lazy_static! {
        static ref FRAMEWORK_PATH_RE: Regex =
            Regex::new(r"/([^./]+)\.framework/Headers/[^./]+.h\z").unwrap();
    }
    if let Some(caps) = FRAMEWORK_PATH_RE.captures(path) {
        return Origin::Framework(caps.get(1).unwrap().as_str().to_owned());
    }

    lazy_static! {
        static ref LIBRARY_PATH_RE: Regex =
            Regex::new(r"/usr/include/([^./]+)/[^./]+.h\z").unwrap();
    }
    if let Some(caps) = LIBRARY_PATH_RE.captures(path) {
        let library = caps.get(1).unwrap().as_str();
        if library == "objc" {
            return Origin::ObjCCore;
        } else {
            return Origin::Library(library.to_owned());
        }
    }

    Origin::Unknown
}

fn get_entity_file_path(entity: &clang::Entity) -> Option<String> {
    let path = entity.get_location()?.get_file_location().file?.get_path();
    match path.into_os_string().into_string() {
        Ok(string) => Some(string),
        _ => None,
    }
}

fn guess_entity_origin(entity: &clang::Entity) -> Origin {
    if let Some(file_path) = get_entity_file_path(entity) {
        guess_origin(&file_path)
    } else {
        Origin::Unknown
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
    String::from_utf8_lossy(&output.stdout).trim().to_owned()
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

    // if let Some(location) = entity.get_location() {
    //     println!("{}location: {:?}", indent, location);
    // }

    // if let Some(range) = entity.get_range() {
    //     println!("{}range: {:?}", indent, range);
    // }

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
        println!("{}canonical_entity: {:?}", indent, canonical_entity);
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
enum Nullability {
    NonNull,
    Nullable,
    Unspecified,
}

impl Nullability {
    fn from(nul: clang::Nullability) -> Self {
        match nul {
            clang::Nullability::NonNull => Nullability::NonNull,
            clang::Nullability::Nullable => Nullability::Nullable,
            clang::Nullability::Unspecified => Nullability::Unspecified,
        }
    }
}

#[derive(Debug, PartialEq)]
struct ObjCObjectPointer {
    interface: String,
    protocols: Vec<String>,
    type_args: Vec<ObjCTypeArg>,
    nullability: Nullability,
}

impl ObjCObjectPointer {
    fn with_nullability(self, nullability: Nullability) -> Self {
        ObjCObjectPointer {
            interface: self.interface,
            protocols: self.protocols,
            type_args: self.type_args,
            nullability,
        }
    }
}

#[derive(Debug, PartialEq)]
struct ObjCId {
    protocols: Vec<String>,
    nullability: Nullability,
}

impl ObjCId {
    fn with_nullability(self, nullability: Nullability) -> Self {
        ObjCId {
            protocols: self.protocols,
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

#[derive(Debug, PartialEq)]
struct Field {
    name: Option<String>,
    objc_type: ObjCType,
}

impl Field {
    fn from(entity: &clang::Entity) -> Self {
        assert_eq!(entity.get_kind(), EntityKind::FieldDecl);
        Field {
            name: entity.get_name(),
            objc_type: ObjCType::from(&entity.get_type().unwrap(), &mut parm_decl_children(entity)),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
enum RecordKind {
    Struct,
    Union,
}

#[derive(Debug, PartialEq)]
struct Record {
    name: Option<String>,
    kind: RecordKind,
    fields: Option<Vec<Field>>,
}

impl Record {
    fn from(clang_type: &clang::Type) -> Self {
        let decl = clang_type.get_declaration().unwrap();
        let fields = if decl.get_definition().is_some() {
            Some(
                clang_type
                    .get_fields()
                    .unwrap()
                    .iter()
                    .map(Field::from)
                    .collect(),
            )
        } else {
            None
        };
        let kind = match decl.get_kind() {
            EntityKind::StructDecl => RecordKind::Struct,
            EntityKind::UnionDecl => RecordKind::Union,
            unexpected_kind => panic!(
                "Unexpected kind for record declaration {:?}: {:?}",
                unexpected_kind, clang_type
            ),
        };

        Record {
            name: decl.get_name(),
            kind,
            fields,
        }
    }
}

#[derive(Debug, PartialEq)]
struct Param {
    name: Option<String>,
    objc_type: ObjCType,
}

#[derive(Debug, PartialEq)]
struct Callable {
    result: Box<ObjCType>,
    params: Option<Vec<Param>>,
    is_variadic: bool,
}

impl Callable {
    fn from<'a>(
        clang_type: &clang::Type,
        base_parm_decls: &mut impl Iterator<Item = clang::Entity<'a>>,
    ) -> Self {
        // result must be processed before parameters due to the order the entities are in base_parm_decls.
        let result = Box::new(ObjCType::from(
            &clang_type.get_result_type().unwrap(),
            base_parm_decls,
        ));

        let params = match clang_type.get_kind() {
            TypeKind::FunctionNoPrototype => None,
            TypeKind::FunctionPrototype => Some({
                let argument_types = clang_type.get_argument_types().unwrap();
                let mut params = Vec::with_capacity(argument_types.len());
                for arg_type in argument_types {
                    let parm_decl = base_parm_decls.next().unwrap();

                    assert_eq!(
                        ObjCType::from(&arg_type, &mut parm_decl_children(&parm_decl)),
                        ObjCType::from(
                            &parm_decl.get_type().unwrap(),
                            &mut parm_decl_children(&parm_decl)
                        )
                    );
                    let objc_type = ObjCType::from(&arg_type, &mut parm_decl_children(&parm_decl));
                    params.push(Param {
                        name: parm_decl.get_name(),
                        objc_type,
                    });
                }
                params
            }),
            unexpected_kind => panic!(
                "Unexpected kind for function declaration {:?}: {:?}",
                unexpected_kind, clang_type
            ),
        };

        Callable {
            result,
            params,
            is_variadic: clang_type.is_variadic(),
        }
    }
}

#[derive(Debug, PartialEq)]
struct Typedef {
    name: String,
    underlying: Box<ObjCType>,
}

impl Typedef {
    fn from(clang_type: &clang::Type) -> Self {
        assert_eq!(clang_type.get_kind(), TypeKind::Typedef);
        let decl = clang_type.get_declaration().unwrap();
        Typedef {
            name: clang_type.get_display_name(),
            underlying: Box::new(ObjCType::from(
                &decl.get_typedef_underlying_type().unwrap(),
                &mut parm_decl_children(&decl),
            )),
        }
    }
}

#[derive(Debug, PartialEq)]
struct Pointer {
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

#[derive(Debug, PartialEq)]
struct BlockPointer {
    pointee: Callable,
    nullability: Nullability,
}

impl BlockPointer {
    fn with_nullability(self, nullability: Nullability) -> Self {
        BlockPointer {
            pointee: self.pointee,
            nullability,
        }
    }
}

#[derive(Debug, PartialEq)]
struct Array {
    size: Option<usize>,
    element: Box<ObjCType>,
}

#[derive(Debug, PartialEq)]
struct EnumValue {
    name: String,
    value: i64, // TODO: Should be either signed or unsigned depending of underlying type
}

#[derive(Debug, PartialEq)]
struct Enum {
    name: Option<String>,
    underlying: Option<Box<ObjCType>>,
    values: Option<Vec<EnumValue>>,
}

impl Enum {
    fn from(clang_type: &clang::Type) -> Self {
        let decl = clang_type.get_declaration().unwrap();
        let values = if decl.get_definition().is_some() {
            Some(
                decl.get_children()
                    .into_iter()
                    .filter(|child| child.get_kind() == EntityKind::EnumConstantDecl)
                    .map(|decl| EnumValue {
                        name: decl.get_name().unwrap(),
                        value: decl.get_enum_constant_value().unwrap().0,
                    })
                    .collect(),
            )
        } else {
            None
        };

        let underlying = decl.get_enum_underlying_type().map(|underlying| {
            Box::new(ObjCType::from(&underlying, &mut parm_decl_children(&decl)))
        });

        Enum {
            name: decl.get_name(),
            underlying,
            values,
        }
    }
}

#[derive(Debug, PartialEq)]
enum ObjCTypeArg {
    ObjCObjectPointer(ObjCObjectPointer),
    ObjCTypeParam(String),
}

#[derive(Debug, PartialEq)]
enum ObjCType {
    Void,
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
    Typedef(Typedef),
    Pointer(Pointer),
    Record(Record),
    Function(Callable),
    BlockPointer(BlockPointer),
    ObjCObjectPointer(ObjCObjectPointer),
    ObjCTypeParam(String),
    ObjCId(ObjCId),
    ObjCClass(Nullability),
    ObjCSel(Nullability),
    Array(Array),
    Enum(Enum),
}

impl ObjCType {
    fn from<'a>(
        clang_type: &clang::Type,
        base_parm_decls: &mut impl Iterator<Item = clang::Entity<'a>>,
    ) -> Self {
        match clang_type.get_kind() {
            TypeKind::Void => ObjCType::Void,
            // SChar is "signed char", CharS is "char" when it is signed by default.
            TypeKind::SChar | TypeKind::CharS => ObjCType::SChar,
            // UChar is "unsigned char", CharU is "char" when it is unsigned by default.
            TypeKind::UChar | TypeKind::CharU => ObjCType::UChar,
            TypeKind::Short => ObjCType::Short,
            TypeKind::UShort => ObjCType::UShort,
            TypeKind::Int => ObjCType::Int,
            TypeKind::UInt => ObjCType::UInt,
            TypeKind::Long => ObjCType::Long,
            TypeKind::ULong => ObjCType::ULong,
            TypeKind::LongLong => ObjCType::LongLong,
            TypeKind::ULongLong => ObjCType::ULongLong,
            TypeKind::Float => ObjCType::Float,
            TypeKind::Double => ObjCType::Double,
            TypeKind::Pointer => {
                let pointee_type = clang_type.get_pointee_type().unwrap();
                ObjCType::Pointer(Pointer {
                    pointee: Box::new(ObjCType::from(&pointee_type, base_parm_decls)),
                    nullability: Nullability::Unspecified,
                })
            }
            TypeKind::ObjCObjectPointer => {
                let pointee_type = clang_type.get_pointee_type().unwrap();
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
                    .map(
                        |arg| match ObjCType::from(arg, &mut Vec::new().into_iter()) {
                            ObjCType::ObjCObjectPointer(pointer) => {
                                ObjCTypeArg::ObjCObjectPointer(pointer)
                            }
                            ObjCType::ObjCTypeParam(name) => ObjCTypeArg::ObjCTypeParam(name),
                            unexpected => panic!(
                                "Type arguments expected not expected to be {:?}",
                                unexpected
                            ),
                        },
                    )
                    .collect();

                let base_type = pointee_type.get_objc_object_base_type().unwrap();

                if base_type.get_kind() == TypeKind::ObjCId {
                    assert!(type_args.is_empty());
                    assert_eq!(base_type.get_display_name(), "id");
                    return ObjCType::ObjCId(ObjCId {
                        protocols,
                        nullability: Nullability::Unspecified,
                    });
                } else {
                    ObjCType::ObjCObjectPointer(ObjCObjectPointer {
                        interface: base_type.get_display_name(),
                        protocols,
                        type_args,
                        nullability: Nullability::Unspecified,
                    })
                }
            }
            TypeKind::Attributed => {
                let modified =
                    ObjCType::from(&clang_type.get_modified_type().unwrap(), base_parm_decls);
                if let Some(nullability) = clang_type.get_nullability() {
                    let nullability = Nullability::from(nullability);
                    match modified {
                        ObjCType::Pointer(pointer) => {
                            ObjCType::Pointer(pointer.with_nullability(nullability))
                        }
                        ObjCType::ObjCObjectPointer(pointer) => {
                            ObjCType::ObjCObjectPointer(pointer.with_nullability(nullability))
                        }
                        ObjCType::BlockPointer(pointer) => {
                            ObjCType::BlockPointer(pointer.with_nullability(nullability))
                        }
                        ObjCType::ObjCId(id) => ObjCType::ObjCId(id.with_nullability(nullability)),
                        ObjCType::ObjCSel(_) => ObjCType::ObjCSel(nullability),
                        ObjCType::ObjCClass(_) => ObjCType::ObjCClass(nullability),
                        _ => modified,
                    }
                } else {
                    modified
                }
            }
            TypeKind::ObjCTypeParam => ObjCType::ObjCTypeParam(clang_type.get_display_name()),
            TypeKind::ObjCId => ObjCType::ObjCId(ObjCId {
                protocols: Vec::new(),
                nullability: Nullability::Unspecified,
            }),
            TypeKind::ObjCSel => ObjCType::ObjCSel(Nullability::Unspecified),
            TypeKind::ObjCClass => ObjCType::ObjCClass(Nullability::Unspecified),
            TypeKind::Typedef => ObjCType::Typedef(Typedef::from(&clang_type)),
            TypeKind::Elaborated => {
                Self::from(&clang_type.get_elaborated_type().unwrap(), base_parm_decls)
            }
            TypeKind::Record => ObjCType::Record(Record::from(&clang_type)),
            TypeKind::FunctionNoPrototype | TypeKind::FunctionPrototype => {
                ObjCType::Function(Callable::from(&clang_type, base_parm_decls))
            }
            TypeKind::ConstantArray => ObjCType::Array(Array {
                size: Some(clang_type.get_size().unwrap()),
                element: Box::new(ObjCType::from(
                    &clang_type.get_element_type().unwrap(),
                    base_parm_decls,
                )),
            }),
            TypeKind::IncompleteArray => ObjCType::Array(Array {
                size: None,
                element: Box::new(ObjCType::from(
                    &clang_type.get_element_type().unwrap(),
                    base_parm_decls,
                )),
            }),
            TypeKind::BlockPointer => ObjCType::BlockPointer(BlockPointer {
                pointee: Callable::from(&clang_type.get_pointee_type().unwrap(), base_parm_decls),
                nullability: Nullability::Unspecified,
            }),
            TypeKind::Enum => ObjCType::Enum(Enum::from(&clang_type)),
            unknown_kind => panic!("Unhandled type kind {:?}: {:?}", unknown_kind, clang_type),
        }
    }
}

#[derive(Debug, PartialEq)]
struct ObjCParam {
    name: String,
    objc_type: ObjCType,
}

impl ObjCParam {
    fn from(entity: &clang::Entity) -> Self {
        assert_eq!(entity.get_kind(), EntityKind::ParmDecl);
        ObjCParam {
            name: entity.get_name().unwrap(),
            objc_type: ObjCType::from(&entity.get_type().unwrap(), &mut parm_decl_children(entity)),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
enum ObjCMethodKind {
    Class,
    Instance,
}

#[derive(Debug, PartialEq)]
struct ObjCMethod {
    name: String,
    kind: ObjCMethodKind,
    params: Vec<ObjCParam>,
    result: ObjCType,
}

impl ObjCMethod {
    fn from(entity: &clang::Entity) -> Self {
        let kind = match entity.get_kind() {
            EntityKind::ObjCClassMethodDecl => ObjCMethodKind::Class,
            EntityKind::ObjCInstanceMethodDecl => ObjCMethodKind::Instance,
            _ => panic!("entity should be either a class or instance method"),
        };

        let params = entity
            .get_arguments()
            .unwrap()
            .iter()
            .map(ObjCParam::from)
            .collect();

        let result = ObjCType::from(
            &entity.get_result_type().unwrap(),
            &mut parm_decl_children(entity),
        );
        ObjCMethod {
            name: entity.get_name().unwrap(),
            kind,
            params,
            result,
        }
    }
}

#[derive(Debug, PartialEq)]
struct ObjCInterface {
    name: String,
    superclass: Option<String>,
    adopted_protocols: Vec<String>,
    template_params: Vec<String>,
    methods: Vec<ObjCMethod>,
}

impl ObjCInterface {
    fn from(entity: &clang::Entity) -> Self {
        assert_eq!(entity.get_kind(), EntityKind::ObjCInterfaceDecl);
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
            .map(ObjCMethod::from)
            .collect();
        ObjCInterface {
            name: entity.get_name().unwrap(),
            superclass,
            adopted_protocols,
            template_params,
            methods,
        }
    }
}

#[derive(Debug, PartialEq)]
struct ObjCCategory {
    name: String,
    class: String,
    adopted_protocols: Vec<String>,
    methods: Vec<ObjCMethod>,
}

impl ObjCCategory {
    fn from(entity: &clang::Entity) -> Self {
        assert_eq!(entity.get_kind(), EntityKind::ObjCCategoryDecl);
        let children = entity.get_children();

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
            .map(ObjCMethod::from)
            .collect();
        ObjCCategory {
            name: entity.get_name().unwrap(),
            class,
            adopted_protocols,
            methods,
        }
    }
}

#[derive(Debug, PartialEq)]
struct ObjCProtocolMethod {
    method: ObjCMethod,
    is_optional: bool,
}

#[derive(Debug, PartialEq)]
struct ObjCProtocol {
    name: String,
    inherited_protocols: Vec<String>,
    methods: Vec<ObjCProtocolMethod>,
}

impl ObjCProtocol {
    fn from(entity: &clang::Entity) -> Self {
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
            .map(|child| ObjCProtocolMethod {
                method: ObjCMethod::from(child),
                is_optional: child.is_objc_optional(),
            })
            .collect();

        ObjCProtocol {
            name: entity.get_name().unwrap(),
            inherited_protocols,
            methods,
        }
    }
}

#[derive(Debug, PartialEq)]
enum ObjCDecl {
    ObjCProtocol(ObjCProtocol),
    ObjCInterface(ObjCInterface),
    ObjCCategory(ObjCCategory),
}

#[derive(Debug)]
enum ParseError {
    SourceError(clang::SourceError),
    CompilationError(String),
}

impl From<clang::SourceError> for ParseError {
    fn from(err: clang::SourceError) -> Self {
        ParseError::SourceError(err)
    }
}

fn parse_objc(clang: &Clang, source: &str) -> Result<Vec<ObjCDecl>, ParseError> {
    use clang::diagnostic::Severity;

    // The documentation says that files specified as unsaved must exist so create a dummy temporary empty file
    let file = tempfile::NamedTempFile::new().unwrap();
    let index = clang::Index::new(clang, false, true);
    let mut parser = index.parser(file.path());

    parser.arguments(&[
        "-x",
        "objective-c", // The file doesn't have an Objective-C extension so set the language explicitely
        "-isysroot",
        &sdk_path(AppleSdk::MacOs),
    ]);
    parser.skip_function_bodies(true);
    parser.include_attributed_types(true); // Needed to get nullability
    parser.visit_implicit_attributes(true); // TODO: Check if needed
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

    println!("--------------------------------");
    show_tree(&tu.get_entity(), 0);
    println!("--------------------------------");

    let mut objc_decls: Vec<ObjCDecl> = Vec::new();
    for entity in tu.get_entity().get_children() {
        match entity.get_kind() {
            EntityKind::ObjCInterfaceDecl => {
                objc_decls.push(ObjCDecl::ObjCInterface(ObjCInterface::from(&entity)));
            }
            EntityKind::ObjCProtocolDecl => {
                objc_decls.push(ObjCDecl::ObjCProtocol(ObjCProtocol::from(&entity)));
            }
            EntityKind::ObjCCategoryDecl => {
                objc_decls.push(ObjCDecl::ObjCCategory(ObjCCategory::from(&entity)));
            }
            _ => {}
        }
    }
    Ok(objc_decls)
}

fn main() {
    let source = "
        // #import <AVFoundation/AVFoundation.h>
        struct ll { struct ll *next; };
        @interface I
        - (struct ll)foo;
        @end
    ";
    let clang = Clang::new().expect("Could not load libclang");
    let decls = parse_objc(&clang, source).unwrap();
    println!("{:#?}", decls);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_guess_origin() {
        assert_eq!(guess_origin(""), Origin::Unknown);
        assert_eq!(
            guess_origin("/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.13.sdk/System/Library/Frameworks/Foundation.framework/Headers/NSValue.h"),
            Origin::Framework("Foundation".to_owned()),
        );
        assert_eq!(
            guess_origin("/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.13.sdk/System/Library/Frameworks/Metal.framework/Headers/MTLCaptureManager.h"),
            Origin::Framework("Metal".to_owned()),
        );
        assert_eq!(
            guess_origin("/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.13.sdk/usr/include/objc/NSObject.h"),
            Origin::ObjCCore,
        );
        assert_eq!(
            guess_origin("/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.13.sdk/usr/include/dispatch/object.h"),
            Origin::Library("dispatch".to_owned()),
        );
        assert_eq!(
            guess_origin("/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.13.sdk/usr/include/dispatch/queue.h"),
            Origin::Library("dispatch".to_owned()),
        );
    }

    #[test]
    fn test_parameter_adopting_protocols() {
        let clang = Clang::new().expect("Could not load libclang");

        let source = "
            @protocol P1, P2;
            @class B;
            @interface A
            - (void)foo:(id<P1, P2>)x;
            + (void)bar:(B<P2>* _Nonnull)y;
            - (id<P1, P2>)foobar:(B<P2>*)z;
            @end
        ";

        let expected_decls = vec![ObjCDecl::ObjCInterface(ObjCInterface {
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
                        objc_type: ObjCType::ObjCId(ObjCId {
                            protocols: vec!["P1".to_string(), "P2".to_string()],
                            nullability: Nullability::Unspecified,
                        }),
                    }],
                    result: ObjCType::Void,
                },
                ObjCMethod {
                    name: "bar:".to_string(),
                    kind: ObjCMethodKind::Class,
                    params: vec![ObjCParam {
                        name: "y".to_string(),
                        objc_type: ObjCType::ObjCObjectPointer(ObjCObjectPointer {
                            interface: "B".to_string(),
                            protocols: vec!["P2".to_string()],
                            type_args: vec![],
                            nullability: Nullability::NonNull,
                        }),
                    }],
                    result: ObjCType::Void,
                },
                ObjCMethod {
                    name: "foobar:".to_string(),
                    kind: ObjCMethodKind::Instance,
                    params: vec![ObjCParam {
                        name: "z".to_string(),
                        objc_type: ObjCType::ObjCObjectPointer(ObjCObjectPointer {
                            interface: "B".to_string(),
                            protocols: vec!["P2".to_string()],
                            type_args: vec![],
                            nullability: Nullability::Unspecified,
                        }),
                    }],
                    result: ObjCType::ObjCId(ObjCId {
                        protocols: vec!["P1".to_string(), "P2".to_string()],
                        nullability: Nullability::Unspecified,
                    }),
                },
            ],
        })];

        let parsed_decls = parse_objc(&clang, source).unwrap();
        assert_eq!(parsed_decls, expected_decls);
    }

    #[test]
    fn test_superclass() {
        let clang = Clang::new().expect("Could not load libclang");

        let source = "
            @interface A
            @end
            @interface B: A
            @end
        ";

        let expected_decls = vec![
            ObjCDecl::ObjCInterface(ObjCInterface {
                name: "A".to_string(),
                superclass: None,
                adopted_protocols: vec![],
                template_params: vec![],
                methods: vec![],
            }),
            ObjCDecl::ObjCInterface(ObjCInterface {
                name: "B".to_string(),
                superclass: Some("A".to_string()),
                adopted_protocols: vec![],
                template_params: vec![],
                methods: vec![],
            }),
        ];

        let parsed_decls = parse_objc(&clang, source).unwrap();
        assert_eq!(parsed_decls, expected_decls);
    }

    #[test]
    fn test_template_params() {
        let clang = Clang::new().expect("Could not load libclang");

        let source = "
            @interface A
            @end
            @interface B<X, Y, Z>: A
            - (X)x;
            @end
        ";

        let expected_decls = vec![
            ObjCDecl::ObjCInterface(ObjCInterface {
                name: "A".to_string(),
                superclass: None,
                adopted_protocols: vec![],
                template_params: vec![],
                methods: vec![],
            }),
            ObjCDecl::ObjCInterface(ObjCInterface {
                name: "B".to_string(),
                superclass: Some("A".to_string()),
                adopted_protocols: vec![],
                template_params: vec!["X".to_string(), "Y".to_string(), "Z".to_string()],
                methods: vec![ObjCMethod {
                    name: "x".to_string(),
                    kind: ObjCMethodKind::Instance,
                    params: vec![],
                    result: ObjCType::ObjCTypeParam("X".to_string()),
                }],
            }),
        ];

        let parsed_decls = parse_objc(&clang, source).unwrap();
        assert_eq!(parsed_decls, expected_decls);
    }

    #[test]
    fn test_protocol() {
        let clang = Clang::new().expect("Could not load libclang");

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
            ObjCDecl::ObjCProtocol(ObjCProtocol {
                name: "B".to_string(),
                inherited_protocols: vec![],
                methods: vec![],
            }),
            ObjCDecl::ObjCProtocol(ObjCProtocol {
                name: "A".to_string(),
                inherited_protocols: vec![],
                methods: vec![
                    ObjCProtocolMethod {
                        method: ObjCMethod {
                            name: "x".to_string(),
                            kind: ObjCMethodKind::Instance,
                            params: vec![],
                            result: ObjCType::Void,
                        },
                        is_optional: false,
                    },
                    ObjCProtocolMethod {
                        method: ObjCMethod {
                            name: "y".to_string(),
                            kind: ObjCMethodKind::Class,
                            params: vec![],
                            result: ObjCType::Void,
                        },
                        is_optional: true,
                    },
                    ObjCProtocolMethod {
                        method: ObjCMethod {
                            name: "z".to_string(),
                            kind: ObjCMethodKind::Instance,
                            params: vec![],
                            result: ObjCType::Int,
                        },
                        is_optional: true,
                    },
                ],
            }),
        ];

        let parsed_decls = parse_objc(&clang, source).unwrap();
        assert_eq!(parsed_decls, expected_decls);
    }

    #[test]
    fn test_category() {
        let clang = Clang::new().expect("Could not load libclang");

        let source = "
            @protocol P;
            @interface A
            @end
            @interface A (Categ) <P>
            - (void)foo;
            @end
        ";

        let expected_decls = vec![
            ObjCDecl::ObjCInterface(ObjCInterface {
                name: "A".to_string(),
                superclass: None,
                adopted_protocols: vec![],
                template_params: vec![],
                methods: vec![],
            }),
            ObjCDecl::ObjCCategory(ObjCCategory {
                name: "Categ".to_string(),
                class: "A".to_string(),
                adopted_protocols: vec!["P".to_string()],
                methods: vec![ObjCMethod {
                    name: "foo".to_string(),
                    kind: ObjCMethodKind::Instance,
                    params: vec![],
                    result: ObjCType::Void,
                }],
            }),
        ];

        let parsed_decls = parse_objc(&clang, source).unwrap();
        assert_eq!(parsed_decls, expected_decls);
    }

    #[test]
    fn test_id() {
        let clang = Clang::new().expect("Could not load libclang");

        let source = "
            @protocol P;
            @interface A
            - (id)x;
            - (id<P>)y;
            - (id<P> _Nonnull)z;
            @end
        ";

        let expected_decls = vec![ObjCDecl::ObjCInterface(ObjCInterface {
            name: "A".to_string(),
            superclass: None,
            adopted_protocols: vec![],
            template_params: vec![],
            methods: vec![
                ObjCMethod {
                    name: "x".to_string(),
                    kind: ObjCMethodKind::Instance,
                    params: vec![],
                    result: ObjCType::ObjCId(ObjCId {
                        protocols: vec![],
                        nullability: Nullability::Unspecified,
                    }),
                },
                ObjCMethod {
                    name: "y".to_string(),
                    kind: ObjCMethodKind::Instance,
                    params: vec![],
                    result: ObjCType::ObjCId(ObjCId {
                        protocols: vec!["P".to_string()],
                        nullability: Nullability::Unspecified,
                    }),
                },
                ObjCMethod {
                    name: "z".to_string(),
                    kind: ObjCMethodKind::Instance,
                    params: vec![],
                    result: ObjCType::ObjCId(ObjCId {
                        protocols: vec!["P".to_string()],
                        nullability: Nullability::NonNull,
                    }),
                },
            ],
        })];

        let parsed_decls = parse_objc(&clang, source).unwrap();
        assert_eq!(parsed_decls, expected_decls);
    }

    #[test]
    fn test_typedef() {
        let clang = Clang::new().expect("Could not load libclang");

        let source = "
            typedef int I;
            @interface A
            - (I)foo;
            @end
        ";

        let expected_decls = vec![ObjCDecl::ObjCInterface(ObjCInterface {
            name: "A".to_string(),
            superclass: None,
            adopted_protocols: vec![],
            template_params: vec![],
            methods: vec![ObjCMethod {
                name: "foo".to_string(),
                kind: ObjCMethodKind::Instance,
                params: vec![],
                result: ObjCType::Typedef(Typedef {
                    name: "I".to_string(),
                    underlying: Box::new(ObjCType::Int),
                }),
            }],
        })];

        let parsed_decls = parse_objc(&clang, source).unwrap();
        assert_eq!(parsed_decls, expected_decls);
    }

    #[test]
    fn test_struct() {
        let clang = Clang::new().expect("Could not load libclang");

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

        let expected_decls = vec![ObjCDecl::ObjCInterface(ObjCInterface {
            name: "A".to_string(),
            superclass: None,
            adopted_protocols: vec![],
            template_params: vec![],
            methods: vec![
                ObjCMethod {
                    name: "standardStruct".to_string(),
                    kind: ObjCMethodKind::Instance,
                    params: vec![],
                    result: ObjCType::Record(Record {
                        name: Some("S".to_string()),
                        kind: RecordKind::Struct,
                        fields: Some(vec![Field {
                            name: Some("x".to_string()),
                            objc_type: ObjCType::Int,
                        }]),
                    }),
                },
                ObjCMethod {
                    name: "inlineUnnamedStruct".to_string(),
                    kind: ObjCMethodKind::Instance,
                    params: vec![],
                    result: ObjCType::Record(Record {
                        name: None,
                        kind: RecordKind::Struct,
                        fields: Some(vec![
                            Field {
                                name: Some("f".to_string()),
                                objc_type: ObjCType::Float,
                            },
                            Field {
                                name: None,
                                objc_type: ObjCType::Record(Record {
                                    name: None,
                                    kind: RecordKind::Union,
                                    fields: Some(vec![
                                        Field {
                                            name: Some("i".to_string()),
                                            objc_type: ObjCType::Int,
                                        },
                                        Field {
                                            name: Some("d".to_string()),
                                            objc_type: ObjCType::Double,
                                        },
                                    ]),
                                }),
                            },
                        ]),
                    }),
                },
                ObjCMethod {
                    name: "pointerToStructTypedef".to_string(),
                    kind: ObjCMethodKind::Instance,
                    params: vec![],
                    result: ObjCType::Pointer(Pointer {
                        pointee: Box::new(ObjCType::Typedef(Typedef {
                            name: "T".to_string(),
                            underlying: Box::new(ObjCType::Record(Record {
                                name: Some("S".to_string()),
                                kind: RecordKind::Struct,
                                fields: Some(vec![Field {
                                    name: Some("x".to_string()),
                                    objc_type: ObjCType::Int,
                                }]),
                            })),
                        })),
                        nullability: Nullability::Unspecified,
                    }),
                },
                ObjCMethod {
                    name: "pointerToUndeclaredStruct".to_string(),
                    kind: ObjCMethodKind::Instance,
                    params: vec![],
                    result: ObjCType::Pointer(Pointer {
                        pointee: Box::new(ObjCType::Record(Record {
                            name: Some("Undeclared".to_string()),
                            kind: RecordKind::Struct,
                            fields: None,
                        })),
                        nullability: Nullability::Unspecified,
                    }),
                },
                ObjCMethod {
                    name: "pointerToStructDeclaredAfterwards".to_string(),
                    kind: ObjCMethodKind::Instance,
                    params: vec![],
                    result: ObjCType::Pointer(Pointer {
                        pointee: Box::new(ObjCType::Record(Record {
                            name: Some("DeclaredAfterwards".to_string()),
                            kind: RecordKind::Struct,
                            fields: Some(vec![Field {
                                name: Some("c".to_string()),
                                objc_type: ObjCType::SChar,
                            }]),
                        })),
                        nullability: Nullability::Unspecified,
                    }),
                },
            ],
        })];

        let parsed_decls = parse_objc(&clang, source).unwrap();
        assert_eq!(parsed_decls, expected_decls);
    }

    #[test]
    fn test_function_pointers() {
        let clang = Clang::new().expect("Could not load libclang");

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

        let expected_decls = vec![ObjCDecl::ObjCInterface(ObjCInterface {
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
                        pointee: Box::new(ObjCType::Function(Callable {
                            result: Box::new(ObjCType::Int),
                            params: None,
                            is_variadic: true,
                        })),
                        nullability: Nullability::Unspecified,
                    }),
                },
                // - (int(*)(float, ...))returningFunctionPointerVariadic;
                ObjCMethod {
                    name: "returningFunctionPointerVariadic".to_string(),
                    kind: ObjCMethodKind::Instance,
                    params: vec![],
                    result: ObjCType::Pointer(Pointer {
                        pointee: Box::new(ObjCType::Function(Callable {
                            result: Box::new(ObjCType::Int),
                            params: Some(vec![Param {
                                name: None,
                                objc_type: ObjCType::Float,
                            }]),
                            is_variadic: true,
                        })),
                        nullability: Nullability::Unspecified,
                    }),
                },
                // - (int(*)(void))returningFunctionPointerWithNoParameters;
                ObjCMethod {
                    name: "returningFunctionPointerWithNoParameters".to_string(),
                    kind: ObjCMethodKind::Instance,
                    params: vec![],
                    result: ObjCType::Pointer(Pointer {
                        pointee: Box::new(ObjCType::Function(Callable {
                            result: Box::new(ObjCType::Int),
                            params: Some(vec![]),
                            is_variadic: false,
                        })),
                        nullability: Nullability::Unspecified,
                    }),
                },
                // - (T)returningFunctionPointerTypedef;
                ObjCMethod {
                    name: "returningFunctionPointerTypedef".to_string(),
                    kind: ObjCMethodKind::Instance,
                    params: vec![],
                    result: ObjCType::Typedef(Typedef {
                        name: "T".to_string(),
                        underlying: Box::new(ObjCType::Pointer(Pointer {
                            pointee: Box::new(ObjCType::Function(Callable {
                                result: Box::new(ObjCType::Void),
                                params: Some(vec![Param {
                                    name: Some("typedefParam".to_string()),
                                    objc_type: ObjCType::Int,
                                }]),
                                is_variadic: false,
                            })),
                            nullability: Nullability::Unspecified,
                        })),
                    }),
                },
                // - (char (*(*)(double innerParam))(float outerParam))returningFunctionPointerReturningFunctionPointer;
                ObjCMethod {
                    name: "returningFunctionPointerReturningFunctionPointer".to_string(),
                    kind: ObjCMethodKind::Instance,
                    params: vec![],
                    result: ObjCType::Pointer(Pointer {
                        pointee: Box::new(ObjCType::Function(Callable {
                            result: Box::new(ObjCType::Pointer(Pointer {
                                pointee: Box::new(ObjCType::Function(Callable {
                                    result: Box::new(ObjCType::SChar),
                                    params: Some(vec![Param {
                                        name: Some("outerParam".to_string()),
                                        objc_type: ObjCType::Float,
                                    }]),
                                    is_variadic: false,
                                })),
                                nullability: Nullability::Unspecified,
                            })),
                            params: Some(vec![Param {
                                name: Some("innerParam".to_string()),
                                objc_type: ObjCType::Double,
                            }]),
                            is_variadic: false,
                        })),
                        nullability: Nullability::Unspecified,
                    }),
                },
                // - (A *(*)(short returnedFunctionParameter))takingTypedef:(T)typedefParam andFunctionPointersTakingFunctionPointers:(A *(*)(float someFloat, int (*functionPointerParam)(char someChar)))complicatedParam;
                ObjCMethod {
                    name: "takingTypedef:andFunctionPointersTakingFunctionPointers:".to_string(),
                    kind: ObjCMethodKind::Instance,
                    params: vec![
                        ObjCParam {
                            name: "typedefParam".to_string(),
                            objc_type: ObjCType::Typedef(Typedef {
                                name: "T".to_string(),
                                underlying: Box::new(ObjCType::Pointer(Pointer {
                                    pointee: Box::new(ObjCType::Function(Callable {
                                        result: Box::new(ObjCType::Void),
                                        params: Some(vec![Param {
                                            name: Some("typedefParam".to_string()),
                                            objc_type: ObjCType::Int,
                                        }]),
                                        is_variadic: false,
                                    })),
                                    nullability: Nullability::Unspecified,
                                })),
                            }),
                        },
                        ObjCParam {
                            name: "complicatedParam".to_string(),
                            objc_type: ObjCType::Pointer(Pointer {
                                pointee: Box::new(ObjCType::Function(Callable {
                                    result: Box::new(ObjCType::ObjCObjectPointer(
                                        ObjCObjectPointer {
                                            interface: "A".to_string(),
                                            protocols: vec![],
                                            type_args: vec![],
                                            nullability: Nullability::Unspecified,
                                        },
                                    )),
                                    params: Some(vec![
                                        Param {
                                            name: Some("someFloat".to_string()),
                                            objc_type: ObjCType::Float,
                                        },
                                        Param {
                                            name: Some("functionPointerParam".to_string()),
                                            objc_type: ObjCType::Pointer(Pointer {
                                                pointee: Box::new(ObjCType::Function(Callable {
                                                    result: Box::new(ObjCType::Int),
                                                    params: Some(vec![Param {
                                                        name: Some("someChar".to_string()),
                                                        objc_type: ObjCType::SChar,
                                                    }]),
                                                    is_variadic: false,
                                                })),
                                                nullability: Nullability::Unspecified,
                                            }),
                                        },
                                    ]),
                                    is_variadic: false,
                                })),
                                nullability: Nullability::Unspecified,
                            }),
                        },
                    ],
                    result: ObjCType::Pointer(Pointer {
                        pointee: Box::new(ObjCType::Function(Callable {
                            result: Box::new(ObjCType::ObjCObjectPointer(ObjCObjectPointer {
                                interface: "A".to_string(),
                                protocols: vec![],
                                type_args: vec![],
                                nullability: Nullability::Unspecified,
                            })),
                            params: Some(vec![Param {
                                name: Some("returnedFunctionParameter".to_string()),
                                objc_type: ObjCType::Short,
                            }]),
                            is_variadic: false,
                        })),
                        nullability: Nullability::Unspecified,
                    }),
                },
            ],
        })];

        let parsed_decls = parse_objc(&clang, source).unwrap();
        assert_eq!(parsed_decls, expected_decls);
    }

    #[test]
    fn test_method_for_selector() {
        let clang = Clang::new().expect("Could not load libclang");

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

        let expected_decls = vec![ObjCDecl::ObjCProtocol(ObjCProtocol {
            name: "P".to_string(),
            inherited_protocols: vec![],
            methods: vec![ObjCProtocolMethod {
                method: ObjCMethod {
                    name: "methodForSelector:".to_string(),
                    kind: ObjCMethodKind::Instance,
                    params: vec![ObjCParam {
                        name: "aSelector".to_string(),
                        objc_type: ObjCType::ObjCSel(Nullability::Unspecified),
                    }],
                    result: ObjCType::Typedef(Typedef {
                        name: "IMP".to_string(),
                        underlying: Box::new(ObjCType::Pointer(Pointer {
                            pointee: Box::new(ObjCType::Function(Callable {
                                result: Box::new(ObjCType::ObjCId(ObjCId {
                                    protocols: vec![],
                                    nullability: Nullability::Nullable,
                                })),
                                params: Some(vec![
                                    Param {
                                        name: None,
                                        objc_type: ObjCType::ObjCId(ObjCId {
                                            protocols: vec![],
                                            nullability: Nullability::NonNull,
                                        }),
                                    },
                                    Param {
                                        name: None,
                                        objc_type: ObjCType::ObjCSel(Nullability::NonNull),
                                    },
                                ]),
                                is_variadic: true,
                            })),
                            nullability: Nullability::Unspecified,
                        })),
                    }),
                },
                is_optional: false,
            }],
        })];

        let parsed_decls = parse_objc(&clang, source).unwrap();
        assert_eq!(parsed_decls, expected_decls);
    }
}
