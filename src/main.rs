#![allow(dead_code)]
#![allow(clippy::cognitive_complexity)]

mod ast;

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
    fn from(decls: &Vec<ast::Decl>) -> Self {
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
                    let vec = categories
                        .entry(def.class.clone())
                        .or_insert_with(|| Vec::new());
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

fn main() {
    let source = r#"
        // @interface I
        // - (id)foo __attribute__((__ns_returns_retained__));
        // @end
        // __attribute__((ns_returns_retained)) id foo(void);

        // #import <AVFoundation/AVFoundation.h>
        // #import <Cocoa/Cocoa.h>
        #import <Foundation/NSString.h>

        // @interface I
        // @end

// typedef const struct __attribute__((objc_bridge(NSString))) __CFString * CFStringRef;

        // typedef enum { A = 1 } foo;
        // enum E { B = 1000 };
        // typedef signed long CFIndex;
        // typedef enum __attribute__((enum_extensibility(open))) CFStreamStatus : CFIndex CFStreamStatus; enum CFStreamStatus : CFIndex {
        //     kCFStreamStatusNotOpen = 0,
        //     kCFStreamStatusOpening,
        //     kCFStreamStatusOpen,
        //     kCFStreamStatusReading,
        //     kCFStreamStatusWriting,
        //     kCFStreamStatusAtEnd,
        //     kCFStreamStatusClosed,
        //     kCFStreamStatusError
        // };
        // struct ll { struct ll *nextl; };
        // struct { float f; union { int i; double d; }; } a;
  "#;
    let decls = ast::ast_from_str(source).unwrap();
    // ast::print_full_clang_ast(source);
    // println!("{:#?}", decls);
    let index = TypeIndex::from(&decls);
    println!("{:#?}", index);
}
