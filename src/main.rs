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

fn generate(decls: &Vec<ast::Decl>, _index: &TypeIndex) {
    use ast::{Decl, TagId};
    use quote::{format_ident, quote};

    for decl in decls {
        // let streams = Vec::new();
        match decl {
            Decl::EnumDef(def) => {
                let enum_name = match def.id {
                    TagId::Named(ref name) => name,
                    TagId::Unnamed(_) => continue,
                };

                let struct_ident = format_ident!("{}", enum_name);

                let value_names =
                    cleanup_enum_value_names(enum_name, def.values.iter().map(|value| &value.name));

                let value_name_idents = value_names
                    .iter()
                    .map(|value_name| format_ident!("{}", value_name));

                let original_value_names = def.values.iter().map(|value| &value.name);

                let values = def.values.iter().map(|value| value.value);

                let underlying = quote! {isize}; // TODO
                let code = quote! {
                    #[repr(transparent)]
                    struct #struct_ident(#underlying);

                    impl #struct_ident {
                        #(#[doc = #original_value_names] const #value_name_idents: Self = #struct_ident(#values);)*
                    }
                };

                println!("{}", code.to_string());
            }
            _ => {}
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
        #import <Foundation/Foundation.h>

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
    // println!("{:#?}", index);
    generate(&decls, &index);
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

fn without_final_s(text: &str) -> &str {
    if text.ends_with('s') || text.ends_with('S') {
        &text[..text.len() - 1]
    } else {
        text
    }
}

fn loosely_equal(text1: &str, text2: &str) -> bool {
    without_final_s(text1) == without_final_s(text2)
}

fn cleanup_enum_value_names<S: AsRef<str>, Iter: Iterator<Item = S>>(
    enum_name: &str,
    value_names: Iter,
) -> Vec<String> {
    let split_enum_name = snake_case_split(enum_name);

    value_names
        .map(|value_name| {
            let split_value_name = snake_case_split(value_name.as_ref());
            let mut value_name_iter = split_value_name.iter().peekable();
            if let Some(ref s) = value_name_iter.peek() {
                if *s == &"k" {
                    value_name_iter.next();
                }
            }
            let mut split_enum_name_iter = split_enum_name.iter();
            let name = value_name_iter
                .skip_while(|value_name_part| match split_enum_name_iter.next() {
                    Some(enum_name_part) => loosely_equal(value_name_part, enum_name_part),
                    None => false,
                })
                .map(|word| word.to_ascii_uppercase())
                .collect::<Vec<String>>()
                .join("_");
            if name.is_empty() {
                split_value_name.last().unwrap().to_ascii_uppercase()
            } else {
                name
            }
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
                "CFGregorianUnitFlags",
                [
                    "kCFGregorianUnitsYears",
                    "kCFGregorianUnitsMonths",
                    "kCFGregorianUnitsDays",
                    "kCFGregorianAllUnits",
                ]
                .into_iter()
            ),
            ["YEARS", "MONTHS", "DAYS", "ALL_UNITS"],
        );

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
                ["kCFSocketSuccess", "kCFSocketError", "kCFSocketTimeout",].into_iter()
            ),
            ["SUCCESS", "ERROR", "TIMEOUT",],
        );
    }
}
