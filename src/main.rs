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

                // Enums with names starting with an underscore are probably private.
                if enum_name.starts_with('_') {
                    continue;
                }

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
}
