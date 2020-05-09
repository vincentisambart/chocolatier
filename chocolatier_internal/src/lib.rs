#![warn(rust_2018_idioms)]

use chocolatier_objc_parser as objc_parser;
use proc_macro2::{Ident, Span, TokenStream};
use quote::ToTokens;
use syn::parse::{Parse, ParseStream};

#[derive(Debug)]
struct FrameworkImport {
    framework_token: Ident,
    eq_token: syn::token::Eq,
    framework: syn::LitStr,
}

impl Parse for FrameworkImport {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        let framework_token = input.parse()?;
        if framework_token != "framework" {
            return Err(syn::Error::new_spanned(
                framework_token,
                format!("expected `framework`"),
            ));
        }
        let eq_token = input.parse()?;
        let framework = input.parse()?;
        Ok(Self {
            framework_token,
            eq_token,
            framework,
        })
    }
}

#[derive(Debug)]
enum Import {
    Framework(FrameworkImport),
}

impl Parse for Import {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        Ok(Self::Framework(input.parse()?))
    }
}

fn is_import_macro(mac: &syn::ItemMacro) -> bool {
    mac.mac.path.is_ident("import")
}

fn empty_item() -> syn::Item {
    syn::Item::Verbatim(TokenStream::new())
}

// TODO: It might be nice to put the 'extern "C"' block where the first import was.
fn extract_imports(items: &mut [syn::Item]) -> syn::Result<Vec<Import>> {
    let mut imports = Vec::new();
    for item in items.iter_mut().filter(|item| match item {
        syn::Item::Macro(mac) if is_import_macro(mac) => true,
        _ => false,
    }) {
        let mut extracted_item = empty_item();
        std::mem::swap(item, &mut extracted_item);
        let mac = match extracted_item {
            syn::Item::Macro(mac) => mac,
            _ => unreachable!(),
        };
        let import = syn::parse2::<Import>(mac.mac.tokens)?;
        imports.push(import)
    }

    Ok(imports)
}

fn parse_objc(imports: &[Import]) -> syn::Result<objc_parser::index::TypeIndex> {
    use std::fmt::Write;

    let mut objc_code = String::new();
    for import in imports {
        match import {
            Import::Framework(import) => writeln!(
                &mut objc_code,
                "#import <{name}/{name}.h>",
                name = import.framework.value(),
            )
            .unwrap(),
        }
    }

    // TODO: The target should come from the current target, or use multiple targets (should probably be configurable)
    let target = objc_parser::xcode::Target::MacOsX86_64;
    let ast = match objc_parser::ast::ast_from_str(target, &objc_code) {
        Ok(ast) => ast,
        Err(err) => return Err(syn::Error::new(Span::call_site(), err.to_string())),
    };
    let index = objc_parser::index::TypeIndex::new(&ast);

    Ok(index)
}

pub fn chocolatier(input: TokenStream) -> syn::Result<TokenStream> {
    let mut item: syn::ItemMod = syn::parse2(input)?;

    let items = if let Some((_, ref mut items)) = item.content {
        items
    } else {
        return Err(syn::Error::new(
            Span::call_site(),
            "chocolatier must not be used on empty module declaration",
        ));
    };

    let imports = extract_imports(items)?;

    if imports.is_empty() {
        return Err(syn::Error::new(
            Span::call_site(),
            "chocolatier requires at least one import!()",
        ));
    }

    parse_objc(&imports)?;

    Ok(item.into_token_stream())
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
