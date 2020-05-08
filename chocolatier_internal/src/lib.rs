use proc_macro2::{Ident, TokenStream};
use quote::ToTokens;
use syn::parse::{Parse, ParseStream};

#[derive(Debug)]
struct FrameworkImport {
    framework_token: Ident,
    eq_token: syn::token::Eq,
    framework: syn::LitStr,
}

impl Parse for FrameworkImport {
    fn parse(input: ParseStream) -> syn::Result<Self> {
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
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self::Framework(input.parse()?))
    }
}

fn is_import_macro(mac: &syn::ItemMacro) -> bool {
    mac.mac.path.is_ident("import")
}

fn extract_imports(items: Vec<syn::Item>) -> syn::Result<Vec<syn::Item>> {
    let (imports, rest): (Vec<_>, Vec<_>) = items.into_iter().partition(|item| match item {
        syn::Item::Macro(mac) if is_import_macro(mac) => true,
        _ => false,
    });

    let _imports = imports
        .into_iter()
        .map(|import| {
            let mac = match import {
                syn::Item::Macro(mac) if is_import_macro(&mac) => mac,
                _ => unreachable!(),
            };
            syn::parse2::<Import>(mac.mac.tokens)
        })
        .collect::<syn::Result<Vec<_>>>()?;

    Ok(rest)
}

pub fn chocolatier(mut item: syn::ItemMod) -> syn::Result<TokenStream> {
    if let Some((brace, items)) = item.content {
        let items = extract_imports(items)?;
        item.content = Some((brace, items));
    }

    Ok(item.into_token_stream())
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
