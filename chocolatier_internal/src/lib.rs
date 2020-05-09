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
    for item in items
        .iter_mut()
        .filter(|item| matches!(item, syn::Item::Macro(mac) if is_import_macro(mac)))
    {
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

#[derive(Debug)]
struct EnumArgs {
    enum_token: syn::Token![enum],
    eq_token: syn::Token![=],
    objc_name: Ident,
}

impl syn::parse::Parse for EnumArgs {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        let enum_token = input.parse::<syn::Token![enum]>()?;
        let eq_token = input.parse::<syn::Token![=]>()?;
        let objc_name = input.parse::<syn::Ident>()?;
        Ok(Self {
            enum_token,
            eq_token,
            objc_name,
        })
    }
}

#[derive(Debug)]
struct InterfaceArgs {
    interface_token: Ident,
    eq_token: syn::token::Eq,
    objc_name: Ident,
}

impl syn::parse::Parse for InterfaceArgs {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        let interface_token = input.parse::<Ident>()?;
        if interface_token != "interface" {
            return Err(syn::Error::new_spanned(
                interface_token,
                "expecting \"interface\"",
            ));
        }
        let eq_token = input.parse::<syn::Token![=]>()?;
        let objc_name = input.parse::<syn::Ident>()?;
        Ok(Self {
            interface_token,
            eq_token,
            objc_name,
        })
    }
}

#[derive(Debug)]
struct ProtocolArgs {
    protocol_token: Ident,
    eq_token: syn::token::Eq,
    objc_name: Ident,
}

impl syn::parse::Parse for ProtocolArgs {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        let protocol_token = input.parse::<Ident>()?;
        if protocol_token != "protocol" {
            return Err(syn::Error::new_spanned(
                protocol_token,
                "expecting \"protocol\"",
            ));
        }
        let eq_token = input.parse::<syn::Token![=]>()?;
        let objc_name = input.parse::<syn::Ident>()?;
        Ok(Self {
            protocol_token,
            eq_token,
            objc_name,
        })
    }
}

#[derive(Debug)]
enum ItemArgs {
    Enum(EnumArgs),
    Protocol(ProtocolArgs),
    Interface(InterfaceArgs),
}

impl syn::parse::Parse for ItemArgs {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        if input.peek(syn::Token![enum]) {
            Ok(Self::Enum(input.parse()?))
        } else if let Some((ident, _)) = input.cursor().ident() {
            if ident == "protocol" {
                Ok(Self::Protocol(input.parse()?))
            } else if ident == "interface" {
                Ok(Self::Interface(input.parse()?))
            } else {
                Err(syn::Error::new_spanned(
                    ident,
                    "expecting enum/protocol/interface",
                ))
            }
        } else {
            Err(syn::Error::new(
                input.cursor().span(),
                "expecting enum/protocol/interface",
            ))
        }
    }
}

fn extract_chocolatier_attr(attrs: &mut Vec<syn::Attribute>) -> Option<syn::Attribute> {
    match attrs
        .iter()
        .position(|attr| attr.path.is_ident("chocolatier"))
    {
        Some(pos) => Some(attrs.remove(pos)),
        None => None,
    }
}

fn is_repr_transparent(attr: &syn::Attribute) -> bool {
    if attr.path.is_ident("repr") {
        if let Ok(arg) = attr.parse_args::<syn::Ident>() {
            arg == "transparent"
        } else {
            false
        }
    } else {
        false
    }
}

struct RustProcessor {
    objc_index: objc_parser::index::TypeIndex,
}

impl RustProcessor {
    fn process_struct(&self, struc: &mut syn::ItemStruct) -> syn::Result<()> {
        let attr = if let Some(attr) = extract_chocolatier_attr(&mut struc.attrs) {
            attr
        } else {
            return Ok(());
        };

        let arg = attr.parse_args::<ItemArgs>()?;
        match arg {
            ItemArgs::Enum(_) | ItemArgs::Interface(_) => {}
            ItemArgs::Protocol(protocol) => {
                return Err(syn::Error::new_spanned(
                    protocol.protocol_token,
                    "protocols should not need a struct declaration",
                ));
            }
        }

        if !struc.attrs.iter().any(is_repr_transparent) {
            return Err(syn::Error::new_spanned(
                attr,
                "chocolatier structs are expected to be #[repr(transparent)]",
            ));
        }

        // TODO: Check that type of first field is the same as the one of the C type.

        Ok(())
    }

    fn get_enum_defs(&self, objc_name: &Ident) -> syn::Result<Vec<&objc_parser::ast::EnumDef>> {
        use objc_parser::ast;

        let mut enum_defs = Vec::new();
        let str_name = objc_name.to_string();
        let tag_id = ast::TagId::Named(str_name.clone());
        match self.objc_index.enums.get(&tag_id) {
            Some(objc_def) => enum_defs.push(objc_def),
            None => match self.objc_index.typedefs.get(&str_name) {
                Some(_) => {}
                None => {
                    return Err(syn::Error::new_spanned(
                        objc_name,
                        std::format!("could not find enum type {}", str_name),
                    ));
                }
            },
        }

        for enu in self.objc_index.enums.values() {
            match &enu.underlying.ty {
                ast::Type::Typedef(typedef) if typedef.name == str_name => {}
                ast::Type::Tag(tag) if tag.id == tag_id => {}
                _ => continue,
            }
            enum_defs.push(enu);
        }

        Ok(enum_defs)
    }

    fn process_enum_impl(&self, enum_args: &EnumArgs, imp: &mut syn::ItemImpl) -> syn::Result<()> {
        use syn::spanned::Spanned;

        let defs = self.get_enum_defs(&enum_args.objc_name)?;
        for item in imp.items.iter_mut() {
            let cnst = match item {
                syn::ImplItem::Const(cnst) => cnst,
                _ => continue,
            };
            let expr_path = match &cnst.expr {
                syn::Expr::Path(expr_path) => expr_path,
                _ => continue,
            };
            if let Some(ident) = expr_path.path.get_ident() {
                if let Some(val) = defs
                    .iter()
                    .filter_map(|def| def.values.iter().filter(|val| ident == &val.name).next())
                    .next()
                {
                    use objc_parser::ast::SignedOrNotInt;
                    let mut lit = match val.value {
                        SignedOrNotInt::Signed(i) => proc_macro2::Literal::i64_unsuffixed(i),
                        SignedOrNotInt::Unsigned(u) => proc_macro2::Literal::u64_unsuffixed(u),
                    };
                    lit.set_span(cnst.expr.span());
                    cnst.expr = syn::parse_quote! {Self(#lit)};
                }
            }
        }

        Ok(())
    }

    fn process_impl(&self, imp: &mut syn::ItemImpl) -> syn::Result<()> {
        let attr = if let Some(attr) = extract_chocolatier_attr(&mut imp.attrs) {
            attr
        } else {
            return Ok(());
        };

        let arg = attr.parse_args::<ItemArgs>()?;
        match arg {
            ItemArgs::Enum(enum_args) => self.process_enum_impl(&enum_args, imp)?,
            _ => {}
        }

        Ok(())
    }

    fn process(&self, items: &mut Vec<syn::Item>) -> syn::Result<()> {
        for item in items.iter_mut() {
            match item {
                syn::Item::Struct(struc) => self.process_struct(struc)?,
                syn::Item::Impl(imp) => self.process_impl(imp)?,
                _ => {}
            }
        }
        Ok(())
    }
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

    let objc_index = parse_objc(&imports)?;

    let processor = RustProcessor { objc_index };

    processor.process(items)?;

    Ok(item.into_token_stream())
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
