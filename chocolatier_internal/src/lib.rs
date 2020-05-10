#![warn(rust_2018_idioms)]
// TODO:
// - proper handling of protocol optional methods

mod custom_parse;

use chocolatier_objc_parser as objc_parser;
use objc_parser::ast::ObjCMethodKind;
use proc_macro2::{Ident, Span, TokenStream};
use quote::ToTokens;
use syn::visit::Visit;

fn is_import_macro(mac: &syn::ItemMacro) -> bool {
    mac.mac.path.is_ident("import")
}

fn empty_item() -> syn::Item {
    syn::Item::Verbatim(TokenStream::new())
}

// TODO: It might be nice to put the 'extern "C"' block where the first import was.
fn extract_imports(items: &mut [syn::Item]) -> syn::Result<Vec<(usize, custom_parse::Import)>> {
    let mut imports = Vec::new();
    for (idx, item) in items
        .iter_mut()
        .enumerate()
        .filter(|(_, item)| matches!(item, syn::Item::Macro(mac) if is_import_macro(mac)))
    {
        let mut extracted_item = empty_item();
        std::mem::swap(item, &mut extracted_item);
        let mac = match extracted_item {
            syn::Item::Macro(mac) => mac,
            _ => unreachable!(),
        };
        let import = syn::parse2::<custom_parse::Import>(mac.mac.tokens)?;
        imports.push((idx, import))
    }

    Ok(imports)
}

#[derive(Debug, PartialEq, Eq, Clone)]
enum ObjCMacroUseLocation {
    Protocol(syn::Ident),
    Interface(syn::Ident),
}

struct ObjCMacroVisitor {
    errs: Vec<syn::Error>,
    location: Option<ObjCMacroUseLocation>,
    uses: Vec<(ObjCMacroUseLocation, custom_parse::ObjCExpr)>,
}

impl syn::visit::Visit<'_> for ObjCMacroVisitor {
    fn visit_item_impl(&mut self, imp: &'_ syn::ItemImpl) {
        use custom_parse::ItemArgs;

        let (_, args) = match read_chocolatier_attr(&imp.attrs) {
            Ok(Some((attr, args))) => (attr, args),
            Ok(None) => return,
            Err(err) => {
                self.errs.push(err);
                return;
            }
        };

        let mut new_location = match args {
            ItemArgs::Enum(_) => return,
            ItemArgs::Protocol(protocol) => {
                Some(ObjCMacroUseLocation::Protocol(protocol.objc_name))
            }
            ItemArgs::Interface(interface) => {
                Some(ObjCMacroUseLocation::Interface(interface.objc_name))
            }
        };
        std::mem::swap(&mut self.location, &mut new_location);

        syn::visit::visit_item_impl(self, imp);

        std::mem::swap(&mut self.location, &mut new_location);
    }

    fn visit_expr_macro(&mut self, expr_mac: &'_ syn::ExprMacro) {
        if expr_mac.mac.path.is_ident("objc") {
            let location = match &self.location {
                Some(location) => location,
                None => {
                    let err = syn::Error::new_spanned(
                        expr_mac,
                        "objc!() must be used in #[chocolatier] marked impl",
                    );
                    self.errs.push(err);
                    return;
                }
            };
            // Note that we do not support nesting, as it increases complexity and is probably not useful.
            match expr_mac.mac.parse_body::<custom_parse::ObjCExpr>() {
                Ok(expr) => self.uses.push((location.clone(), expr)),
                Err(err) => self.errs.push(err),
            }
        } else {
            syn::visit::visit_expr_macro(self, expr_mac);
        }
    }
}

fn find_objc_macro_uses(
    items: &[syn::Item],
) -> syn::Result<Vec<(ObjCMacroUseLocation, custom_parse::ObjCExpr)>> {
    let mut visitor = ObjCMacroVisitor {
        errs: Vec::new(),
        location: None,
        uses: Vec::new(),
    };
    for item in items {
        visitor.visit_item(&item);
        if let Some(err) = visitor.errs.drain(..).next() {
            return Err(err);
        }
        assert!(visitor.location.is_none());
    }
    Ok(visitor.uses)
}

#[derive(Debug, Clone)]
enum ResolvedReceiver {
    Interface {
        name: String,
        method_kind: ObjCMethodKind,
    },
    Protocol {
        name: String,
        method_kind: ObjCMethodKind,
    },
}

impl ResolvedReceiver {
    fn method_kind(&self) -> ObjCMethodKind {
        match self {
            Self::Interface {
                name: _,
                method_kind,
            } => *method_kind,
            Self::Protocol {
                name: _,
                method_kind,
            } => *method_kind,
        }
    }

    fn name(&self) -> &str {
        match self {
            Self::Interface {
                name,
                method_kind: _,
            } => name,
            Self::Protocol {
                name,
                method_kind: _,
            } => name,
        }
    }
}

fn find_method(
    objc_index: &objc_parser::index::TypeIndex,
    error_spanned: &dyn ToTokens,
    receiver: ResolvedReceiver,
    sel: &str,
) -> syn::Result<Option<(ResolvedReceiver, objc_parser::ast::ObjCMethod)>> {
    match receiver {
        ResolvedReceiver::Interface {
            ref name,
            method_kind,
        } => {
            let def = match objc_index.interfaces.get(&name.to_string()) {
                Some(def) => def,
                None => {
                    return Err(syn::Error::new_spanned(
                        error_spanned,
                        std::format!("could not find any interface named {}", name),
                    ));
                }
            };
            if let Some(method) = def
                .methods
                .iter()
                .find(|method| method.name == sel && method.kind == method_kind)
            {
                return Ok(Some((receiver, method.clone())));
            }

            if let Some(superclass) = &def.superclass {
                let superclass_receiver = ResolvedReceiver::Interface {
                    name: superclass.clone(),
                    method_kind,
                };
                if let Some((real_receiver, method)) =
                    find_method(objc_index, error_spanned, superclass_receiver, sel)?
                {
                    return Ok(Some((real_receiver, method)));
                }
            }

            for protocol in &def.adopted_protocols {
                let protocol_receiver = ResolvedReceiver::Protocol {
                    name: protocol.clone(),
                    method_kind,
                };
                if let Some((real_receiver, method)) =
                    find_method(objc_index, error_spanned, protocol_receiver, sel)?
                {
                    return Ok(Some((real_receiver, method)));
                }
            }
            Ok(None)
        }
        ResolvedReceiver::Protocol {
            ref name,
            method_kind,
        } => {
            let def = match objc_index.protocols.get(&name.to_string()) {
                Some(def) => def,
                None => {
                    return Err(syn::Error::new_spanned(
                        error_spanned,
                        std::format!("could not find any interface named {}", name),
                    ));
                }
            };
            if let Some(method) = def
                .methods
                .iter()
                .find(|method| method.method.name == sel && method.method.kind == method_kind)
            {
                return Ok(Some((receiver, method.method.clone())));
            }

            for protocol in &def.inherited_protocols {
                let protocol_receiver = ResolvedReceiver::Protocol {
                    name: protocol.clone(),
                    method_kind,
                };
                if let Some((real_receiver, method)) =
                    find_method(objc_index, error_spanned, protocol_receiver, sel)?
                {
                    return Ok(Some((real_receiver, method)));
                }
            }
            Ok(None)
        }
    }
}

fn resolve_use(
    objc_index: &objc_parser::index::TypeIndex,
    loc: &ObjCMacroUseLocation,
    expr: &custom_parse::ObjCExpr,
) -> syn::Result<(ResolvedReceiver, objc_parser::ast::ObjCMethod)> {
    use custom_parse::ObjCReceiver;

    let (receiver, receiver_spanned): (ResolvedReceiver, &dyn ToTokens) = match expr.receiver() {
        ObjCReceiver::SelfValue(self_ident) => match loc {
            ObjCMacroUseLocation::Interface(interface) => (
                ResolvedReceiver::Interface {
                    name: interface.to_string(),
                    method_kind: ObjCMethodKind::Instance,
                },
                self_ident,
            ),
            ObjCMacroUseLocation::Protocol(protoc) => (
                ResolvedReceiver::Protocol {
                    name: protoc.to_string(),
                    method_kind: ObjCMethodKind::Instance,
                },
                self_ident,
            ),
        },
        ObjCReceiver::SelfType(self_ident) => match loc {
            ObjCMacroUseLocation::Interface(interface) => (
                ResolvedReceiver::Interface {
                    name: interface.to_string(),
                    method_kind: ObjCMethodKind::Class,
                },
                self_ident,
            ),
            ObjCMacroUseLocation::Protocol(protocol) => (
                ResolvedReceiver::Protocol {
                    name: protocol.to_string(),
                    method_kind: ObjCMethodKind::Class,
                },
                self_ident,
            ),
        },
        ObjCReceiver::Class(interface) => (
            ResolvedReceiver::Interface {
                name: interface.to_string(),
                method_kind: ObjCMethodKind::Class,
            },
            interface,
        ),
        ObjCReceiver::MethodCall(_) => todo!(),
    };
    match expr {
        custom_parse::ObjCExpr::MethodCall(call) => {
            let sel = call.selector();
            match find_method(objc_index, receiver_spanned, receiver.clone(), &sel)? {
                Some((real_receiver, method)) => Ok((real_receiver, method)),
                None => {
                    let method_kind_symbol = match receiver.method_kind() {
                        ObjCMethodKind::Class => "+",
                        ObjCMethodKind::Instance => "-",
                    };
                    return Err(syn::Error::new_spanned(
                        receiver_spanned,
                        std::format!(
                            "could not find method {}[{} {}]",
                            method_kind_symbol,
                            receiver.name(),
                            sel
                        ),
                    ));
                }
            }
        }
        custom_parse::ObjCExpr::PropertyGet(_) => todo!(),
        custom_parse::ObjCExpr::PropertySet(_) => todo!(),
    }
}

fn resolve_uses(
    objc_index: &objc_parser::index::TypeIndex,
    uses: &[(ObjCMacroUseLocation, custom_parse::ObjCExpr)],
) -> syn::Result<()> {
    for (loc, expr) in uses {
        resolve_use(objc_index, loc, expr)?;
    }
    Ok(())
}

fn parse_imported_objc<'a, I>(imports: I) -> syn::Result<objc_parser::index::TypeIndex>
where
    I: Iterator<Item = &'a custom_parse::Import>,
{
    use std::fmt::Write;

    let mut objc_code = String::new();
    for import in imports {
        match import {
            custom_parse::Import::Framework(import) => writeln!(
                &mut objc_code,
                "#import <{name}/{name}.h>",
                name = import.framework.value(),
            )
            .unwrap(),
        }
    }

    // TODO: The target should come from the current target, or use multiple targets (should probably be configurable)
    assert!(cfg!(all(target_os = "macos", target_arch = "x86_64")));
    let target = objc_parser::xcode::Target::MacOsX86_64;
    let ast = match objc_parser::ast::ast_from_str(target, &objc_code) {
        Ok(ast) => ast,
        Err(err) => return Err(syn::Error::new(Span::call_site(), err.to_string())),
    };
    let index = objc_parser::index::TypeIndex::new(&ast);

    Ok(index)
}

fn extract_chocolatier_attr(
    attrs: &mut Vec<syn::Attribute>,
) -> syn::Result<Option<(syn::Attribute, custom_parse::ItemArgs)>> {
    let idx = match attrs
        .iter()
        .position(|attr| attr.path.is_ident("chocolatier"))
    {
        Some(idx) => idx,
        None => return Ok(None),
    };

    let attr = attrs.remove(idx);
    let args = attr.parse_args::<custom_parse::ItemArgs>()?;
    Ok(Some((attr.clone(), args)))
}

fn read_chocolatier_attr(
    attrs: &Vec<syn::Attribute>,
) -> syn::Result<Option<(syn::Attribute, custom_parse::ItemArgs)>> {
    let attr = match attrs
        .iter()
        .filter(|attr| attr.path.is_ident("chocolatier"))
        .next()
    {
        Some(attr) => attr,
        None => return Ok(None),
    };

    let args = attr.parse_args::<custom_parse::ItemArgs>()?;
    Ok(Some((attr.clone(), args)))
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

fn signed_or_not_to_lit(val: objc_parser::ast::SignedOrNotInt) -> proc_macro2::Literal {
    use objc_parser::ast::SignedOrNotInt;
    use proc_macro2::Literal;

    match val {
        SignedOrNotInt::Signed(i) => Literal::i64_unsuffixed(i),
        SignedOrNotInt::Unsigned(u) => Literal::u64_unsuffixed(u),
    }
}

struct RustProcessor {
    objc_index: objc_parser::index::TypeIndex,
}

impl RustProcessor {
    fn process_trait_item(&self, struc: &mut syn::ItemTrait) -> syn::Result<()> {
        let (_, args) = match extract_chocolatier_attr(&mut struc.attrs)? {
            Some((attr, args)) => (attr, args),
            None => return Ok(()),
        };

        match args {
            custom_parse::ItemArgs::Interface(_) | custom_parse::ItemArgs::Protocol(_) => {}
            custom_parse::ItemArgs::Enum(enu) => {
                return Err(syn::Error::new_spanned(
                    enu.enum_token,
                    "traits should not need an enum declaration",
                ));
            }
        }

        Ok(())
    }

    fn process_struct_item(&self, struc: &mut syn::ItemStruct) -> syn::Result<()> {
        let (attr, args) = match extract_chocolatier_attr(&mut struc.attrs)? {
            Some((attr, args)) => (attr, args),
            None => return Ok(()),
        };

        match args {
            custom_parse::ItemArgs::Enum(_) | custom_parse::ItemArgs::Interface(_) => {}
            custom_parse::ItemArgs::Protocol(protocol) => {
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

        // TODO: For enums, check that type of first field is the same as the one of the C type.

        Ok(())
    }

    fn get_objc_enum_defs(
        &self,
        objc_name: &Ident,
    ) -> syn::Result<Vec<&objc_parser::ast::EnumDef>> {
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

    fn process_enum_impl(
        &self,
        enum_args: &custom_parse::EnumArgs,
        imp: &mut syn::ItemImpl,
    ) -> syn::Result<()> {
        use syn::spanned::Spanned;

        let defs = self.get_objc_enum_defs(&enum_args.objc_name)?;
        // For all "const CONST_NAME: Self = ObjCEnumValue;" with ObjCEnumValue being an ObjC enum value,
        // replace it by "Self(xxxx)", with xxxx being the real numerical value in ObjC.
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
                    let mut lit = signed_or_not_to_lit(val.value);
                    lit.set_span(cnst.expr.span());
                    cnst.expr = syn::parse_quote! {Self(#lit)};
                }
            }
        }

        Ok(())
    }

    fn process_impl_item(&self, imp: &mut syn::ItemImpl) -> syn::Result<()> {
        // Only interested in impl blocks marked "#[chocolatier(xxxx = xxxxx)]".
        let (_, args) = match extract_chocolatier_attr(&mut imp.attrs)? {
            Some((attr, args)) => (attr, args),
            None => return Ok(()),
        };

        match args {
            custom_parse::ItemArgs::Enum(enum_args) => self.process_enum_impl(&enum_args, imp)?,
            _ => {}
        }

        Ok(())
    }

    fn process(&self, items: &mut Vec<syn::Item>) -> syn::Result<()> {
        for item in items.iter_mut() {
            match item {
                syn::Item::Struct(struc) => self.process_struct_item(struc)?,
                syn::Item::Impl(imp) => self.process_impl_item(imp)?,
                syn::Item::Trait(trai) => self.process_trait_item(trai)?,
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

    let objc_index = parse_imported_objc(imports.iter().map(|(_, import)| import))?;

    let uses = find_objc_macro_uses(items)?;

    resolve_uses(&objc_index, &uses)?;

    let processor = RustProcessor { objc_index };

    processor.process(items)?;

    Ok(item.into_token_stream())
}
