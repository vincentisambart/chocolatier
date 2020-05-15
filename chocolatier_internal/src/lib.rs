#![warn(rust_2018_idioms)]
// TODO:
// - proper handling of protocol optional methods

mod custom_parse;

use chocolatier_objc_parser as objc_parser;
use objc_parser::ast::ObjCMethodKind;
use proc_macro2::{Ident, Span, TokenStream};
use quote::quote;
use quote::ToTokens;
use std::collections::HashMap;
use syn::parse_quote;
use syn::visit_mut::VisitMut;

fn is_import_macro(mac: &syn::ItemMacro) -> bool {
    mac.mac.path.is_ident("import")
}

fn read_imports(items: &[syn::Item]) -> syn::Result<Vec<custom_parse::Import>> {
    let imports = items
        .iter()
        .filter_map(|item| match item {
            syn::Item::Macro(mac) if is_import_macro(mac) => Some(mac),
            _ => None,
        })
        .map(|mac| syn::parse2::<custom_parse::Import>(mac.mac.tokens.clone()))
        .collect::<syn::Result<Vec<_>>>()?;

    Ok(imports)
}

fn replace_imports(items: &mut [syn::Item], c_funcs: &HashMap<String, HashMap<String, CFunc>>) {
    use custom_parse::Import;

    for (i, item) in items.iter_mut().enumerate() {
        let mac = match item {
            syn::Item::Macro(mac) if is_import_macro(mac) => mac,
            _ => continue,
        };

        // We can unwrap as read_imports already parsed it successfully.
        let import: Import = syn::parse2(mac.mac.tokens.clone()).unwrap();
        let name = match import {
            Import::Framework(framework) => framework.framework.value(),
        };
        let mut funcs = Vec::new();
        if name == "Foundation" {
            if let Some(f) = c_funcs.get(ORIGIN_CORE) {
                funcs.extend(f.values());
            }
            if let Some(f) = c_funcs.get(ORIGIN_SYSTEM) {
                funcs.extend(f.values());
            }
        }
        if i == 0 {
            if let Some(f) = c_funcs.get(UNKNOWN_ORIGIN) {
                funcs.extend(f.values());
            }
        }
        if let Some(f) = c_funcs.get(&name) {
            funcs.extend(f.values());
        }
        let func_decls = funcs.iter().map(|f| {
            // TODO: Use a better span.
            let f_name = Ident::new(&f.name, Span::call_site());
            quote! {
                fn #f_name(class: std::ptr::NonNull<choco_runtime::ObjCClass>) -> std::ptr::NonNull<choco_runtime::ObjCObject>;
            }
        });

        // In fact the functions themselves are not in the framework we specify, but they use it.
        let replacement_item: syn::Item = parse_quote! {
            #[link(name = #name, kind = "framework")]
            extern "C" {
                #(#func_decls)*
            }
        };
        *item = replacement_item;
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
enum ObjCMacroUseImplKind {
    Protocol(syn::Ident),
    Interface(syn::Ident),
}

#[derive(Debug)]
struct ObjCMacroUse {
    impl_kind: ObjCMacroUseImplKind,
    expr: custom_parse::ObjCExpr,
    expr_mac: syn::ExprMacro,
}

struct CFunc {
    name: String,
    resolved_use: ObjCMacroResolvedUse,
}

struct ObjCMacroReplacer<'idx> {
    objc_index: &'idx objc_parser::index::TypeIndex,
    errs: Vec<syn::Error>,
    impl_kind: Option<ObjCMacroUseImplKind>,
    c_funcs: HashMap<String, HashMap<String, CFunc>>,
}

impl ObjCMacroReplacer<'_> {
    fn process_objc_macro(&mut self, expr_mac: &mut syn::ExprMacro) -> syn::Result<syn::Expr> {
        use custom_parse::{ObjCExpr, ObjCMethodParams};

        let impl_kind = match &self.impl_kind {
            Some(impl_kind) => impl_kind,
            None => {
                return Err(syn::Error::new_spanned(
                    expr_mac,
                    "objc!() must be used in #[chocolatier] marked impl",
                ))
            }
        };
        // Note that we do not support nesting, as it increases complexity and is probably not useful.
        let objc_expr: custom_parse::ObjCExpr = expr_mac.mac.parse_body()?;
        let mac_use = ObjCMacroUse {
            impl_kind: impl_kind.clone(),
            expr: objc_expr,
            expr_mac: expr_mac.clone(),
        };

        let resolved = resolve_use(self.objc_index, &mac_use)?;

        let func_name_span = match &mac_use.expr {
            ObjCExpr::MethodCall(call) => match &call.params {
                ObjCMethodParams::Without(ident) => ident.span(),
                ObjCMethodParams::With(params) => params[0].0.as_ref().unwrap().span(),
            },
            ObjCExpr::PropertyGet(_) => todo!("{}:{}", file!(), line!()),
            ObjCExpr::PropertySet(_) => todo!("{}:{}", file!(), line!()),
        };

        let (func_name, origin_name) = c_func_name(self.objc_index, &resolved);

        let c_func = self
            .c_funcs
            .entry(origin_name)
            .or_insert(HashMap::new())
            .entry(func_name.clone())
            .or_insert_with(|| CFunc {
                name: func_name,
                resolved_use: resolved,
            });

        let func_ident = Ident::new(&c_func.name, func_name_span);

        let mut params: Vec<syn::Expr> = Vec::new();

        match c_func.resolved_use.method.kind {
            ObjCMethodKind::Class => match &mac_use.expr.receiver() {
                custom_parse::ObjCReceiver::SelfValue(_) => todo!(),
                custom_parse::ObjCReceiver::SelfType(_) => {
                    params.push(parse_quote!(<Self as choco_runtime::ObjCPtr>::class()))
                }
                custom_parse::ObjCReceiver::Class(_) => todo!("{}:{}", file!(), line!()),
                custom_parse::ObjCReceiver::MethodCall(_) => todo!("{}:{}", file!(), line!()),
            },
            ObjCMethodKind::Instance => params.push(parse_quote!(self)),
        }

        match &mac_use.expr {
            ObjCExpr::MethodCall(call) => match &call.params {
                ObjCMethodParams::Without(_) => {}
                ObjCMethodParams::With(mac_params) => {
                    params.extend(mac_params.iter().map(|(_, _, expr)| expr.clone()));
                }
            },
            ObjCExpr::PropertyGet(_) => {}
            ObjCExpr::PropertySet(set) => params.push(set.val_expr.clone()),
        }

        Ok(
            parse_quote! {
                unsafe {
                    <Self as choco_runtime::ObjCPtr>::from_raw_unchecked(#func_ident(#(#params),*))
                }
            },
        )
    }
}

impl syn::visit_mut::VisitMut for ObjCMacroReplacer<'_> {
    fn visit_item_impl_mut(&mut self, imp: &'_ mut syn::ItemImpl) {
        use custom_parse::ItemArgs;

        let (_, args) = match read_chocolatier_attr(&imp.attrs) {
            Ok(Some((attr, args))) => (attr, args),
            Ok(None) => return,
            Err(err) => {
                self.errs.push(err);
                return;
            }
        };

        let mut new_impl_kind = match args {
            ItemArgs::Enum(_) => return,
            ItemArgs::Protocol(protocol) => {
                Some(ObjCMacroUseImplKind::Protocol(protocol.objc_name))
            }
            ItemArgs::Interface(interface) => {
                Some(ObjCMacroUseImplKind::Interface(interface.objc_name))
            }
        };
        std::mem::swap(&mut self.impl_kind, &mut new_impl_kind);

        syn::visit_mut::visit_item_impl_mut(self, imp);

        std::mem::swap(&mut self.impl_kind, &mut new_impl_kind);
    }

    fn visit_expr_mut(&mut self, expr: &mut syn::Expr) {
        let expr_mac = match expr {
            syn::Expr::Macro(expr_mac) if expr_mac.mac.path.is_ident("objc") => expr_mac,
            _ => return syn::visit_mut::visit_expr_mut(self, expr),
        };

        match self.process_objc_macro(expr_mac) {
            Ok(new_expr) => *expr = new_expr,
            Err(err) => {
                self.errs.push(err);
            }
        }
    }
}

fn replace_objc_macro_uses(
    objc_index: &objc_parser::index::TypeIndex,
    items: &mut [syn::Item],
) -> syn::Result<HashMap<String, HashMap<String, CFunc>>> {
    let mut replacer = ObjCMacroReplacer {
        objc_index: objc_index,
        errs: Vec::new(),
        impl_kind: None,
        c_funcs: HashMap::new(),
    };
    for item in items.iter_mut() {
        replacer.visit_item_mut(item);
        if let Some(err) = replacer.errs.drain(..).next() {
            return Err(err);
        }
        assert!(replacer.impl_kind.is_none());
    }
    Ok(replacer.c_funcs)
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

#[derive(Debug, Clone)]
struct ObjCMacroResolvedUse {
    receiver: ResolvedReceiver,
    method: objc_parser::ast::ObjCMethod,
    src_macro: syn::ExprMacro,
}

fn resolve_method(
    objc_index: &objc_parser::index::TypeIndex,
    expr_mac: &syn::ExprMacro,
    receiver: ResolvedReceiver,
    sel: &str,
) -> syn::Result<Option<ObjCMacroResolvedUse>> {
    match receiver {
        ResolvedReceiver::Interface {
            ref name,
            method_kind,
        } => {
            let def = match objc_index.interfaces.get(&name.to_string()) {
                Some(def) => def,
                None => {
                    return Err(syn::Error::new_spanned(
                        expr_mac,
                        format!("could not find any interface named {}", name),
                    ));
                }
            };
            if let Some(method) = def
                .methods
                .iter()
                .find(|method| method.name == sel && method.kind == method_kind)
            {
                return Ok(Some(ObjCMacroResolvedUse {
                    receiver: receiver,
                    method: method.clone(),
                    src_macro: expr_mac.clone(),
                }));
            }

            if let Some(superclass) = &def.superclass {
                let superclass_receiver = ResolvedReceiver::Interface {
                    name: superclass.clone(),
                    method_kind,
                };
                if let Some(resolved_use) =
                    resolve_method(objc_index, expr_mac, superclass_receiver, sel)?
                {
                    return Ok(Some(resolved_use));
                }
            }

            for protocol in &def.adopted_protocols {
                let protocol_receiver = ResolvedReceiver::Protocol {
                    name: protocol.clone(),
                    method_kind,
                };
                if let Some(resolved_use) =
                    resolve_method(objc_index, expr_mac, protocol_receiver, sel)?
                {
                    return Ok(Some(resolved_use));
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
                        expr_mac,
                        format!("could not find any interface named {}", name),
                    ));
                }
            };
            if let Some(method) = def
                .methods
                .iter()
                .find(|method| method.method.name == sel && method.method.kind == method_kind)
            {
                return Ok(Some(ObjCMacroResolvedUse {
                    receiver: receiver,
                    method: method.method.clone(),
                    src_macro: expr_mac.clone(),
                }));
            }

            for protocol in &def.inherited_protocols {
                let protocol_receiver = ResolvedReceiver::Protocol {
                    name: protocol.clone(),
                    method_kind,
                };
                if let Some(resolved_use) =
                    resolve_method(objc_index, expr_mac, protocol_receiver, sel)?
                {
                    return Ok(Some(resolved_use));
                }
            }
            Ok(None)
        }
    }
}

fn resolve_use(
    objc_index: &objc_parser::index::TypeIndex,
    mac_use: &ObjCMacroUse,
) -> syn::Result<ObjCMacroResolvedUse> {
    use custom_parse::ObjCReceiver;

    let receiver = match mac_use.expr.receiver() {
        ObjCReceiver::SelfValue(_) => match &mac_use.impl_kind {
            ObjCMacroUseImplKind::Interface(interface) => ResolvedReceiver::Interface {
                name: interface.to_string(),
                method_kind: ObjCMethodKind::Instance,
            },
            ObjCMacroUseImplKind::Protocol(protoc) => ResolvedReceiver::Protocol {
                name: protoc.to_string(),
                method_kind: ObjCMethodKind::Instance,
            },
        },
        ObjCReceiver::SelfType(_) => match &mac_use.impl_kind {
            ObjCMacroUseImplKind::Interface(interface) => ResolvedReceiver::Interface {
                name: interface.to_string(),
                method_kind: ObjCMethodKind::Class,
            },
            ObjCMacroUseImplKind::Protocol(protocol) => ResolvedReceiver::Protocol {
                name: protocol.to_string(),
                method_kind: ObjCMethodKind::Class,
            },
        },
        ObjCReceiver::Class(interface) => ResolvedReceiver::Interface {
            name: interface.to_string(),
            method_kind: ObjCMethodKind::Class,
        },
        ObjCReceiver::MethodCall(_) => todo!("{}:{}", file!(), line!()),
    };
    match &mac_use.expr {
        custom_parse::ObjCExpr::MethodCall(call) => {
            let sel = call.selector();
            match resolve_method(objc_index, &mac_use.expr_mac, receiver.clone(), &sel)? {
                Some(resolved_use) => Ok(resolved_use),
                None => {
                    let method_kind_symbol = match receiver.method_kind() {
                        ObjCMethodKind::Class => "+",
                        ObjCMethodKind::Instance => "-",
                    };
                    return Err(syn::Error::new_spanned(
                        &mac_use.expr_mac,
                        format!(
                            "could not find method {}[{} {}]",
                            method_kind_symbol,
                            receiver.name(),
                            sel
                        ),
                    ));
                }
            }
        }
        custom_parse::ObjCExpr::PropertyGet(_) => todo!("{}:{}", file!(), line!()),
        custom_parse::ObjCExpr::PropertySet(_) => todo!("{}:{}", file!(), line!()),
    }
}

const UNKNOWN_ORIGIN: &'static str = "_";
const ORIGIN_CORE: &'static str = "core";
const ORIGIN_SYSTEM: &'static str = "system";

fn c_func_name(
    objc_index: &objc_parser::index::TypeIndex,
    resolved_use: &ObjCMacroResolvedUse,
) -> (String, String) {
    use objc_parser::ast::Origin;

    let (receiver_name, method_kind, origin, receiver_kind_name) = match &resolved_use.receiver {
        ResolvedReceiver::Interface { name, method_kind } => {
            let origin = objc_index
                .interfaces
                .get(name.as_str())
                .unwrap()
                .origin
                .clone();
            (name, method_kind, origin, "Protocol")
        }
        ResolvedReceiver::Protocol { name, method_kind } => {
            let origin = objc_index
                .protocols
                .get(name.as_str())
                .unwrap()
                .origin
                .clone();
            (name, method_kind, origin, "Interface")
        }
    };
    let origin_name = match &origin {
        None => UNKNOWN_ORIGIN,
        Some(Origin::ObjCCore) => ORIGIN_CORE,
        Some(Origin::System) => ORIGIN_SYSTEM,
        Some(Origin::Framework(framework)) => framework.as_str(),
        Some(Origin::Library(lib)) => lib.as_str(),
    };
    let method_kind_name = match method_kind {
        ObjCMethodKind::Class => "class",
        ObjCMethodKind::Instance => "instance",
    };
    let escaped_sel = resolved_use.method.name.replace(":", "_");

    // TODO: Also add the name of the crate generating the code.
    // We might have to use env!("CARGO_PKG_NAME") as-is, in the generated code.
    // Also make sure that it works when generating the ObjC code.

    (
        format!(
            "choco_{origin_name}_{receiver_name}{receiver_kind_name}_{method_kind_name}_{escaped_sel}",
            origin_name = origin_name,
            receiver_name = receiver_name,
            receiver_kind_name = receiver_kind_name,
            method_kind_name = method_kind_name,
            escaped_sel = escaped_sel
        ),
        origin_name.to_string(),
    )
}

fn parse_imported_objc(
    imports: &[custom_parse::Import],
) -> syn::Result<objc_parser::index::TypeIndex> {
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

fn process_trait_item(
    _objc_index: &objc_parser::index::TypeIndex,
    struc: &mut syn::ItemTrait,
) -> syn::Result<()> {
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

fn process_struct_item(
    _objc_index: &objc_parser::index::TypeIndex,
    struc: &mut syn::ItemStruct,
) -> syn::Result<()> {
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

fn get_objc_enum_defs<'objc>(
    objc_index: &'objc objc_parser::index::TypeIndex,
    objc_name: &Ident,
) -> syn::Result<Vec<&'objc objc_parser::ast::EnumDef>> {
    use objc_parser::ast;

    let mut enum_defs = Vec::new();
    let str_name = objc_name.to_string();
    let tag_id = ast::TagId::Named(str_name.clone());
    match objc_index.enums.get(&tag_id) {
        Some(objc_def) => enum_defs.push(objc_def),
        None => match objc_index.typedefs.get(&str_name) {
            Some(_) => {}
            None => {
                return Err(syn::Error::new_spanned(
                    objc_name,
                    format!("could not find enum type {}", str_name),
                ));
            }
        },
    }

    for enu in objc_index.enums.values() {
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
    objc_index: &objc_parser::index::TypeIndex,
    enum_args: &custom_parse::EnumArgs,
    imp: &mut syn::ItemImpl,
) -> syn::Result<()> {
    use syn::spanned::Spanned;

    let defs = get_objc_enum_defs(&objc_index, &enum_args.objc_name)?;
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

fn process_impl_item(
    objc_index: &objc_parser::index::TypeIndex,
    imp: &mut syn::ItemImpl,
) -> syn::Result<()> {
    // Only interested in impl blocks marked "#[chocolatier(xxxx = xxxxx)]".
    let (_, args) = match extract_chocolatier_attr(&mut imp.attrs)? {
        Some((attr, args)) => (attr, args),
        None => return Ok(()),
    };

    match args {
        custom_parse::ItemArgs::Enum(enum_args) => process_enum_impl(objc_index, &enum_args, imp)?,
        _ => {}
    }

    Ok(())
}

pub fn chocolatier(input: TokenStream) -> syn::Result<TokenStream> {
    let item_mod: syn::ItemMod = syn::parse2(input)?;

    if item_mod.content.is_none() {
        return Err(syn::Error::new(
            Span::call_site(),
            "chocolatier must not be used on empty module declaration",
        ));
    }

    let (_, ref items) = item_mod.content.as_ref().unwrap();

    let imports = read_imports(items)?;

    if imports.is_empty() {
        return Err(syn::Error::new(
            Span::call_site(),
            "chocolatier requires at least one import!()",
        ));
    }

    let objc_index = parse_imported_objc(&imports)?;

    let mut rewritten_item_mod = item_mod.clone();

    let (_, ref mut mut_items) = rewritten_item_mod.content.as_mut().unwrap();

    let c_funcs = replace_objc_macro_uses(&objc_index, mut_items)?;

    for item in mut_items.iter_mut() {
        match item {
            syn::Item::Struct(struc) => process_struct_item(&objc_index, struc)?,
            syn::Item::Impl(imp) => process_impl_item(&objc_index, imp)?,
            syn::Item::Trait(trai) => process_trait_item(&objc_index, trai)?,
            _ => {}
        }
    }

    replace_imports(mut_items, &c_funcs);

    Ok(rewritten_item_mod.into_token_stream())
}
