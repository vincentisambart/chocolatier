#![warn(rust_2018_idioms)]
// TODO:
// - proper handling of protocol optional methods
// - consumed self proper handling

mod custom_parse;

use chocolatier_objc_parser::{ast as objc_ast, index as objc_index, xcode};
use objc_ast::ObjCMethodKind;
use proc_macro2::{Ident, Span, TokenStream};
use quote::{format_ident, quote, ToTokens};
use std::collections::HashMap;
use std::io::{Read, Write};
use syn::parse_quote;
use syn::visit::Visit;
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

fn generate_objc_func_decl(c_func: &CFunc) -> TokenStream {
    // TODO: Use a better span.
    let func_name = Ident::new(&c_func.name, Span::call_site());

    let mut params = Vec::new();
    match c_func.resolved_use.method.kind {
        ObjCMethodKind::Class => {
            params.push(quote!(class: std::ptr::NonNull<choco_runtime::ObjCClass>))
        }
        ObjCMethodKind::Instance => {
            params.push(quote!(self_: std::ptr::NonNull<choco_runtime::ObjCObject>))
        }
    }
    params.extend(c_func.resolved_use.method.params.iter().map(|param| {
        let name = Ident::new(&param.name, Span::call_site());
        match &param.ty.ty {
            objc_ast::Type::ObjPtr(ptr) => match ptr.nullability {
                Some(objc_ast::Nullability::NonNull) => quote! {
                    #name: std::ptr::NonNull<choco_runtime::ObjCObject>,
                },
                Some(objc_ast::Nullability::Nullable) | None => quote! {
                    #name: *mut choco_runtime::ObjCObject,
                },
            },
            _ => todo!("{}:{}", file!(), line!()),
        }
    }));

    let ret = match &c_func.resolved_use.method.result.ty {
        objc_ast::Type::Void => None,
        objc_ast::Type::Typedef(typedef) => match typedef.name.as_str() {
            "BOOL" => {
                assert_eq!(c_func.resolved_use.method.result.size, Some(1));
                Some(quote!(i8))
            }
            // TODO: Should probably be nullable here and make Rust check it was really not null.
            "instancetype" => Some(quote! {
                std::ptr::NonNull<choco_runtime::ObjCObject>
            }),
            _ => todo!("{}:{}", file!(), line!()),
        },
        // objc_ast::Type::ObjPtr(ptr) => {}
        _ => todo!("{}:{}", file!(), line!()),
    };

    match ret {
        Some(ret) => quote! {
            fn #func_name(#(#params),*) -> #ret;
        },
        None => quote! {
            fn #func_name(#(#params),*);
        },
    }
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
        let func_decls = funcs.iter().map(|f| generate_objc_func_decl(f));

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
    Protocol(Ident),
    Interface(Ident),
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
    objc_index: &'idx objc_index::TypeIndex,
    errs: Vec<syn::Error>,
    impl_kind: Option<ObjCMacroUseImplKind>,
    c_funcs: HashMap<String, HashMap<String, CFunc>>,
}

fn rust_param_conv(
    rust_objc_param: &custom_parse::ObjCMethodParam,
    objc_param: &objc_ast::ObjCParam,
) -> syn::Expr {
    let expr = &rust_objc_param.expr;
    match &objc_param.ty.ty {
        objc_ast::Type::ObjPtr(ptr) => {
            let mut restric: Vec<TokenStream> = Vec::new();
            restric.push(quote! { choco_runtime::ObjCPtr });
            match &ptr.kind {
                objc_ast::ObjPtrKind::Class => todo!("{}:{}", file!(), line!()),
                objc_ast::ObjPtrKind::Id(id) => {
                    restric.extend(
                        id.protocols
                            .iter()
                            .map(|protoc| format_ident!("{}Protocol", protoc).to_token_stream()),
                    );
                }
                objc_ast::ObjPtrKind::SomeInstance(desc) => {
                    restric.push(format_ident!("{}Interface", desc.interface).to_token_stream());
                    restric.extend(
                        desc.protocols
                            .iter()
                            .map(|protoc| format_ident!("{}Protocol", protoc).to_token_stream()),
                    );
                }
                objc_ast::ObjPtrKind::Block(_) => todo!("{}:{}", file!(), line!()),
                objc_ast::ObjPtrKind::TypeParam(_) => todo!("{}:{}", file!(), line!()),
            };
            let is_consumed = objc_param.attrs.contains(&objc_ast::Attr::NSConsumed);
            match ptr.nullability {
                Some(objc_ast::Nullability::NonNull) => {
                    let received_ty = if is_consumed {
                        quote! { T }
                    } else {
                        quote! { &T }
                    };
                    parse_quote! {
                        ({fn conv<T: #(#restric)+*>(ptr: #received_ty) -> std::ptr::NonNull<choco_runtime::ObjCObject> {
                            ptr.as_raw()
                        }; conv})(#expr)
                    }
                }
                Some(objc_ast::Nullability::Nullable) | None => {
                    let received_ty = if is_consumed {
                        quote! { Option<T> }
                    } else {
                        quote! { Option<&T> }
                    };
                    parse_quote! {
                        ({fn conv<T: #(#restric)+*>(ptr: #received_ty) -> *mut choco_runtime::ObjCObject {
                            ptr.map_or(std::ptr::null_mut(), |ptr| ptr.as_raw().as_ptr())
                        }; conv})(#expr)
                    }
                }
            }
        }
        _ => todo!("{}:{}", file!(), line!()),
    }
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
                ObjCMethodParams::With(params) => params[0].ident.as_ref().unwrap().span(),
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
                custom_parse::ObjCReceiver::SelfValue(_) => todo!("{}:{}", file!(), line!()),
                custom_parse::ObjCReceiver::SelfType(_) => {
                    params.push(parse_quote!(<Self as choco_runtime::ObjCPtr>::class()))
                }
                custom_parse::ObjCReceiver::Class(_) => todo!("{}:{}", file!(), line!()),
                custom_parse::ObjCReceiver::MethodCall(_) => todo!("{}:{}", file!(), line!()),
            },
            ObjCMethodKind::Instance => {
                params.push(parse_quote!(<Self as choco_runtime::ObjCPtr>::as_raw(self)))
            }
        }

        match &mac_use.expr {
            ObjCExpr::MethodCall(call) => match &call.params {
                ObjCMethodParams::Without(_) => {}
                ObjCMethodParams::With(mac_params) => {
                    assert_eq!(mac_params.len(), c_func.resolved_use.method.params.len());
                    params.extend(
                        mac_params
                            .iter()
                            .zip(c_func.resolved_use.method.params.iter())
                            .map(|(rust_objc_param, objc_param)| {
                                rust_param_conv(rust_objc_param, objc_param)
                            }),
                    );
                }
            },
            ObjCExpr::PropertyGet(_) => {}
            ObjCExpr::PropertySet(set) => params.push(set.val_expr.clone()),
        }

        let func_call = quote! {
            #func_ident(#(#params),*)
        };

        let ret_conv = match &c_func.resolved_use.method.result.ty {
            objc_ast::Type::Typedef(typedef) => match typedef.name.as_str() {
                "BOOL" => quote!(#func_call != 0),
                "instancetype" => quote! {
                    <Self as choco_runtime::ObjCPtr>::from_raw_unchecked(#func_call)
                },
                _ => todo!("{}:{}", file!(), line!()),
            },
            _ => todo!("{}:{}", file!(), line!()),
        };

        Ok(parse_quote! {
            unsafe { #ret_conv }
        })
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
    objc_index: &objc_index::TypeIndex,
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
    method: objc_ast::ObjCMethod,
    src_macro: syn::ExprMacro,
}

fn resolve_method(
    objc_index: &objc_index::TypeIndex,
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
    objc_index: &objc_index::TypeIndex,
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
    objc_index: &objc_index::TypeIndex,
    resolved_use: &ObjCMacroResolvedUse,
) -> (String, String) {
    use objc_ast::Origin;

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

fn parse_imported_objc(imports: &[custom_parse::Import]) -> syn::Result<objc_index::TypeIndex> {
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
    let target = xcode::Target::MacOsX86_64;
    let ast = match objc_ast::ast_from_str(target, &objc_code) {
        Ok(ast) => ast,
        Err(err) => return Err(syn::Error::new(Span::call_site(), err.to_string())),
    };
    let index = objc_index::TypeIndex::new(&ast);

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
        if let Ok(arg) = attr.parse_args::<Ident>() {
            arg == "transparent"
        } else {
            false
        }
    } else {
        false
    }
}

fn signed_or_not_to_lit(val: objc_ast::SignedOrNotInt) -> proc_macro2::Literal {
    use objc_ast::SignedOrNotInt;
    use proc_macro2::Literal;

    match val {
        SignedOrNotInt::Signed(i) => Literal::i64_unsuffixed(i),
        SignedOrNotInt::Unsigned(u) => Literal::u64_unsuffixed(u),
    }
}

fn process_trait_item(
    _objc_index: &objc_index::TypeIndex,
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
    _objc_index: &objc_index::TypeIndex,
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
    objc_index: &'objc objc_index::TypeIndex,
    objc_name: &Ident,
) -> syn::Result<Vec<&'objc objc_ast::EnumDef>> {
    let mut enum_defs = Vec::new();
    let str_name = objc_name.to_string();
    let tag_id = objc_ast::TagId::Named(str_name.clone());
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
            objc_ast::Type::Typedef(typedef) if typedef.name == str_name => {}
            objc_ast::Type::Tag(tag) if tag.id == tag_id => {}
            _ => continue,
        }
        enum_defs.push(enu);
    }

    Ok(enum_defs)
}

fn process_enum_impl(
    objc_index: &objc_index::TypeIndex,
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
    objc_index: &objc_index::TypeIndex,
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

pub fn objc_ptr_derive(input: TokenStream) -> syn::Result<TokenStream> {
    let struc: syn::ItemStruct = syn::parse2(input)?;

    if !struc.attrs.iter().any(is_repr_transparent) {
        return Err(syn::Error::new(
            Span::call_site(),
            "chocolatier structs are expected to be #[repr(transparent)]",
        ));
    }

    let name = struc.ident;
    let zero_terminated_name_str = format!("{}\0", name);
    let first_field = match struc.fields.iter().next() {
        Some(field) => field,
        None => {
            return Err(syn::Error::new(
                Span::call_site(),
                "ObjCPtr derive requires one UntypedObjCPtr field",
            ))
        }
    };
    let first_field_access = match &first_field.ident {
        Some(ident) => ident.to_token_stream(),
        None => quote!(0),
    };

    let creation = match &first_field.ident {
        // TODO: Handle PhantomData fields.
        Some(ident) => quote! {
            Self {
                #ident: choco_runtime::UntypedObjCPtr::from_raw_unchecked(ptr),
            }
        },
        None => quote! {
            Self(choco_runtime::UntypedObjCPtr::from_raw_unchecked(ptr))
        },
    };

    Ok(quote! {
        impl choco_runtime::ObjCPtr for #name {
            fn class() -> std::ptr::NonNull<choco_runtime::ObjCClass> {
                use std::sync::atomic::{AtomicPtr, Ordering};
                static CLASS: AtomicPtr<choco_runtime::ObjCClass> =
                    AtomicPtr::new(std::ptr::null_mut());
                let class = CLASS.load(Ordering::Acquire);
                if let Some(class) = std::ptr::NonNull::new(class) {
                    return class;
                }
                let class = unsafe { choco_runtime::objc_getClass(#zero_terminated_name_str.as_ptr()) };
                CLASS.store(class, Ordering::Release);
                std::ptr::NonNull::new(class).unwrap()
            }
            unsafe fn from_raw_unchecked(ptr: std::ptr::NonNull<choco_runtime::ObjCObject>) -> Self {
                #creation
            }
            fn as_raw(&self) -> std::ptr::NonNull<choco_runtime::ObjCObject> {
                self.#first_field_access.as_raw()
            }
        }
    })
}

struct ModVisit<'a> {
    out_file: &'a mut std::fs::File,
}

impl Visit<'_> for ModVisit<'_> {
    fn visit_item_mod(&mut self, item_mod: &'_ syn::ItemMod) {
        if !item_mod
            .attrs
            .iter()
            .any(|attr| attr.path.is_ident("chocolatier"))
        {
            syn::visit::visit_item_mod(self, item_mod);
            return;
        }

        if item_mod.content.is_none() {
            panic!("chocolatier must not be used on empty module declaration");
        }

        let (_, ref items) = item_mod.content.as_ref().unwrap();

        let imports = read_imports(items).unwrap();

        if imports.is_empty() {
            panic!("chocolatier requires at least one import!()");
        }

        for import in &imports {
            match import {
                custom_parse::Import::Framework(import) => writeln!(
                    self.out_file,
                    "#import <{name}/{name}.h>",
                    name = import.framework.value(),
                )
                .unwrap(),
            }
        }

        let objc_index = parse_imported_objc(&imports).unwrap();

        let mut rewritten_item_mod = item_mod.clone();

        let (_, ref mut mut_items) = rewritten_item_mod.content.as_mut().unwrap();

        let c_funcs = replace_objc_macro_uses(&objc_index, mut_items).unwrap();

        for c_func in c_funcs.values().flat_map(|h| h.values()) {
            let ret_ty = match &c_func.resolved_use.method.result.ty {
                objc_ast::Type::Typedef(typedef) => match typedef.name.as_str() {
                    "BOOL" => "BOOL",
                    "instancetype" => "id",
                    _ => todo!("{}:{}", file!(), line!()),
                },
                _ => todo!("{}:{}", file!(), line!()),
            };

            let mut param_decls = Vec::new();
            match c_func.resolved_use.method.kind {
                ObjCMethodKind::Class => param_decls.push("Class klass".to_string()),
                // TODO: self should have a proper type
                ObjCMethodKind::Instance => {
                    param_decls.push("__unsafe_unretained id _Nonnull self_".to_string())
                }
            }

            for param in &c_func.resolved_use.method.params {
                match &param.ty.ty {
                    objc_ast::Type::ObjPtr(ptr) => {
                        let nullability = match ptr.nullability {
                            Some(objc_ast::Nullability::NonNull) => " _Nonnull",
                            Some(objc_ast::Nullability::Nullable) => " _Nullable",
                            None => "",
                        };
                        let ty = match &ptr.kind {
                            objc_ast::ObjPtrKind::Class => "Class".to_string(),
                            objc_ast::ObjPtrKind::Id(id) => {
                                if id.protocols.is_empty() {
                                    format!("__unsafe_unretained id{}", nullability)
                                } else {
                                    format!(
                                        "__unsafe_unretained id<{}>{}",
                                        id.protocols.join(", "),
                                        nullability
                                    )
                                }
                            }
                            objc_ast::ObjPtrKind::SomeInstance(_) => {
                                todo!("{}:{}", file!(), line!())
                            }
                            objc_ast::ObjPtrKind::Block(_) => todo!("{}:{}", file!(), line!()),
                            objc_ast::ObjPtrKind::TypeParam(_) => todo!("{}:{}", file!(), line!()),
                        };
                        param_decls.push(format!("{} {}", ty, param.name));
                    }
                    _ => todo!("{}:{}", file!(), line!()),
                }
            }

            let call_target = match c_func.resolved_use.method.kind {
                ObjCMethodKind::Class => "klass",
                ObjCMethodKind::Instance => "self_",
            };

            let call;
            if c_func.resolved_use.method.params.is_empty() {
                call = format!("[{} {}]", call_target, c_func.resolved_use.method.name);
            } else {
                let params_passing = c_func
                    .resolved_use
                    .method
                    .name
                    .split_terminator(":")
                    .zip(&c_func.resolved_use.method.params)
                    .map(|(part, param)| format!("{}:{}", part, param.name))
                    .collect::<Vec<_>>()
                    .join(" ");

                call = format!("[{} {}]", call_target, params_passing);
            }

            writeln!(
                self.out_file,
                "\
{ret_ty} {name}({param_decls}) {{
    @try {{
        return {call};
    }}
    @catch (NSException *exception) {{
        abort_on_exception(exception);
    }}
}}
",
                ret_ty = ret_ty,
                name = c_func.name,
                param_decls = param_decls.join(", "),
                call = call,
            )
            .unwrap();
        }
    }
}

pub fn build_objc_for_files<T: AsRef<str>>(files: &[T]) {
    let out_dir = std::env::var("OUT_DIR").unwrap();
    let out_dir = std::path::Path::new(&out_dir);
    let out_file_path = out_dir.join("chocolatier-generated.m");
    let mut out_file = std::fs::File::create(&out_file_path).expect("unable to open output file");

    writeln!(
        &mut out_file,
        r#"// File automatically generated by chocolatier - Do not modify directly.
#import <objc/runtime.h>
#import <Foundation/NSObjCRuntime.h> // for declaration of NSLog
#import <Foundation/NSException.h>
#import <stdlib.h> // for declaration of abort()

#if !__has_feature(objc_arc)
#error This file must be compiled with ARC turned on (-fobjc-arc)
#endif

static void abort_on_exception(__unsafe_unretained NSException *exception)
__attribute__((noreturn));

static void abort_on_exception(__unsafe_unretained NSException *exception) {{
    NSLog(@"Unexpected exception: %@", exception.reason);
    abort();
}}
"#
    )
    .unwrap();

    for filename in files {
        println!("cargo:rerun-if-changed={}", filename.as_ref());

        let mut file = std::fs::File::open(filename.as_ref()).expect("unable to open file");
        let mut src = String::new();
        file.read_to_string(&mut src).expect("unable to read file");
        let parsed_file = syn::parse_file(&src).expect("unable to parse file");

        let mut visit = ModVisit {
            out_file: &mut out_file,
        };
        visit.visit_file(&parsed_file);
    }

    cc::Build::new()
        .file(out_file_path)
        .flag("-fobjc-arc")
        .compile("chocolatier-generated");
}
