use proc_macro2::Ident;
use std::borrow::Cow;
use syn::parse::{Parse, ParseStream};

#[derive(Debug)]
pub struct FrameworkImport {
    pub framework_token: Ident,
    pub eq_token: syn::token::Eq,
    pub framework: syn::LitStr,
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
pub enum Import {
    Framework(FrameworkImport),
}

impl Parse for Import {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        Ok(Self::Framework(input.parse()?))
    }
}

#[derive(Debug)]
pub struct EnumArgs {
    pub enum_token: syn::Token![enum],
    pub eq_token: syn::Token![=],
    pub objc_name: Ident,
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
pub struct InterfaceArgs {
    pub interface_token: Ident,
    pub eq_token: syn::token::Eq,
    pub objc_name: Ident,
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
pub struct ProtocolArgs {
    pub protocol_token: Ident,
    pub eq_token: syn::token::Eq,
    pub objc_name: Ident,
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
pub enum ItemArgs {
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

#[derive(Debug, PartialEq, Eq)]
pub enum ObjCMethodParams {
    Without(syn::Ident),
    With(Vec<(Option<syn::Ident>, syn::Token![:], syn::Expr)>),
}

#[derive(Debug, PartialEq, Eq)]
pub enum ObjCReceiver {
    SelfValue(syn::Token![self]),
    SelfType(syn::Token![Self]),
    Class(syn::Ident),
    MethodCall(Box<ObjCMethodCall>),
}

impl Parse for ObjCReceiver {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(syn::Token![self]) {
            Ok(Self::SelfValue(input.parse()?))
        } else if lookahead.peek(syn::Token![Self]) {
            Ok(Self::SelfType(input.parse()?))
        } else if lookahead.peek(syn::Ident) {
            Ok(Self::Class(input.parse()?))
        } else if lookahead.peek(syn::token::Bracket) {
            Ok(Self::MethodCall(input.parse()?))
        } else {
            Err(lookahead.error())
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ObjCMethodCall {
    pub bracket_token: syn::token::Bracket,
    pub receiver: ObjCReceiver,
    pub params: ObjCMethodParams,
}

impl ObjCMethodCall {
    pub fn selector(&self) -> String {
        match &self.params {
            ObjCMethodParams::Without(ident) => ident.to_string(),
            ObjCMethodParams::With(params) => params
                .iter()
                .map(|(ident, _, _)| match ident {
                    Some(ident) => Cow::Owned(std::format!("{}:", ident)),
                    None => Cow::Borrowed(":"),
                })
                .collect::<Vec<_>>()
                .join(""),
        }
    }
}

impl Parse for ObjCMethodCall {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        let content;
        let bracket_token = syn::bracketed!(content in input);
        let receiver: ObjCReceiver = content.parse()?;
        let method_name_start: syn::Ident = content.parse()?;
        let params;
        if content.peek(syn::Token![:]) {
            let mut v: Vec<(Option<syn::Ident>, syn::Token![:], syn::Expr)> = Vec::new();

            let colon_token = content.parse()?;
            let expr = content.parse()?;
            v.push((Some(method_name_start), colon_token, expr));

            loop {
                if content.is_empty() {
                    break;
                }

                let ident: Option<syn::Ident> = if content.peek(syn::Ident) {
                    Some(content.parse()?)
                } else {
                    None
                };
                let colon_token = content.parse()?;
                let expr = content.parse()?;
                v.push((ident, colon_token, expr));
            }

            params = ObjCMethodParams::With(v);
        } else {
            params = ObjCMethodParams::Without(method_name_start);
        }
        Ok(Self {
            bracket_token,
            receiver,
            params,
        })
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ObjCPropertyGet {
    pub receiver: ObjCReceiver,
    pub dot_token: syn::Token![.],
    pub property_name: syn::Ident,
}

#[derive(Debug, PartialEq, Eq)]
pub struct ObjCPropertySet {
    pub receiver: ObjCReceiver,
    pub dot_token: syn::Token![.],
    pub property_name: syn::Ident,
    pub eq_token: syn::Token![=],
    pub expr: syn::Expr,
}

// Valid uses of the objc!() macro:
// - objc!([self myInstanceMethod:val1 param2:val2])
// - objc!([self innerMethod:val1] myMethod:val2 param2:val3])
// - objc!([Self myClassMethod:val1 param:val2])
// - objc!(myFunc(val1, val2))
// - objc!(self.myInstanceProperty)
// - objc!(Self.myClassProperty)
// - objc!(self.myInstanceProperty = 1)
#[derive(Debug)]
pub enum ObjCExpr {
    MethodCall(ObjCMethodCall),
    PropertyGet(ObjCPropertyGet),
    PropertySet(ObjCPropertySet),
}

impl ObjCExpr {
    pub fn receiver(&self) -> &ObjCReceiver {
        match self {
            Self::MethodCall(call) => &call.receiver,
            Self::PropertyGet(get) => &get.receiver,
            Self::PropertySet(set) => &set.receiver,
        }
    }
}

impl Parse for ObjCExpr {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        let receiver: ObjCReceiver = input.parse()?;

        if !input.peek(syn::Token![.]) {
            let err_msg = "method or property name expected";
            return match receiver {
                ObjCReceiver::SelfValue(token) => Err(syn::Error::new_spanned(token, err_msg)),
                ObjCReceiver::SelfType(token) => Err(syn::Error::new_spanned(token, err_msg)),
                ObjCReceiver::Class(token) => Err(syn::Error::new_spanned(token, err_msg)),
                ObjCReceiver::MethodCall(call) => Ok(Self::MethodCall(*call)),
            };
        }

        let dot_token: syn::Token![.] = input.parse()?;
        let property_name: syn::Ident = input.parse()?;
        if !input.peek(syn::Token![=]) {
            let get = ObjCPropertyGet {
                receiver,
                dot_token,
                property_name,
            };
            return Ok(Self::PropertyGet(get));
        }

        let eq_token: syn::Token![=] = input.parse()?;
        let expr: syn::Expr = input.parse()?;
        let set = ObjCPropertySet {
            receiver,
            dot_token,
            property_name,
            eq_token,
            expr,
        };
        Ok(Self::PropertySet(set))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_objc_expr() {
        let no_param_call: ObjCExpr = syn::parse_quote!([self myMethod]);
        assert!(matches!(
            &no_param_call,
            ObjCExpr::MethodCall(ObjCMethodCall {
                bracket_token: _,
                receiver: ObjCReceiver::SelfValue(_),
                params: ObjCMethodParams::Without(method_name),
            }) if method_name == "myMethod"
        ));

        let nested_call_multiparams: ObjCExpr =
            syn::parse_quote!([[Self alloc] myMethod:1+2 otherParam:3+4]);
        match &nested_call_multiparams {
            ObjCExpr::MethodCall(ObjCMethodCall {
                bracket_token: _,
                receiver: ObjCReceiver::MethodCall(receiver),
                params: ObjCMethodParams::With(params),
            }) => {
                assert!(matches!(receiver.as_ref(), ObjCMethodCall {
                    bracket_token: _,
                    receiver: ObjCReceiver::SelfType(_),
                    params: ObjCMethodParams::Without(method_name),
                } if method_name == "alloc"));
                match params.as_slice() {
                    [(Some(part1), _, _), (Some(part2), _, _)]
                        if part1 == "myMethod" && part2 == "otherParam" => {}
                    _ => panic!("unexpected expr {:?}", nested_call_multiparams),
                }
            }
            _ => panic!("unexpected expr {:?}", nested_call_multiparams),
        }

        let no_method_name: syn::Result<ObjCExpr> = syn::parse_str("self");
        assert!(no_method_name.is_err());
    }
}
