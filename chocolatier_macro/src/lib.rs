#![warn(rust_2018_idioms)]

#[proc_macro_attribute]
pub fn chocolatier(attr: proc_macro::TokenStream, input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    chocolatier_internal::chocolatier(attr.into(), input.into()).into()
}
