#![warn(rust_2018_idioms)]

use syn::parse_macro_input;

#[proc_macro_attribute]
pub fn chocolatier(
    _attr: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let item = parse_macro_input!(input as syn::ItemMod);
    match chocolatier_internal::chocolatier(item) {
        Ok(output) => output,
        Err(err) => err.to_compile_error(),
    }
    .into()
}
