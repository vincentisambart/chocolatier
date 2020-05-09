#![warn(rust_2018_idioms)]

#[proc_macro_attribute]
pub fn chocolatier(
    _attr: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    match chocolatier_internal::chocolatier(input.into()) {
        Ok(output) => output,
        Err(err) => err.to_compile_error(),
    }
    .into()
}
