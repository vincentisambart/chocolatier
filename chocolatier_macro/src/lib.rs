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

#[proc_macro_derive(ObjCPtr)]
pub fn objc_ptr_derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    match chocolatier_internal::objc_ptr_derive(input.into()) {
        Ok(output) => output,
        Err(err) => err.to_compile_error(),
    }
    .into()
}
