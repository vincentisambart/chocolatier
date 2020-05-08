use proc_macro2::TokenStream;

pub fn chocolatier(_attr: TokenStream, input: TokenStream) -> TokenStream {
    input
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
