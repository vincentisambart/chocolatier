#![warn(rust_2018_idioms)]

pub use chocolatier_macro::chocolatier;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
