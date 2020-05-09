use chocolatier::chocolatier;

#[chocolatier]
mod inline {
    import!(framework = "Foundation");

    #[chocolatier(enum = NSStringEncoding)]
    #[repr(transparent)]
    pub struct NSStringEncoding(pub usize);

    #[chocolatier(enum = NSStringEncoding)]
    impl NSStringEncoding {
        pub const ASCII: Self = NSASCIIStringEncoding;
        pub const UTF8: Self = NSUTF8StringEncoding;
        // Testing a strange use of some use of enums akin to categories in Cocoa.
        pub const PROPRIETARY: Self = NSProprietaryStringEncoding;
    }
}

use inline::*;

fn main() {
    assert_eq!(NSStringEncoding::ASCII.0, 1);
    assert_eq!(NSStringEncoding::UTF8.0, 4);
    assert_eq!(NSStringEncoding::PROPRIETARY.0, 65536);
}
