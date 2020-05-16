use chocolatier::chocolatier;

#[chocolatier]
mod inline {
    import!(framework = "Foundation");

    #[chocolatier(interface = NSObject)]
    #[repr(transparent)]
    #[derive(chocolatier::ObjCPtr)]
    pub struct NSObject {
        ptr: choco_runtime::UntypedObjCPtr,
    }

    #[chocolatier(interface = NSObject)]
    impl NSObject {
        pub fn new() -> Self {
            objc!([Self new])
        }
    }
}

fn main() {}
