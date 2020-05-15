use chocolatier::chocolatier;

#[chocolatier]
mod inline {
    import!(framework = "Foundation");

    #[chocolatier(interface = NSObject)]
    #[repr(transparent)]
    pub struct NSObject {
        ptr: choco_runtime::UntypedObjCPtr,
    }
    impl choco_runtime::ObjCPtr for NSObject {
        fn class() -> std::ptr::NonNull<choco_runtime::ObjCClass> {
            use std::sync::atomic::{AtomicPtr, Ordering};
            static CLASS: AtomicPtr<choco_runtime::ObjCClass> =
                AtomicPtr::new(std::ptr::null_mut());
            let class = CLASS.load(Ordering::Acquire);
            if let Some(class) = std::ptr::NonNull::new(class) {
                return class;
            }
            let class = unsafe { choco_runtime::objc_getClass("NSObject\0".as_ptr()) };
            CLASS.store(class, Ordering::Release);
            std::ptr::NonNull::new(class).unwrap()
        }
        unsafe fn from_raw_unchecked(ptr: std::ptr::NonNull<choco_runtime::ObjCObject>) -> Self {
            Self {
                ptr: choco_runtime::UntypedObjCPtr::from_raw_unchecked(ptr),
            }
        }
        fn as_raw(&self) -> std::ptr::NonNull<choco_runtime::ObjCObject> {
            self.ptr.as_raw()
        }
    }

    #[chocolatier(interface = NSObject)]
    impl NSObject {
        pub fn new() -> Self {
            objc!([Self new])
        }
    }
}

fn main() {}
