// ARC runtime support - https://clang.llvm.org/docs/AutomaticReferenceCounting.html#runtime-support
#[link(name = "objc", kind = "dylib")]
extern "C" {
    fn objc_release(value: *mut Object);
    fn objc_retain(value: *mut Object) -> *mut Object;
}

pub trait ObjCPtr: Sized {
    unsafe fn from_raw_unchecked(ptr: NonNull<Object>) -> Self;
    fn as_raw(&self) -> NonNull<Object>;
}

#[repr(transparent)]
pub struct UntypedObjCPtr {
    raw: NonNull<Object>,
}

impl ObjCPtr for UntypedObjCPtr {
    unsafe fn from_raw_unchecked(raw: NonNull<Object>) -> Self {
        Self { raw }
    }

    fn as_raw(&self) -> NonNull<Object> {
        self.raw
    }
}

impl Drop for UntypedObjCPtr {
    fn drop(&mut self) {
        unsafe {
            objc_release(self.as_raw().as_ptr());
        }
    }
}

impl Clone for UntypedObjCPtr {
    fn clone(&self) -> Self {
        let raw_self = self.as_raw().as_ptr();
        let ptr_ret = unsafe { objc_retain(raw_self) };
        debug_assert_eq!(
            ptr_ret, raw_self,
            "objc_retain should not change the pointer, only increment the ref count"
        );
        let raw_ret = NonNull::new(ptr_ret).expect("objc_retain should not return a null pointer");
        unsafe { Self::from_raw_unchecked(raw_ret) }
    }
}
