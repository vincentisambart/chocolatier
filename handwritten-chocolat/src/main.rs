#![allow(dead_code)]

// TODO:
// - enum
// - protocols
// - categories
// - string, arrays
// - exceptions
// - check things get freed

use std::ffi;
use std::ptr::NonNull;

#[repr(C)]
pub struct Object {
    _private: [u8; 0],
}

extern "C" {
    fn chocolat_Foundation_NSObjectProtocol_instance_hash(self_: *mut Object) -> usize;
    fn chocolat_Foundation_NSObjectProtocol_instance_isEqual(
        self_: *mut Object,
        other: *mut Object,
    ) -> i8;
    fn chocolat_Foundation_NSObjectProtocol_instance_description(self_: *mut Object)
        -> *mut Object;

    fn chocolat_Foundation_NSObjectInterface_class_new() -> *mut Object;

    fn chocolat_Foundation_NSStringInterface_class_newWithBytes_length_encoding(
        bytes: *const ffi::c_void,
        len: usize,
        encoding: NSStringEncoding,
    ) -> *mut Object;
    fn chocolat_Foundation_NSStringInterface_instance_lengthOfBytesUsingEncoding(
        self_: *mut Object,
        encoding: NSStringEncoding,
    ) -> usize;
    fn chocolat_Foundation_NSStringInterface_instance_UTF8String(self_: *mut Object) -> *const i8;

    fn chocolat_Foundation_FooInterface_class_new() -> *mut Object;
    fn chocolat_Foundation_FooInterface_instance_bar(self_: *mut Object) -> *mut Object;
}

extern "C" {
    fn CFGetRetainCount(self_: *mut Object) -> isize;
}

// ARC runtime support - https://clang.llvm.org/docs/AutomaticReferenceCounting.html#runtime-support
#[link(name = "objc", kind = "dylib")]
extern "C" {
    fn objc_autoreleasePoolPush() -> *const ffi::c_void;
    fn objc_autoreleasePoolPop(pool: *const ffi::c_void);
    fn objc_release(value: *mut Object);
    fn objc_retain(value: *mut Object) -> *mut Object;
}

trait ObjCPtr {
    fn ptr(&self) -> NonNull<Object>;
}

trait NullableObjCPtr {
    fn nullable_ptr(&self) -> *mut Object;
}

impl<T> NullableObjCPtr for T
where
    T: ObjCPtr,
{
    fn nullable_ptr(&self) -> *mut Object {
        self.ptr().as_ptr()
    }
}

impl<T> NullableObjCPtr for Option<T>
where
    T: ObjCPtr,
{
    fn nullable_ptr(&self) -> *mut Object {
        self.as_ref()
            .map_or(std::ptr::null_mut(), |obj| obj.ptr().as_ptr())
    }
}

trait NSObjectProtocol: ObjCPtr {
    fn hash(&self) -> usize {
        unsafe { chocolat_Foundation_NSObjectProtocol_instance_hash(self.ptr().as_ptr()) }
    }
    fn is_equal(&self, object: &impl NullableObjCPtr) -> bool {
        let ret = unsafe {
            chocolat_Foundation_NSObjectProtocol_instance_isEqual(
                self.ptr().as_ptr(),
                object.nullable_ptr(),
            )
        };
        ret != 0
    }
    fn description(&self) -> Option<NSString> {
        let raw_self = self.ptr().as_ptr();
        let raw_ptr =
            unsafe { chocolat_Foundation_NSObjectProtocol_instance_description(raw_self) };
        NonNull::new(raw_ptr).map(|ptr| NSString { ptr })
    }
    fn retain_count(&self) -> isize {
        unsafe { CFGetRetainCount(self.ptr().as_ptr()) }
    }
}

trait NSObjectInterface: NSObjectProtocol {}

struct NSObject {
    ptr: NonNull<Object>,
}

impl NSObjectProtocol for NSObject {}
impl NSObjectInterface for NSObject {}

impl ObjCPtr for NSObject {
    fn ptr(&self) -> NonNull<Object> {
        self.ptr
    }
}

impl NSObject {
    fn new() -> Self {
        let raw_ptr = unsafe { chocolat_Foundation_NSObjectInterface_class_new() };
        let ptr =
            NonNull::new(raw_ptr).expect("Expecting +[NSObject new] to return a non null pointer.");
        NSObject { ptr }
    }

    fn retain(&self) -> Self {
        let raw_self = self.ptr().as_ptr();
        let raw_ptr = unsafe { objc_retain(raw_self) };
        debug_assert_eq!(raw_ptr, raw_self);
        let ptr = NonNull::new(raw_ptr).expect("objc_retain should not return a null pointer.");
        NSObject { ptr }
    }
}

impl Drop for NSObject {
    fn drop(&mut self) {
        unsafe {
            objc_release(self.ptr().as_ptr());
        }
    }
}

#[repr(transparent)]
struct NSStringEncoding(usize);

impl NSStringEncoding {
    const ASCII: Self = NSStringEncoding(1);
    const NEXTSTEP: Self = NSStringEncoding(2);
    const JAPANESE_EUC: Self = NSStringEncoding(3);
    const UTF8: Self = NSStringEncoding(4);
    const ISO_LATIN1: Self = NSStringEncoding(5);
}

trait NSStringInterface: NSObjectInterface {
    fn to_string(&self) -> Result<String, std::str::Utf8Error> {
        let raw_self = self.ptr().as_ptr();
        let cstr = unsafe {
            let bytes = chocolat_Foundation_NSStringInterface_instance_UTF8String(raw_self);
            ffi::CStr::from_ptr(bytes)
        };
        Ok(cstr.to_str()?.to_string())
    }
}

struct NSString {
    ptr: NonNull<Object>,
}

impl NSObjectProtocol for NSString {}
impl NSObjectInterface for NSString {}
impl NSStringInterface for NSString {}

impl ObjCPtr for NSString {
    fn ptr(&self) -> NonNull<Object> {
        self.ptr
    }
}

impl NSString {
    fn new_with_str(text: &str) -> Self {
        let raw_ptr = unsafe {
            chocolat_Foundation_NSStringInterface_class_newWithBytes_length_encoding(
                text.as_ptr() as *const ffi::c_void,
                text.len(),
                NSStringEncoding(4),
            )
        };
        let ptr = NonNull::new(raw_ptr).expect(
            "expecting -[NSString initWithBytes:length:encoding:] to return a non null pointer.",
        );
        NSString { ptr }
    }
}

impl Drop for NSString {
    fn drop(&mut self) {
        unsafe {
            objc_release(self.ptr().as_ptr());
        }
    }
}

trait FooInterface: ObjCPtr {
    fn bar(&self) -> NSObject {
        let raw_self = self.ptr().as_ptr();
        let raw_ptr = unsafe { chocolat_Foundation_FooInterface_instance_bar(raw_self) };
        let ptr = NonNull::new(raw_ptr).expect("expecting non-null");
        NSObject { ptr }
    }
}

struct Foo {
    ptr: NonNull<Object>,
}

impl NSObjectProtocol for Foo {}
impl NSObjectInterface for Foo {}
impl FooInterface for Foo {}

impl ObjCPtr for Foo {
    fn ptr(&self) -> NonNull<Object> {
        self.ptr
    }
}

impl Foo {
    fn new() -> Self {
        let raw_ptr = unsafe { chocolat_Foundation_FooInterface_class_new() };
        let ptr =
            NonNull::new(raw_ptr).expect("Expecting +[Foo new] to return a non null pointer.");
        Foo { ptr }
    }

    fn retain(&self) -> Self {
        let raw_self = self.ptr().as_ptr();
        let raw_ptr = unsafe { objc_retain(raw_self) };
        debug_assert_eq!(raw_ptr, raw_self);
        let ptr = NonNull::new(raw_ptr).expect("objc_retain should not return a null pointer.");
        Foo { ptr }
    }
}

impl Drop for Foo {
    fn drop(&mut self) {
        unsafe {
            objc_release(self.ptr().as_ptr());
        }
    }
}

fn main() {
    // let pool = unsafe { objc_autoreleasePoolPush() };
    let bar;

    {
        let obj1 = NSObject::new();
        let obj2 = NSObject::new();
        let obj3 = Some(NSObject::new());
        println!("hash1: {}", obj1.hash());
        println!("hash1: {}", obj1.hash());
        println!("hash2: {}", obj2.hash());
        println!("equal0: {}", obj1.is_equal(&obj1));
        println!("equal1: {}", obj1.is_equal(&obj2));
        println!("equal2: {}", obj2.is_equal(&obj2));
        println!("equal3: {}", obj2.is_equal(&None::<NSObject>));
        println!("equal4: {}", obj2.is_equal(&obj3));
        println!("desc: {}", obj1.description().unwrap().to_string().unwrap());
        let x = NSString::new_with_str("こんにちは！");
        let desc = x.description();
        println!("desc string: {}", desc.unwrap().to_string().unwrap());

        println!("----1");
        let foo = Foo::new();
        println!("----2 - {}", foo.retain_count());
        {
            let _ = foo.retain();
            println!("----3 - {}", foo.retain_count());
            bar = foo.bar();
            println!("----4 - {} - {}", foo.retain_count(), bar.retain_count());
        }
        println!("----5 - {} - {}", foo.retain_count(), bar.retain_count());
    }
    println!("----6 - {}", bar.retain_count());
    // unsafe { objc_autoreleasePoolPop(pool) };
}
