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

    fn chocolat_Foundation_NSArrayInterface_class_new() -> *mut Object;
    fn chocolat_Foundation_NSArrayInterface_instance_count(self_: *mut Object) -> usize;
    fn chocolat_Foundation_NSArrayInterface_instance_firstObject(self_: *mut Object)
        -> *mut Object;
    fn chocolat_Foundation_NSArrayInterface_instance_lastObject(self_: *mut Object) -> *mut Object;
    fn chocolat_Foundation_NSArrayInterface_instance_objectAtIndex(
        self_: *mut Object,
        index: usize,
    ) -> *mut Object;
    fn chocolat_Foundation_NSArrayInterface_instance_arrayByAddingObject(
        self_: *mut Object,
        index: *mut Object,
    ) -> *mut Object;
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

trait ObjCPtr: Sized {
    unsafe fn from_raw_unchecked(ptr: NonNull<Object>) -> Self;
    fn as_raw(&self) -> NonNull<Object>;
}

trait NSObjectProtocol: ObjCPtr {
    fn hash(&self) -> usize {
        unsafe { chocolat_Foundation_NSObjectProtocol_instance_hash(self.as_raw().as_ptr()) }
    }
    fn is_equal(&self, object: Option<&impl ObjCPtr>) -> bool {
        let self_raw = self.as_raw().as_ptr();
        let object_raw = object.map_or(std::ptr::null_mut(), |obj| obj.as_raw().as_ptr());
        let ret =
            unsafe { chocolat_Foundation_NSObjectProtocol_instance_isEqual(self_raw, object_raw) };
        ret != 0
    }
    fn description(&self) -> Option<NSString> {
        let raw_self = self.as_raw().as_ptr();
        let raw_ptr =
            unsafe { chocolat_Foundation_NSObjectProtocol_instance_description(raw_self) };
        NonNull::new(raw_ptr).map(|raw| unsafe { NSString::from_raw_unchecked(raw) })
    }
    fn retain_count(&self) -> isize {
        unsafe { CFGetRetainCount(self.as_raw().as_ptr()) }
    }
    fn retain(&self) -> Self {
        let raw_self = self.as_raw().as_ptr();
        let ptr_ret = unsafe { objc_retain(raw_self) };
        debug_assert_eq!(ptr_ret, raw_self);
        let raw_ret = NonNull::new(ptr_ret).expect("objc_retain should not return a null pointer.");
        unsafe { Self::from_raw_unchecked(raw_ret) }
    }
}

trait NSObjectInterface: NSObjectProtocol {}

struct NSObject {
    raw: NonNull<Object>,
}

impl NSObjectProtocol for NSObject {}
impl NSObjectInterface for NSObject {}

impl ObjCPtr for NSObject {
    unsafe fn from_raw_unchecked(raw: NonNull<Object>) -> Self {
        Self { raw }
    }

    fn as_raw(&self) -> NonNull<Object> {
        self.raw
    }
}

impl NSObject {
    fn new() -> Self {
        let raw_ptr = unsafe { chocolat_Foundation_NSObjectInterface_class_new() };
        let raw =
            NonNull::new(raw_ptr).expect("Expecting +[NSObject new] to return a non null pointer.");
        unsafe { Self::from_raw_unchecked(raw) }
    }
}

impl Drop for NSObject {
    fn drop(&mut self) {
        unsafe {
            objc_release(self.as_raw().as_ptr());
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
        let raw_self = self.as_raw().as_ptr();
        let cstr = unsafe {
            let bytes = chocolat_Foundation_NSStringInterface_instance_UTF8String(raw_self);
            ffi::CStr::from_ptr(bytes)
        };
        Ok(cstr.to_str()?.to_string())
    }
}

struct NSString {
    raw: NonNull<Object>,
}

impl NSObjectProtocol for NSString {}
impl NSObjectInterface for NSString {}
impl NSStringInterface for NSString {}

impl ObjCPtr for NSString {
    unsafe fn from_raw_unchecked(raw: NonNull<Object>) -> Self {
        Self { raw }
    }

    fn as_raw(&self) -> NonNull<Object> {
        self.raw
    }
}

impl NSString {
    fn new_with_str(text: &str) -> Self {
        let bytes = text.as_ptr() as *const ffi::c_void;
        let len = text.len();
        let encoding = NSStringEncoding::UTF8;
        let raw_ptr = unsafe {
            chocolat_Foundation_NSStringInterface_class_newWithBytes_length_encoding(
                bytes, len, encoding,
            )
        };
        let ptr = NonNull::new(raw_ptr).expect(
            "expecting -[NSString initWithBytes:length:encoding:] to return a non null pointer.",
        );
        unsafe { Self::from_raw_unchecked(ptr) }
    }
}

impl Drop for NSString {
    fn drop(&mut self) {
        unsafe {
            objc_release(self.as_raw().as_ptr());
        }
    }
}

impl Into<NSObject> for NSString {
    fn into(self) -> NSObject {
        unsafe { NSObject::from_raw_unchecked(self.as_raw()) }
    }
}
trait FooInterface: ObjCPtr {
    fn bar(&self) -> NSObject {
        let raw_self = self.as_raw().as_ptr();
        let raw_ptr = unsafe { chocolat_Foundation_FooInterface_instance_bar(raw_self) };
        let raw = NonNull::new(raw_ptr).expect("expecting non-null");
        unsafe { NSObject::from_raw_unchecked(raw) }
    }
}

struct Foo {
    raw: NonNull<Object>,
}

impl NSObjectProtocol for Foo {}
impl NSObjectInterface for Foo {}
impl FooInterface for Foo {}

impl ObjCPtr for Foo {
    unsafe fn from_raw_unchecked(raw: NonNull<Object>) -> Self {
        Foo { raw }
    }

    fn as_raw(&self) -> NonNull<Object> {
        self.raw
    }
}

impl Foo {
    fn new() -> Self {
        let raw_ptr = unsafe { chocolat_Foundation_FooInterface_class_new() };
        let raw =
            NonNull::new(raw_ptr).expect("Expecting +[Foo new] to return a non null pointer.");
        unsafe { Self::from_raw_unchecked(raw) }
    }

    fn retain(&self) -> Self {
        let raw_self = self.as_raw().as_ptr();
        let raw_ptr = unsafe { objc_retain(raw_self) };
        debug_assert_eq!(raw_ptr, raw_self);
        let raw = NonNull::new(raw_ptr).expect("objc_retain should not return a null pointer");
        unsafe { Self::from_raw_unchecked(raw) }
    }
}

impl Drop for Foo {
    fn drop(&mut self) {
        unsafe {
            objc_release(self.as_raw().as_ptr());
        }
    }
}

trait NSArrayInterface<T: ObjCPtr>: NSObjectInterface {
    fn first(&self) -> Option<T> {
        let raw_self = self.as_raw().as_ptr();
        let raw_ptr =
            unsafe { chocolat_Foundation_NSArrayInterface_instance_firstObject(raw_self) };
        NonNull::new(raw_ptr).map(|raw| unsafe { T::from_raw_unchecked(raw) })
    }
    fn last(&self) -> Option<T> {
        let raw_self = self.as_raw().as_ptr();
        let raw_ptr = unsafe { chocolat_Foundation_NSArrayInterface_instance_lastObject(raw_self) };
        NonNull::new(raw_ptr).map(|raw| unsafe { T::from_raw_unchecked(raw) })
    }
    fn object_at_index(&self, index: usize) -> T {
        let raw_self = self.as_raw().as_ptr();
        let raw_ptr =
            unsafe { chocolat_Foundation_NSArrayInterface_instance_objectAtIndex(raw_self, index) };
        let raw = NonNull::new(raw_ptr).expect("expecting non-null");
        unsafe { T::from_raw_unchecked(raw) }
    }
    fn count(&self) -> usize {
        let raw_self = self.as_raw().as_ptr();
        unsafe { chocolat_Foundation_NSArrayInterface_instance_count(raw_self) }
    }
    fn adding<X: Into<T>>(&self, object: X) -> NSArray<T> {
        let raw_self = self.as_raw().as_ptr();
        let raw_object = object.into().as_raw().as_ptr();
        let raw_ptr = unsafe {
            chocolat_Foundation_NSArrayInterface_instance_arrayByAddingObject(raw_self, raw_object)
        };
        let raw = NonNull::new(raw_ptr).expect("expecting non-null");
        unsafe { NSArray::from_raw_unchecked(raw) }
    }
}

struct NSArray<T: ObjCPtr> {
    raw: NonNull<Object>,
    _marker: std::marker::PhantomData<T>,
}

impl<T: ObjCPtr> NSObjectProtocol for NSArray<T> {}
impl<T: ObjCPtr> NSObjectInterface for NSArray<T> {}
impl<T: ObjCPtr> NSArrayInterface<T> for NSArray<T> {}

impl<T: ObjCPtr> ObjCPtr for NSArray<T> {
    unsafe fn from_raw_unchecked(raw: NonNull<Object>) -> Self {
        Self {
            raw,
            _marker: std::marker::PhantomData,
        }
    }

    fn as_raw(&self) -> NonNull<Object> {
        self.raw
    }
}

impl<T: ObjCPtr> NSArray<T> {
    fn new() -> Self {
        let raw_ptr = unsafe { chocolat_Foundation_NSArrayInterface_class_new() };
        let raw =
            NonNull::new(raw_ptr).expect("Expecting +[NSArray new] to return a non null pointer.");
        unsafe { Self::from_raw_unchecked(raw) }
    }

    fn retain(&self) -> Self {
        let raw_self = self.as_raw().as_ptr();
        let raw_ptr = unsafe { objc_retain(raw_self) };
        debug_assert_eq!(raw_ptr, raw_self);
        let raw = NonNull::new(raw_ptr).expect("objc_retain should not return a null pointer");
        unsafe { Self::from_raw_unchecked(raw) }
    }
}

impl<T: ObjCPtr> Drop for NSArray<T> {
    fn drop(&mut self) {
        unsafe {
            objc_release(self.as_raw().as_ptr());
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
        println!("equal0: {}", obj1.is_equal(Some(&obj1)));
        println!("equal1: {}", obj1.is_equal(Some(&obj2)));
        println!("equal2: {}", obj2.is_equal(Some(&obj2)));
        println!("equal3: {}", obj2.is_equal(None::<&NSObject>));
        println!("equal4: {}", obj2.is_equal(obj3.as_ref()));
        println!("equal4: {}", obj2.is_equal(obj3.as_ref()));
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

    let array: NSArray<NSObject> = NSArray::new();
    println!("array count - {}", array.count());
    let array = array.adding(NSObject::new());
    println!("array count - {}", array.count());
    let array = array.adding(NSString::new_with_str("bonjour"));
    println!("array count - {}", array.count());

    // let _first = array.object_at_index(0);

    // unsafe { objc_autoreleasePoolPop(pool) };
}
