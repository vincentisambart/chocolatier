#![allow(dead_code)]
#![warn(rust_2018_idioms)]

mod block;

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
pub struct ObjCObject {
    _private: [u8; 0],
}
#[repr(C)]
pub struct ObjCClass {
    _private: [u8; 0],
}

extern "C" {
    fn chocolat_Foundation_NSObjectProtocol_instance_hash(self_: *mut ObjCObject) -> usize;
    fn chocolat_Foundation_NSObjectProtocol_instance_isEqual(
        self_: *mut ObjCObject,
        other: *mut ObjCObject,
    ) -> i8;
    fn chocolat_Foundation_NSObjectProtocol_instance_description(
        self_: *mut ObjCObject,
    ) -> *mut ObjCObject;

    fn chocolat_Foundation_NSObjectInterface_class_new(
        class: NonNull<ObjCClass>,
    ) -> *mut ObjCObject;

    fn chocolat_Foundation_NSStringInterface_class_newWithBytes_length_encoding(
        class: NonNull<ObjCClass>,
        bytes: *const ffi::c_void,
        len: usize,
        encoding: NSStringEncoding,
    ) -> *mut ObjCObject;
    fn chocolat_Foundation_NSStringInterface_instance_lengthOfBytesUsingEncoding(
        self_: *mut ObjCObject,
        encoding: NSStringEncoding,
    ) -> usize;
    fn chocolat_Foundation_NSStringInterface_instance_UTF8String(
        self_: *mut ObjCObject,
    ) -> *const i8;
    fn chocolat_Foundation_NSStringInterface_instance_charAtIndex(
        self_: *mut ObjCObject,
        index: usize,
    ) -> u16;
    fn chocolat_Foundation_NSStringInterface_instance_length(self_: *mut ObjCObject) -> usize;

    fn chocolat_Foundation_FooInterface_instance_bar(self_: *mut ObjCObject) -> *mut ObjCObject;

    fn chocolat_Foundation_NSArrayInterface_instance_count(self_: *mut ObjCObject) -> usize;
    fn chocolat_Foundation_NSArrayInterface_instance_firstObject(
        self_: *mut ObjCObject,
    ) -> *mut ObjCObject;
    fn chocolat_Foundation_NSArrayInterface_instance_lastObject(
        self_: *mut ObjCObject,
    ) -> *mut ObjCObject;
    fn chocolat_Foundation_NSArrayInterface_instance_objectAtIndex(
        self_: *mut ObjCObject,
        index: usize,
    ) -> *mut ObjCObject;
    fn chocolat_Foundation_NSArrayInterface_instance_arrayByAddingObject(
        self_: *mut ObjCObject,
        obj: *mut ObjCObject,
    ) -> *mut ObjCObject;
    fn chocolat_Foundation_NSArrayInterface_instance_enumerateObjectsUsingBlock(
        self_: *mut ObjCObject,
        block: *mut std::ffi::c_void,
    );

    fn chocolat_Foundation_NSMutableArrayInterface_instance_addObject(
        self_: *mut ObjCObject,
        obj: *mut ObjCObject,
    );
    fn chocolat_Foundation_NSMutableArrayInterface_instance_insertObject_atIndex(
        self_: *mut ObjCObject,
        obj: *mut ObjCObject,
        index: usize,
    );
}

extern "C" {
    fn CFGetRetainCount(self_: *mut ObjCObject) -> isize;
}

// ARC runtime support - https://clang.llvm.org/docs/AutomaticReferenceCounting.html#runtime-support
#[link(name = "objc", kind = "dylib")]
extern "C" {
    fn objc_autoreleasePoolPush() -> *const ffi::c_void;
    fn objc_autoreleasePoolPop(pool: *const ffi::c_void);
    fn objc_release(value: *mut ObjCObject);
    fn objc_retain(value: *mut ObjCObject) -> *mut ObjCObject;
    fn objc_getClass(name: *const u8) -> *mut ObjCClass;
}

trait ObjCPtr: Sized {
    fn class() -> NonNull<ObjCClass>;
    unsafe fn from_raw_unchecked(ptr: NonNull<ObjCObject>) -> Self;
    fn as_raw(&self) -> NonNull<ObjCObject>;
}

struct UntypedObjCPtr {
    raw: NonNull<ObjCObject>,
}

impl UntypedObjCPtr {
    unsafe fn from_raw_unchecked(raw: NonNull<ObjCObject>) -> Self {
        Self { raw }
    }

    fn as_raw(&self) -> NonNull<ObjCObject> {
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

trait NSObjectProtocol: ObjCPtr {
    fn hash(&self) -> usize {
        unsafe { chocolat_Foundation_NSObjectProtocol_instance_hash(self.as_raw().as_ptr()) }
    }
    fn is_equal(&self, obj: Option<&impl ObjCPtr>) -> bool {
        let self_raw = self.as_raw().as_ptr();
        let obj_raw = obj.map_or(std::ptr::null_mut(), |obj| obj.as_raw().as_ptr());
        let ret =
            unsafe { chocolat_Foundation_NSObjectProtocol_instance_isEqual(self_raw, obj_raw) };
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
    fn new() -> Self {
        let raw_ptr = unsafe { chocolat_Foundation_NSObjectInterface_class_new(Self::class()) };
        let raw = NonNull::new(raw_ptr)
            .expect("Expecting +[<class_name> new] to return a non null pointer.");
        unsafe { Self::from_raw_unchecked(raw) }
    }
}

trait NSObjectInterface: NSObjectProtocol {}

struct NSObject {
    ptr: UntypedObjCPtr,
}

impl NSObjectProtocol for NSObject {}
impl NSObjectInterface for NSObject {}

impl ObjCPtr for NSObject {
    unsafe fn from_raw_unchecked(raw: NonNull<ObjCObject>) -> Self {
        let ptr = UntypedObjCPtr::from_raw_unchecked(raw);
        Self { ptr }
    }

    fn as_raw(&self) -> NonNull<ObjCObject> {
        self.ptr.as_raw()
    }

    fn class() -> NonNull<ObjCClass> {
        use std::sync::atomic::{AtomicPtr, Ordering};
        static CLASS: AtomicPtr<ObjCClass> = AtomicPtr::new(std::ptr::null_mut());
        let class = CLASS.load(Ordering::Acquire);
        if let Some(class) = NonNull::new(class) {
            return class;
        }
        let class = unsafe { objc_getClass("NSObject\0".as_ptr()) };
        CLASS.store(class, Ordering::Release);
        NonNull::new(class).unwrap()
    }
}

#[repr(transparent)]
struct NSStringEncoding(usize);

impl NSStringEncoding {
    const ASCII: Self = Self(1);
    const UTF8: Self = Self(4);
    const UTF16: Self = Self(10);
    const UTF16_BE: Self = Self(0x90000100);
    const UTF16_LE: Self = Self(0x94000100);
    const UTF32: Self = Self(0x8c000100);
    const UTF32_BE: Self = Self(0x98000100);
    const UTF32_LE: Self = Self(0x9c000100);
    const NEXTSTEP: Self = Self(2);
    const JAPANESE_EUC: Self = Self(3);
    const ISO_LATIN1: Self = Self(5);
    const SYMBOL: Self = Self(6);
    const NON_LOSSY_ASCII: Self = Self(7);
    const SHIFT_JIS: Self = Self(8);
    const ISO_LATIN2: Self = Self(9);
    const WINDOWS_CP1251: Self = Self(11);
    const WINDOWS_CP1252: Self = Self(12);
    const WINDOWS_CP1253: Self = Self(13);
    const WINDOWS_CP1254: Self = Self(14);
    const WINDOWS_CP1250: Self = Self(15);
    const ISO2022JP: Self = Self(21);
    const MACOS_ROMAN: Self = Self(30);
}

trait NSCopyingProtocol {}
trait NSStringInterface: NSObjectInterface
where
    Self: NSCopyingProtocol,
{
    fn to_string(&self) -> Result<String, std::str::Utf8Error> {
        let raw_self = self.as_raw().as_ptr();
        let cstr = unsafe {
            let bytes = chocolat_Foundation_NSStringInterface_instance_UTF8String(raw_self);
            ffi::CStr::from_ptr(bytes)
        };
        Ok(cstr.to_str()?.to_string())
    }

    fn char_at(&self, index: usize) -> u16 {
        let raw_self = self.as_raw().as_ptr();
        unsafe { chocolat_Foundation_NSStringInterface_instance_charAtIndex(raw_self, index) }
    }

    fn len(&self) -> usize {
        let raw_self = self.as_raw().as_ptr();
        unsafe { chocolat_Foundation_NSStringInterface_instance_length(raw_self) }
    }
}

struct NSString {
    ptr: UntypedObjCPtr,
}

impl NSObjectProtocol for NSString {}
impl NSObjectInterface for NSString {}
impl NSStringInterface for NSString {}
impl NSCopyingProtocol for NSString {}

impl ObjCPtr for NSString {
    unsafe fn from_raw_unchecked(raw: NonNull<ObjCObject>) -> Self {
        let ptr = UntypedObjCPtr::from_raw_unchecked(raw);
        Self { ptr }
    }

    fn as_raw(&self) -> NonNull<ObjCObject> {
        self.ptr.as_raw()
    }

    fn class() -> NonNull<ObjCClass> {
        use std::sync::atomic::{AtomicPtr, Ordering};
        static CLASS: AtomicPtr<ObjCClass> = AtomicPtr::new(std::ptr::null_mut());
        let class = CLASS.load(Ordering::Acquire);
        if let Some(class) = NonNull::new(class) {
            return class;
        }
        let class = unsafe { objc_getClass("NSString\0".as_ptr()) };
        CLASS.store(class, Ordering::Release);
        NonNull::new(class).unwrap()
    }
}

impl From<NSString> for NSObject {
    fn from(obj: NSString) -> Self {
        unsafe { NSObject::from_raw_unchecked(obj.as_raw()) }
    }
}

impl NSString {
    fn new_with_str(text: &str) -> Self {
        let bytes = text.as_ptr() as *const ffi::c_void;
        let len = text.len();
        let encoding = NSStringEncoding::UTF8;
        let raw_ptr = unsafe {
            chocolat_Foundation_NSStringInterface_class_newWithBytes_length_encoding(
                Self::class(),
                bytes,
                len,
                encoding,
            )
        };
        let ptr = NonNull::new(raw_ptr).expect(
            "expecting -[NSString initWithBytes:length:encoding:] to return a non null pointer.",
        );
        unsafe { Self::from_raw_unchecked(ptr) }
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
    ptr: UntypedObjCPtr,
}

impl NSObjectProtocol for Foo {}
impl NSObjectInterface for Foo {}
impl FooInterface for Foo {}

impl ObjCPtr for Foo {
    unsafe fn from_raw_unchecked(raw: NonNull<ObjCObject>) -> Self {
        let ptr = UntypedObjCPtr::from_raw_unchecked(raw);
        Self { ptr }
    }

    fn as_raw(&self) -> NonNull<ObjCObject> {
        self.ptr.as_raw()
    }

    fn class() -> NonNull<ObjCClass> {
        use std::sync::atomic::{AtomicPtr, Ordering};
        static CLASS: AtomicPtr<ObjCClass> = AtomicPtr::new(std::ptr::null_mut());
        let class = CLASS.load(Ordering::Acquire);
        if let Some(class) = NonNull::new(class) {
            return class;
        }
        let class = unsafe { objc_getClass("Foo\0".as_ptr()) };
        CLASS.store(class, Ordering::Release);
        NonNull::new(class).unwrap()
    }
}

impl From<Foo> for NSObject {
    fn from(obj: Foo) -> Self {
        unsafe { NSObject::from_raw_unchecked(obj.as_raw()) }
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
    fn object_at(&self, index: usize) -> T {
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
    fn adding_object<U: Into<T> + ObjCPtr>(&self, object: &U) -> NSArray<T> {
        let raw_self = self.as_raw().as_ptr();
        let raw_obj = object.as_raw().as_ptr();
        let raw_ptr = unsafe {
            chocolat_Foundation_NSArrayInterface_instance_arrayByAddingObject(raw_self, raw_obj)
        };
        let raw = NonNull::new(raw_ptr).expect("expecting non-null");
        unsafe { NSArray::from_raw_unchecked(raw) }
    }
    fn enumerate_objects_using_block<
        F: Fn(NonNull<ObjCObject>, usize, &mut u8) + Send + Sync + Clone + 'static,
    >(
        &self,
        f: F,
    ) {
        let raw_self = self.as_raw().as_ptr();
        let block = block::StackBlock::new(f);
        let block_ref = &block;
        unsafe {
            chocolat_Foundation_NSArrayInterface_instance_enumerateObjectsUsingBlock(
                raw_self,
                block_ref as *const _ as _,
            );
        }
    }
}

struct NSArray<T: ObjCPtr> {
    ptr: UntypedObjCPtr,
    _marker: std::marker::PhantomData<T>,
}

impl<T: ObjCPtr> NSObjectProtocol for NSArray<T> {}
impl<T: ObjCPtr> NSObjectInterface for NSArray<T> {}
impl<T: ObjCPtr> NSArrayInterface<T> for NSArray<T> {}

impl<T: ObjCPtr> ObjCPtr for NSArray<T> {
    unsafe fn from_raw_unchecked(raw: NonNull<ObjCObject>) -> Self {
        let ptr = UntypedObjCPtr::from_raw_unchecked(raw);
        Self {
            ptr,
            _marker: std::marker::PhantomData,
        }
    }

    fn as_raw(&self) -> NonNull<ObjCObject> {
        self.ptr.as_raw()
    }

    fn class() -> NonNull<ObjCClass> {
        use std::sync::atomic::{AtomicPtr, Ordering};
        static CLASS: AtomicPtr<ObjCClass> = AtomicPtr::new(std::ptr::null_mut());
        let class = CLASS.load(Ordering::Acquire);
        if let Some(class) = NonNull::new(class) {
            return class;
        }
        let class = unsafe { objc_getClass("NSArray\0".as_ptr()) };
        CLASS.store(class, Ordering::Release);
        NonNull::new(class).unwrap()
    }
}

impl<T: ObjCPtr> From<NSArray<T>> for NSObject {
    fn from(obj: NSArray<T>) -> Self {
        unsafe { NSObject::from_raw_unchecked(obj.as_raw()) }
    }
}

trait NSMutableArrayInterface<T: ObjCPtr>: NSArrayInterface<T> {
    fn add_object<U: Into<T> + ObjCPtr>(&self, object: &U) {
        let raw_self = self.as_raw().as_ptr();
        let raw_obj = object.as_raw().as_ptr();
        unsafe { chocolat_Foundation_NSMutableArrayInterface_instance_addObject(raw_self, raw_obj) }
    }

    fn insert_object_at<U: Into<T> + ObjCPtr>(&self, object: &U, index: usize) {
        let raw_self = self.as_raw().as_ptr();
        let raw_obj = object.as_raw().as_ptr();
        unsafe {
            chocolat_Foundation_NSMutableArrayInterface_instance_insertObject_atIndex(
                raw_self, raw_obj, index,
            )
        }
    }
}

struct NSMutableArray<T: ObjCPtr> {
    ptr: UntypedObjCPtr,
    _marker: std::marker::PhantomData<T>,
}

impl<T: ObjCPtr> NSObjectProtocol for NSMutableArray<T> {}
impl<T: ObjCPtr> NSObjectInterface for NSMutableArray<T> {}
impl<T: ObjCPtr> NSArrayInterface<T> for NSMutableArray<T> {}
impl<T: ObjCPtr> NSMutableArrayInterface<T> for NSMutableArray<T> {}

impl<T: ObjCPtr> ObjCPtr for NSMutableArray<T> {
    unsafe fn from_raw_unchecked(raw: NonNull<ObjCObject>) -> Self {
        let ptr = UntypedObjCPtr::from_raw_unchecked(raw);
        Self {
            ptr,
            _marker: std::marker::PhantomData,
        }
    }

    fn as_raw(&self) -> NonNull<ObjCObject> {
        self.ptr.as_raw()
    }

    fn class() -> NonNull<ObjCClass> {
        use std::sync::atomic::{AtomicPtr, Ordering};
        static CLASS: AtomicPtr<ObjCClass> = AtomicPtr::new(std::ptr::null_mut());
        let class = CLASS.load(Ordering::Acquire);
        if let Some(class) = NonNull::new(class) {
            return class;
        }
        let class = unsafe { objc_getClass("NSMutableArray\0".as_ptr()) };
        CLASS.store(class, Ordering::Release);
        NonNull::new(class).unwrap()
    }
}

impl<T: ObjCPtr> From<NSMutableArray<T>> for NSObject {
    fn from(obj: NSMutableArray<T>) -> Self {
        unsafe { NSObject::from_raw_unchecked(obj.as_raw()) }
    }
}

impl<T: ObjCPtr> From<NSMutableArray<T>> for NSArray<T> {
    fn from(obj: NSMutableArray<T>) -> Self {
        unsafe { NSArray::from_raw_unchecked(obj.as_raw()) }
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
    let obj = NSObject::new();
    println!("obj ref count 1 - {}", obj.retain_count());
    let array = array.adding_object(&obj);
    println!("obj ref count 2 - {}", obj.retain_count());
    println!("array count - {}", array.count());
    let array = array.adding_object(&NSString::new_with_str("bonjour"));
    println!("array count - {}", array.count());

    println!("obj ref count 2.5 - {}", obj.retain_count());
    let first1 = array.object_at(0);
    println!("obj ref count 3 - {}", obj.retain_count());
    let first2 = array.first();
    println!("obj ref count 4 - {}", obj.retain_count());
    println!("equal: {}", first1.is_equal(first2.as_ref()));

    array.enumerate_objects_using_block(|_, idx, stop| {
        println!("idx: {}", idx);
        *stop = 1;
    });

    // unsafe { objc_autoreleasePoolPop(pool) };
}
