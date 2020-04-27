// TODO: Merge Block and BlockUninit – dropping manually

// 3 block types:
// - "normal" -> Fn + Send + Sync + 'static
// - unescape -> Fn + Send + Sync
// - unescape single thread -> Fn
// Note that none can use FnMut as the block can be copied on the C-side.

use std::marker::PhantomData;

#[repr(C)]
struct Class {
    _private: [u8; 0],
}

#[link(name = "System", kind = "dylib")]
extern "C" {
    static _NSConcreteStackBlock: Class;
    static _NSConcreteGlobalBlock: Class;
}

#[repr(transparent)]
pub struct BlockFlags(pub std::os::raw::c_int);

impl BlockFlags {
    pub const IS_NOESCAPE: Self = Self(1 << 23);
    pub const HAS_COPY_DISPOSE: Self = Self(1 << 25);
    pub const HAS_CTOR: Self = Self(1 << 26); // helpers have C++ code
    pub const IS_GLOBAL: Self = Self(1 << 28);
    pub const HAS_STRET: Self = Self(1 << 29); // IFF BLOCK_HAS_SIGNATURE
    pub const HAS_SIGNATURE: Self = Self(1 << 30);
}

impl std::ops::BitOr for BlockFlags {
    type Output = Self;
    fn bitor(self, rhs: Self) -> Self::Output {
        Self(self.0 | rhs.0)
    }
}

#[repr(C)]
struct BlockDescriptor {
    _reserved: std::os::raw::c_ulong,
    block_size: std::os::raw::c_ulong,
    copy_helper: unsafe extern "C" fn(*mut std::ffi::c_void, *const std::ffi::c_void),
    dispose_helper: unsafe extern "C" fn(*mut std::ffi::c_void),
    signature: *const u8,
}
// By default `*const` are not shareable between threads and so Rust won't less us make a `static` from it.
unsafe impl std::marker::Sync for BlockDescriptor {}

#[repr(C)]
struct BlockHeader {
    isa: *const Class,
    flags: BlockFlags,
    reserved: std::os::raw::c_int,
    // invoke is a function pointer taking a pointer to the block as a first parameter,
    // but the rest of its type depends on the block parameters and return type.
    invoke: *const std::ffi::c_void,
}

// Must have the exact same layout as Block.
#[repr(C)]
struct BlockUninit<F, Ret, Args>
where
    F: Clone + BlockInvoke<Ret, Args> + 'static,
{
    // We won't change the content of header once created, but use an unsafe cell because the blocks runtime changes for example flags, even possibly from other threads.
    header: std::cell::UnsafeCell<BlockHeader>,
    descriptor: &'static BlockDescriptor,
    f: std::mem::MaybeUninit<F>,
    _ret_marker: PhantomData<*const Ret>,
    _args_marker: PhantomData<*const Args>,
}

// Must have the exact same memory layout as BlockUninit.
#[repr(C)]
pub struct Block<F, Ret, Args>
where
    F: Clone + BlockInvoke<Ret, Args> + 'static,
{
    // We won't change the content of header once created, but use an unsafe cell because the blocks runtime changes for example flags, even possibly from other threads.
    header: std::cell::UnsafeCell<BlockHeader>,
    descriptor: &'static BlockDescriptor,
    f: F,
    _ret_marker: PhantomData<*const Ret>,
    _args_marker: PhantomData<*const Args>,
}

impl<F, Ret, Args> Block<F, Ret, Args>
where
    F: Clone + BlockInvoke<Ret, Args> + 'static,
{
    unsafe extern "C" fn copy(dst: *mut std::ffi::c_void, src: *const std::ffi::c_void) {
        let src = &*(src as *const Block<F, Ret, Args>);
        let dst = &mut *(dst as *mut BlockUninit<F, Ret, Args>);

        println!("----copied");
        dst.f.as_mut_ptr().write(src.f.clone());
    }

    unsafe extern "C" fn dispose(src: *mut std::ffi::c_void) {
        let src = &mut *(src as *mut Block<F, Ret, Args>);
        drop(src)
    }

    const DESCRIPTOR: BlockDescriptor = BlockDescriptor {
        _reserved: 0,
        block_size: std::mem::size_of::<Block<F, Ret, Args>>() as u64,
        copy_helper: Self::copy,
        dispose_helper: Self::dispose,
        signature: std::ptr::null(),
    };

    pub fn new(f: F) -> Self {
        Self {
            header: std::cell::UnsafeCell::new(BlockHeader {
                isa: unsafe { &_NSConcreteStackBlock },
                flags: BlockFlags::HAS_COPY_DISPOSE,
                reserved: 0,
                // invoke is a function pointer taking a pointer to the block as a first parameter,
                // but the rest of its type depends on the block parameters and return type.
                invoke: F::invoker() as *const std::ffi::c_void,
            }),
            descriptor: &Self::DESCRIPTOR,
            f: f,
            _ret_marker: PhantomData,
            _args_marker: PhantomData,
        }
    }
}

pub trait BlockInvoke<Ret, Args> {
    fn invoker() -> *const std::ffi::c_void;
}

impl<F, Ret> BlockInvoke<Ret, ()> for F
where
    F: Clone + Fn() -> Ret + 'static,
{
    fn invoker() -> *const std::ffi::c_void {
        unsafe extern "C" fn invoke<F, Ret>(block: &Block<F, Ret, ()>) -> Ret
        where
            F: Clone + Fn() -> Ret + 'static,
        {
            (block.f)()
        }
        invoke::<Self, Ret> as _
    }
}

impl<F, Ret, Arg1> BlockInvoke<Ret, (Arg1,)> for F
where
    F: Clone + Fn(Arg1) -> Ret + 'static,
{
    fn invoker() -> *const std::ffi::c_void {
        unsafe extern "C" fn invoke<F, Ret, Arg1>(block: &Block<F, Ret, (Arg1,)>, arg1: Arg1) -> Ret
        where
            F: Clone + Fn(Arg1) -> Ret,
        {
            (block.f)(arg1)
        }
        invoke::<Self, Ret, Arg1> as _
    }
}

impl<F, Ret, Arg1, Arg2> BlockInvoke<Ret, (Arg1, Arg2)> for F
where
    F: Clone + Fn(Arg1, Arg2) -> Ret + 'static,
{
    fn invoker() -> *const std::ffi::c_void {
        unsafe extern "C" fn invoke<F, Ret, Arg1, Arg2>(
            block: &Block<F, Ret, (Arg1, Arg2)>,
            arg1: Arg1,
            arg2: Arg2,
        ) -> Ret
        where
            F: Clone + Fn(Arg1, Arg2) -> Ret,
        {
            (block.f)(arg1, arg2)
        }
        invoke::<Self, Ret, Arg1, Arg2> as _
    }
}

impl<F, Ret, Arg1, Arg2, Arg3> BlockInvoke<Ret, (Arg1, Arg2, Arg3)> for F
where
    F: Clone + Fn(Arg1, Arg2, Arg3) -> Ret + 'static,
{
    fn invoker() -> *const std::ffi::c_void {
        unsafe extern "C" fn invoke<F, Ret, Arg1, Arg2, Arg3>(
            block: &Block<F, Ret, (Arg1, Arg2, Arg3)>,
            arg1: Arg1,
            arg2: Arg2,
            arg3: Arg3,
        ) -> Ret
        where
            F: Clone + Fn(Arg1, Arg2, Arg3) -> Ret,
        {
            (block.f)(arg1, arg2, arg3)
        }
        invoke::<Self, Ret, Arg1, Arg2, Arg3> as _
    }
}
