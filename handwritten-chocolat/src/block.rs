/// Main documentation sources about C blocks and how they behave:
/// - <https://clang.llvm.org/docs/Block-ABI-Apple.html>
/// - BlocksRuntime in LLVM's `compiler-rt` source code: <https://github.com/llvm/llvm-project/tree/master/compiler-rt/lib/BlocksRuntime>
/// - You can also look at clang or Swift's source code or the LLVM IR or assembly they generate.
///
/// Note that that even if blocks technically can be used from C, they are really for Objective-C.
///
// TODO:
// - Support for returning structures from blocks? That would need adding proper support for signature.
use std::ffi::c_void;
use std::marker::PhantomData;
use std::os::raw::{c_int, c_ulong};

#[repr(C)]
struct Class {
    _private: [u8; 0],
}

#[link(name = "System", kind = "dylib")]
extern "C" {
    static _NSConcreteStackBlock: Class;

    fn _Block_copy(block: *const c_void) -> *mut c_void;
    fn _Block_release(block: *const c_void);
}

/// Indicates that the block descriptor's `copy` and `dispose` should be called.
const BLOCK_HAS_COPY_DISPOSE: c_int = 1 << 25;
/// Indicates that the block has (C++) constructors.
///
/// It looks like something we might be interested in, but looking at the compiler-rt code,
/// it does not seem to do anything when the Objective-C GC is disabled, and Apple does not support that GC anymore.
const BLOCK_HAS_CTOR: c_int = 1 << 26;

#[repr(C)]
struct BlockDescriptor {
    _reserved: c_ulong,
    /// Size of the block this descriptor describes.
    block_size: c_ulong,
    /// Function that takes care of finishing the copy of a block from stack to the heap.
    /// The data has already been `memmove`d from `src` to `dst` when this gets called.
    copy: unsafe extern "C" fn(*mut c_void, *const c_void),
    /// Function called to destruct a block that has been copied to the heap (the system will free memory just after).
    dispose: unsafe extern "C" fn(*mut c_void),
    // `signature` being a pointer will prevent Rust from auto-deriving `Sync` for this structure,
    // and we do not use it (keeping it at `null`) so we could make it a `usize`, but I prefer having the proper type.
    // Of course if it is not `null`, the value it points to must have a `'static` lifetime.
    signature: *const u8,
}
// By default `*const` are not shareable between threads and so Rust won't less us make a `static` from it.
unsafe impl Sync for BlockDescriptor {}

#[repr(C)]
struct BlockHeader {
    isa: *const Class,
    flags: c_int,
    _reserved: c_int,
    // Function pointer taking a pointer to the block as a first parameter,
    // but the rest of its type depends on the block parameters and return type.
    invoke: *const c_void,
    descriptor: &'static BlockDescriptor,
}

#[repr(C)]
struct InnerBlock<F, Args, Ret>
where
    F: BlockInvoke<Args, Ret>,
{
    // We won't change the content of header once created, but use an unsafe cell because the blocks runtime changes for example flags, even possibly from other threads.
    header: std::cell::UnsafeCell<BlockHeader>,
    f: F,
    // We do not own any instance of Ret or Args.
    _ret_marker: PhantomData<*const Ret>,
    _args_marker: PhantomData<*const Args>,
}

#[repr(transparent)]
pub struct StackBlock<F, Args, Ret>
where
    F: BlockInvoke<Args, Ret> + Clone + Send + Sync + 'static,
{
    inner: InnerBlock<F, Args, Ret>,
}

impl<F, Args, Ret> StackBlock<F, Args, Ret>
where
    F: BlockInvoke<Args, Ret> + Clone + Send + Sync + 'static,
{
    unsafe extern "C" fn copy(dst: *mut c_void, src: *const c_void) {
        let src = &*(src as *const InnerBlock<F, Args, Ret>);
        let dst = &mut *(dst as *mut InnerBlock<F, Args, Ret>);

        // The block runtime did a `memmove` of the original block content before calling us,
        // so we should be able to use Rust references without it considered UB.
        (&mut dst.f as *mut F).write(src.f.clone());
    }

    unsafe extern "C" fn dispose(src: *mut c_void) {
        let src = src as *mut InnerBlock<F, Args, Ret>;
        std::ptr::drop_in_place(src);
    }

    const DESCRIPTOR: BlockDescriptor = BlockDescriptor {
        _reserved: 0,
        block_size: std::mem::size_of::<InnerBlock<F, Args, Ret>>() as u64,
        copy: Self::copy,
        dispose: Self::dispose,
        signature: std::ptr::null(),
    };

    pub fn new(f: F) -> Self {
        let inner = InnerBlock {
            header: std::cell::UnsafeCell::new(BlockHeader {
                isa: unsafe { &_NSConcreteStackBlock },
                flags: BLOCK_HAS_COPY_DISPOSE,
                _reserved: 0,
                // invoke is a function pointer taking a pointer to the block as a first parameter,
                // but the rest of its type depends on the block parameters and return type.
                invoke: F::invoker() as *const c_void,
                descriptor: &Self::DESCRIPTOR,
            }),
            f: f,
            _ret_marker: PhantomData,
            _args_marker: PhantomData,
        };
        Self { inner }
    }
}

/// Block that only leaves in the heap.
///
/// Compared with StackBlock, it does not require `Clone`, but it always makes an allocation.
pub struct HeapBlock<F, Args, Ret>
where
    F: BlockInvoke<Args, Ret> + Send + Sync + 'static,
{
    ptr: *const InnerBlock<F, Args, Ret>,
}

impl<F, Args, Ret> HeapBlock<F, Args, Ret>
where
    F: BlockInvoke<Args, Ret> + Send + Sync + 'static,
{
    unsafe extern "C" fn copy(_dst: *mut c_void, _src: *const c_void) {
        // The runtime did a `memmove` for us, nothing to do.
    }

    unsafe extern "C" fn dispose(src: *mut c_void) {
        let src = src as *mut InnerBlock<F, Args, Ret>;
        std::ptr::drop_in_place(src);
    }

    const DESCRIPTOR: BlockDescriptor = BlockDescriptor {
        _reserved: 0,
        block_size: std::mem::size_of::<InnerBlock<F, Args, Ret>>() as u64,
        copy: Self::copy,
        dispose: Self::dispose,
        signature: std::ptr::null(),
    };

    pub fn new(f: F) -> Self {
        let inner = InnerBlock {
            header: std::cell::UnsafeCell::new(BlockHeader {
                isa: unsafe { &_NSConcreteStackBlock },
                flags: BLOCK_HAS_COPY_DISPOSE,
                _reserved: 0,
                // invoke is a function pointer taking a pointer to the block as a first parameter,
                // but the rest of its type depends on the block parameters and return type.
                invoke: F::invoker() as *const c_void,
                descriptor: &Self::DESCRIPTOR,
            }),
            f: f,
            _ret_marker: PhantomData,
            _args_marker: PhantomData,
        };
        let ptr = unsafe {
            _Block_copy(&inner as *const InnerBlock<F, Args, Ret> as _)
                as *const InnerBlock<F, Args, Ret>
        };
        // Everything was copied to the heap by `_Block_copy`,
        // and when the internal ref count gets to zero the copy will be dropped by `dispose`.
        // `clone` was not called so the original version should not be dropped.
        std::mem::forget(inner);
        Self { ptr }
    }
}

impl<F, Args, Ret> Drop for HeapBlock<F, Args, Ret>
where
    F: BlockInvoke<Args, Ret> + Send + Sync + 'static,
{
    fn drop(&mut self) {
        unsafe { _Block_release(self.ptr as _) };
    }
}

impl<F, Args, Ret> Clone for HeapBlock<F, Args, Ret>
where
    F: BlockInvoke<Args, Ret> + Send + Sync + 'static,
{
    fn clone(&self) -> Self {
        // When calling `_Block_copy` on a block already on the heap, no copy will occur, its ref count will just increase.
        let ptr = unsafe { _Block_copy(self.ptr as _) as *const InnerBlock<F, Args, Ret> };
        Self { ptr }
    }
}

impl<F, Args, Ret> From<StackBlock<F, Args, Ret>> for HeapBlock<F, Args, Ret>
where
    F: BlockInvoke<Args, Ret> + Send + Sync + 'static + Clone,
{
    fn from(block: StackBlock<F, Args, Ret>) -> Self {
        let ptr = unsafe {
            _Block_copy(&block.inner as *const InnerBlock<F, Args, Ret> as _)
                as *const InnerBlock<F, Args, Ret>
        };
        // Note that this block does not have the same `copy` as a normal `HeapBlock`,
        // but it is called by the runtime, and it was just called by `_Block_copy` anyway,
        // so that does not matter here.
        Self { ptr }
    }
}

pub trait BlockInvoke<Args, Ret> {
    fn invoker() -> *const c_void;
}

impl<F, Ret> BlockInvoke<(), Ret> for F
where
    F: Fn() -> Ret,
{
    fn invoker() -> *const c_void {
        unsafe extern "C" fn invoke<F, Ret>(block: &InnerBlock<F, (), Ret>) -> Ret
        where
            F: Fn() -> Ret,
        {
            (block.f)()
        }
        invoke::<Self, Ret> as _
    }
}

impl<F, Ret, Arg1> BlockInvoke<(Arg1,), Ret> for F
where
    F: Fn(Arg1) -> Ret,
{
    fn invoker() -> *const c_void {
        unsafe extern "C" fn invoke<F, Arg1, Ret>(
            block: &InnerBlock<F, (Arg1,), Ret>,
            arg1: Arg1,
        ) -> Ret
        where
            F: Fn(Arg1) -> Ret,
        {
            (block.f)(arg1)
        }
        invoke::<Self, Arg1, Ret> as _
    }
}

impl<F, Ret, Arg1, Arg2> BlockInvoke<(Arg1, Arg2), Ret> for F
where
    F: Fn(Arg1, Arg2) -> Ret,
{
    fn invoker() -> *const c_void {
        unsafe extern "C" fn invoke<F, Arg1, Arg2, Ret>(
            block: &InnerBlock<F, (Arg1, Arg2), Ret>,
            arg1: Arg1,
            arg2: Arg2,
        ) -> Ret
        where
            F: Fn(Arg1, Arg2) -> Ret,
        {
            (block.f)(arg1, arg2)
        }
        invoke::<Self, Arg1, Arg2, Ret> as _
    }
}

impl<F, Ret, Arg1, Arg2, Arg3> BlockInvoke<(Arg1, Arg2, Arg3), Ret> for F
where
    F: Fn(Arg1, Arg2, Arg3) -> Ret,
{
    fn invoker() -> *const c_void {
        unsafe extern "C" fn invoke<F, Arg1, Arg2, Arg3, Ret>(
            block: &InnerBlock<F, (Arg1, Arg2, Arg3), Ret>,
            arg1: Arg1,
            arg2: Arg2,
            arg3: Arg3,
        ) -> Ret
        where
            F: Fn(Arg1, Arg2, Arg3) -> Ret,
        {
            (block.f)(arg1, arg2, arg3)
        }
        invoke::<Self, Arg1, Arg2, Arg3, Ret> as _
    }
}

/// A type erased block.
///
/// It can reference either a stack or heap block.
pub struct Block<'a, Args, Ret> {
    ptr: *const c_void,
    /// Whether we own the block or not. We always own heap blocks, and never stack block.
    owning_ptr: bool,
    _lifetime_marker: PhantomData<&'a BlockHeader>,
    // We do not own any instance of Ret or Args.
    _ret_marker: PhantomData<*const Ret>,
    _args_marker: PhantomData<*const Args>,
}

impl<'a, Args, Ret> Block<'a, Args, Ret> {
    fn from_stack<F>(stack_block: &'a StackBlock<F, Args, Ret>) -> Self
    where
        F: BlockInvoke<Args, Ret> + Clone + Send + Sync + 'static,
    {
        Block {
            ptr: stack_block as *const StackBlock<F, Args, Ret> as _,
            owning_ptr: false,
            _lifetime_marker: PhantomData,
            _ret_marker: PhantomData,
            _args_marker: PhantomData,
        }
    }

    fn into_owned(self) -> Block<'static, Args, Ret> {
        let ptr = if self.owning_ptr {
            self.ptr
        } else {
            unsafe { _Block_copy(self.ptr) }
        };
        Block {
            ptr: ptr,
            owning_ptr: true,
            _lifetime_marker: PhantomData,
            _ret_marker: PhantomData,
            _args_marker: PhantomData,
        }
    }

    fn ptr(&self) -> *const c_void {
        self.ptr
    }
}

impl<Args, Ret> Block<'static, Args, Ret> {
    fn from_heap<F>(heap_block: HeapBlock<F, Args, Ret>) -> Self
    where
        F: BlockInvoke<Args, Ret> + Clone + Send + Sync + 'static,
    {
        let block = Block {
            ptr: heap_block.ptr as _,
            owning_ptr: true,
            _lifetime_marker: PhantomData,
            _ret_marker: PhantomData,
            _args_marker: PhantomData,
        };
        // We get the ownership and will take care of releasing.
        std::mem::forget(heap_block);
        block
    }
}

impl<'a, Args, Ret> Drop for Block<'a, Args, Ret> {
    fn drop(&mut self) {
        if self.owning_ptr {
            unsafe { _Block_release(self.ptr as _) }
        }
    }
}
