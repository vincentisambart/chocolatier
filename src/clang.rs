use bitflags::bitflags;
use clang_sys;
use once_cell;
use std::ffi::CString;
use std::marker::PhantomData;

fn ensure_libclang_loaded() {
    if clang_sys::is_loaded() {
        return;
    }

    use once_cell::sync::Lazy;
    use std::sync::Arc;
    static LIBCLANG: Lazy<Arc<clang_sys::SharedLibrary>> = Lazy::new(|| {
        clang_sys::load().expect("could to find libclang");
        let library = clang_sys::get_library().expect("could not find the library we just loaded");
        println!("loaded libclang from {}", library.path().display());
        library
    });

    clang_sys::set_library(Some(LIBCLANG.clone()));
}

#[derive(Debug)]
pub struct Index {
    ptr: clang_sys::CXIndex,
}

impl Index {
    fn from_ptr(ptr: clang_sys::CXIndex) -> Self {
        assert!(!ptr.is_null());
        Self { ptr }
    }

    pub fn new(exclude: bool, diagnostics: bool) -> Self {
        ensure_libclang_loaded();
        let ptr = unsafe { clang_sys::clang_createIndex(exclude as _, diagnostics as _) };
        Self::from_ptr(ptr)
    }

    pub fn parse<'idx>(
        &'idx self,
        args: &[String],
        file_path: &str,
        unsaved: &[UnsavedFile],
        options: TuOptions,
    ) -> Result<TranslationUnit<'idx>, ClangError> {
        let c_file_path = CString::new(file_path).unwrap();

        let cstr_args: Vec<CString> = args
            .iter()
            .map(|arg| CString::new(arg.clone()).unwrap())
            .collect();
        let c_args: Vec<*const _> = cstr_args.iter().map(|cstr| cstr.as_ptr()).collect();

        let mut c_unsaved: Vec<clang_sys::CXUnsavedFile> = unsaved
            .iter()
            .map(|unsaved| clang_sys::CXUnsavedFile {
                Filename: unsaved.path.as_ptr(),
                Contents: unsaved.contents.as_ptr(),
                Length: unsaved.contents.as_bytes().len() as _,
            })
            .collect();

        let mut tu_ptr = std::ptr::null_mut();
        let err_code = unsafe {
            clang_sys::clang_parseTranslationUnit2(
                self.ptr,
                c_file_path.as_ptr(),
                c_args.as_ptr(),
                c_args.len() as _,
                c_unsaved.as_mut_ptr(),
                c_unsaved.len() as _,
                options.bits(),
                &mut tu_ptr,
            )
        };
        if let Some(error) = ClangError::from_err_code(err_code) {
            Err(error)
        } else {
            Ok(TranslationUnit::from_ptr(tu_ptr))
        }
    }
}

#[derive(Debug)]
pub struct UnsavedFile {
    path: CString,
    contents: CString,
}

impl UnsavedFile {
    fn new(path: &std::path::Path, contents: &str) -> Self {
        let path = CString::new(path.as_os_str().to_str().expect("invalid C string")).unwrap();
        let contents = CString::new(contents).unwrap();
        Self { path, contents }
    }
}

bitflags! {
    pub struct TuOptions: i32 {
        const CACHE_COMPLETION_RESULTS = clang_sys::CXTranslationUnit_CacheCompletionResults;
        const DETAILED_PREPROCESSING_RECORD = clang_sys::CXTranslationUnit_DetailedPreprocessingRecord;
        const BRIEFS_IN_COMPLETION_RESULTS = clang_sys::CXTranslationUnit_IncludeBriefCommentsInCodeCompletion;
        const INCOMPLETE = clang_sys::CXTranslationUnit_Incomplete;
        const SKIP_FUNCTION_BODIES = clang_sys::CXTranslationUnit_SkipFunctionBodies;
        const KEEP_GOING = clang_sys::CXTranslationUnit_KeepGoing;
        const SINGLE_FILE_PARSE = clang_sys::CXTranslationUnit_SingleFileParse;
        const LIMIT_SKIP_FUNCTION_BODIES_TO_PREAMBLE = clang_sys::CXTranslationUnit_LimitSkipFunctionBodiesToPreamble;
        const INCLUDE_ATTRIBUTED_TYPES = clang_sys::CXTranslationUnit_IncludeAttributedTypes;
        const VISIT_IMPLICIT_ATTRIBUTES = clang_sys::CXTranslationUnit_VisitImplicitAttributes;
    }
}

#[derive(Debug)]
pub struct TranslationUnit<'idx> {
    ptr: clang_sys::CXTranslationUnit,
    _marker: PhantomData<&'idx Index>,
}

impl<'idx> TranslationUnit<'idx> {
    fn from_ptr(ptr: clang_sys::CXTranslationUnit) -> TranslationUnit<'idx> {
        assert!(!ptr.is_null());
        Self {
            ptr,
            _marker: PhantomData,
        }
    }

    fn cursor(&self) -> Cursor {
        let raw = unsafe { clang_sys::clang_getTranslationUnitCursor(self.ptr) };
        Cursor::from_raw(raw)
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ClangError {
    Failure,
    Crashed,
    InvalidArguments,
    ASTReadError,
}

impl ClangError {
    pub fn from_err_code(err_code: clang_sys::CXErrorCode) -> Option<ClangError> {
        match err_code {
            clang_sys::CXError_Success => None,
            clang_sys::CXError_Crashed => Some(ClangError::Crashed),
            clang_sys::CXError_InvalidArguments => Some(ClangError::InvalidArguments),
            clang_sys::CXError_ASTReadError => Some(ClangError::ASTReadError),
            _ => Some(ClangError::Failure),
        }
    }
}

pub struct Cursor {
    raw: clang_sys::CXCursor,
    // TODO: Should probably need another field (or marker)
}

impl Cursor {
    fn from_raw(raw: clang_sys::CXCursor) -> Self {
        Self { raw }
    }
}
