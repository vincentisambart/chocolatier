use bitflags::bitflags;
use clang_sys;
use once_cell;
use std::ffi::{CStr, CString};
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

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
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

fn to_string(clang_str: clang_sys::CXString) -> Option<String> {
    if clang_str.data.is_null() {
        return None;
    }
    unsafe {
        let c = CStr::from_ptr(clang_sys::clang_getCString(clang_str));
        let owned = c.to_str().expect("invalid Rust string").to_string();
        clang_sys::clang_disposeString(clang_str);
        Some(owned)
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

    pub fn kind(&self) -> CursorKind {
        unsafe { clang_sys::clang_getCursorKind(self.raw) }.into()
    }

    pub fn availability(&self) -> Availability {
        unsafe { clang_sys::clang_getCursorAvailability(self.raw) }.into()
    }

    pub fn display_name(&self) -> Option<String> {
        to_string(unsafe { clang_sys::clang_getCursorDisplayName(self.raw) })
    }

    pub fn usr(&self) -> Option<String> {
        to_string(unsafe { clang_sys::clang_getCursorUSR(self.raw) })
    }

    pub fn arguments_len(&self) -> u32 {
        unsafe { clang_sys::clang_Cursor_getNumArguments(self.raw) as _ }
    }

    pub fn arguments(&self) -> Vec<Cursor> {
        (0..self.arguments_len())
            .map(|i| unsafe { Cursor::from_raw(clang_sys::clang_Cursor_getArgument(self.raw, i)) })
            .collect()
    }

    pub fn is_objc_optional(&self) -> bool {
        unsafe { clang_sys::clang_Cursor_isObjCOptional(self.raw) != 0 }
    }

    pub fn canonical_cursor(&self) -> Cursor {
        Cursor::from_raw(unsafe { clang_sys::clang_getCanonicalCursor(self.raw) })
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum CursorKind {
    UnexposedDecl,
    StructDecl,
    UnionDecl,
    ClassDecl,
    EnumDecl,
    FieldDecl,
    EnumConstantDecl,
    FunctionDecl,
    VarDecl,
    ParmDecl,
    ObjCInterfaceDecl,
    ObjCCategoryDecl,
    ObjCProtocolDecl,
    ObjCPropertyDecl,
    ObjCIvarDecl,
    ObjCInstanceMethodDecl,
    ObjCClassMethodDecl,
    ObjCImplementationDecl,
    ObjCCategoryImplDecl,
    TypedefDecl,
    CXXMethod,
    Namespace,
    LinkageSpec,
    Constructor,
    Destructor,
    ConversionFunction,
    TemplateTypeParameter,
    NonTypeTemplateParameter,
    TemplateTemplateParameter,
    FunctionTemplate,
    ClassTemplate,
    ClassTemplatePartialSpecialization,
    NamespaceAlias,
    UsingDirective,
    UsingDeclaration,
    TypeAliasDecl,
    ObjCSynthesizeDecl,
    ObjCDynamicDecl,
    CXXAccessSpecifier,
    ObjCSuperClassRef,
    ObjCProtocolRef,
    ObjCClassRef,
    TypeRef,
    CXXBaseSpecifier,
    TemplateRef,
    NamespaceRef,
    MemberRef,
    LabelRef,
    OverloadedDeclRef,
    VariableRef,
    InvalidFile,
    NoDeclFound,
    NotImplemented,
    InvalidCode,
    UnexposedExpr,
    DeclRefExpr,
    MemberRefExpr,
    CallExpr,
    ObjCMessageExpr,
    BlockExpr,
    IntegerLiteral,
    FloatingLiteral,
    ImaginaryLiteral,
    StringLiteral,
    CharacterLiteral,
    ParenExpr,
    UnaryOperator,
    ArraySubscriptExpr,
    BinaryOperator,
    CompoundAssignOperator,
    ConditionalOperator,
    CStyleCastExpr,
    CompoundLiteralExpr,
    InitListExpr,
    AddrLabelExpr,
    StmtExpr,
    GenericSelectionExpr,
    GNUNullExpr,
    CXXStaticCastExpr,
    CXXDynamicCastExpr,
    CXXReinterpretCastExpr,
    CXXConstCastExpr,
    CXXFunctionalCastExpr,
    CXXTypeidExpr,
    CXXBoolLiteralExpr,
    CXXNullPtrLiteralExpr,
    CXXThisExpr,
    CXXThrowExpr,
    CXXNewExpr,
    CXXDeleteExpr,
    UnaryExpr,
    ObjCStringLiteral,
    ObjCEncodeExpr,
    ObjCSelectorExpr,
    ObjCProtocolExpr,
    ObjCBridgedCastExpr,
    PackExpansionExpr,
    SizeOfPackExpr,
    LambdaExpr,
    ObjCBoolLiteralExpr,
    ObjCSelfExpr,
    OMPArraySectionExpr,
    ObjCAvailabilityCheckExpr,
    FixedPointLiteral,
    UnexposedStmt,
    LabelStmt,
    CompoundStmt,
    CaseStmt,
    DefaultStmt,
    IfStmt,
    SwitchStmt,
    WhileStmt,
    DoStmt,
    ForStmt,
    GotoStmt,
    IndirectGotoStmt,
    ContinueStmt,
    BreakStmt,
    ReturnStmt,
    AsmStmt,
    ObjCAtTryStmt,
    ObjCAtCatchStmt,
    ObjCAtFinallyStmt,
    ObjCAtThrowStmt,
    ObjCAtSynchronizedStmt,
    ObjCAutoreleasePoolStmt,
    ObjCForCollectionStmt,
    CXXCatchStmt,
    CXXTryStmt,
    CXXForRangeStmt,
    SEHTryStmt,
    SEHExceptStmt,
    SEHFinallyStmt,
    MSAsmStmt,
    NullStmt,
    DeclStmt,
    OMPParallelDirective,
    OMPSimdDirective,
    OMPForDirective,
    OMPSectionsDirective,
    OMPSectionDirective,
    OMPSingleDirective,
    OMPParallelForDirective,
    OMPParallelSectionsDirective,
    OMPTaskDirective,
    OMPMasterDirective,
    OMPCriticalDirective,
    OMPTaskyieldDirective,
    OMPBarrierDirective,
    OMPTaskwaitDirective,
    OMPFlushDirective,
    SEHLeaveStmt,
    OMPOrderedDirective,
    OMPAtomicDirective,
    OMPForSimdDirective,
    OMPParallelForSimdDirective,
    OMPTargetDirective,
    OMPTeamsDirective,
    OMPTaskgroupDirective,
    OMPCancellationPointDirective,
    OMPCancelDirective,
    OMPTargetDataDirective,
    OMPTaskLoopDirective,
    OMPTaskLoopSimdDirective,
    OMPDistributeDirective,
    OMPTargetEnterDataDirective,
    OMPTargetExitDataDirective,
    OMPTargetParallelDirective,
    OMPTargetParallelForDirective,
    OMPTargetUpdateDirective,
    OMPDistributeParallelForDirective,
    OMPDistributeParallelForSimdDirective,
    OMPDistributeSimdDirective,
    OMPTargetParallelForSimdDirective,
    OMPTargetSimdDirective,
    OMPTeamsDistributeDirective,
    OMPTeamsDistributeSimdDirective,
    OMPTeamsDistributeParallelForSimdDirective,
    OMPTeamsDistributeParallelForDirective,
    OMPTargetTeamsDirective,
    OMPTargetTeamsDistributeDirective,
    OMPTargetTeamsDistributeParallelForDirective,
    OMPTargetTeamsDistributeParallelForSimdDirective,
    OMPTargetTeamsDistributeSimdDirective,
    TranslationUnit,
    UnexposedAttr,
    IBActionAttr,
    IBOutletAttr,
    IBOutletCollectionAttr,
    CXXFinalAttr,
    CXXOverrideAttr,
    AnnotateAttr,
    AsmLabelAttr,
    PackedAttr,
    PureAttr,
    ConstAttr,
    NoDuplicateAttr,
    CUDAConstantAttr,
    CUDADeviceAttr,
    CUDAGlobalAttr,
    CUDAHostAttr,
    CUDASharedAttr,
    VisibilityAttr,
    DLLExport,
    DLLImport,
    NSReturnsRetained,
    NSReturnsNotRetained,
    NSReturnsAutoreleased,
    NSConsumesSelf,
    NSConsumed,
    ObjCException,
    ObjCNSObject,
    ObjCIndependentClass,
    ObjCPreciseLifetime,
    ObjCReturnsInnerPointer,
    ObjCRequiresSuper,
    ObjCRootClass,
    ObjCSubclassingRestricted,
    ObjCExplicitProtocolImpl,
    ObjCDesignatedInitializer,
    ObjCRuntimeVisible,
    ObjCBoxable,
    FlagEnum,
    PreprocessingDirective,
    MacroDefinition,
    MacroExpansion,
    InclusionDirective,
    ModuleImportDecl,
    TypeAliasTemplateDecl,
    StaticAssert,
    FriendDecl,
    OverloadCandidate,
}

impl From<clang_sys::CXCursorKind> for CursorKind {
    fn from(cursor_kind: clang_sys::CXCursorKind) -> CursorKind {
        use clang_sys::*;
        #[allow(non_upper_case_globals)]
        match cursor_kind {
            CXCursor_UnexposedDecl => CursorKind::UnexposedDecl,
            CXCursor_StructDecl => CursorKind::StructDecl,
            CXCursor_UnionDecl => CursorKind::UnionDecl,
            CXCursor_ClassDecl => CursorKind::ClassDecl,
            CXCursor_EnumDecl => CursorKind::EnumDecl,
            CXCursor_FieldDecl => CursorKind::FieldDecl,
            CXCursor_EnumConstantDecl => CursorKind::EnumConstantDecl,
            CXCursor_FunctionDecl => CursorKind::FunctionDecl,
            CXCursor_VarDecl => CursorKind::VarDecl,
            CXCursor_ParmDecl => CursorKind::ParmDecl,
            CXCursor_ObjCInterfaceDecl => CursorKind::ObjCInterfaceDecl,
            CXCursor_ObjCCategoryDecl => CursorKind::ObjCCategoryDecl,
            CXCursor_ObjCProtocolDecl => CursorKind::ObjCProtocolDecl,
            CXCursor_ObjCPropertyDecl => CursorKind::ObjCPropertyDecl,
            CXCursor_ObjCIvarDecl => CursorKind::ObjCIvarDecl,
            CXCursor_ObjCInstanceMethodDecl => CursorKind::ObjCInstanceMethodDecl,
            CXCursor_ObjCClassMethodDecl => CursorKind::ObjCClassMethodDecl,
            CXCursor_ObjCImplementationDecl => CursorKind::ObjCImplementationDecl,
            CXCursor_ObjCCategoryImplDecl => CursorKind::ObjCCategoryImplDecl,
            CXCursor_TypedefDecl => CursorKind::TypedefDecl,
            CXCursor_CXXMethod => CursorKind::CXXMethod,
            CXCursor_Namespace => CursorKind::Namespace,
            CXCursor_LinkageSpec => CursorKind::LinkageSpec,
            CXCursor_Constructor => CursorKind::Constructor,
            CXCursor_Destructor => CursorKind::Destructor,
            CXCursor_ConversionFunction => CursorKind::ConversionFunction,
            CXCursor_TemplateTypeParameter => CursorKind::TemplateTypeParameter,
            CXCursor_NonTypeTemplateParameter => CursorKind::NonTypeTemplateParameter,
            CXCursor_TemplateTemplateParameter => CursorKind::TemplateTemplateParameter,
            CXCursor_FunctionTemplate => CursorKind::FunctionTemplate,
            CXCursor_ClassTemplate => CursorKind::ClassTemplate,
            CXCursor_ClassTemplatePartialSpecialization => {
                CursorKind::ClassTemplatePartialSpecialization
            }
            CXCursor_NamespaceAlias => CursorKind::NamespaceAlias,
            CXCursor_UsingDirective => CursorKind::UsingDirective,
            CXCursor_UsingDeclaration => CursorKind::UsingDeclaration,
            CXCursor_TypeAliasDecl => CursorKind::TypeAliasDecl,
            CXCursor_ObjCSynthesizeDecl => CursorKind::ObjCSynthesizeDecl,
            CXCursor_ObjCDynamicDecl => CursorKind::ObjCDynamicDecl,
            CXCursor_CXXAccessSpecifier => CursorKind::CXXAccessSpecifier,
            CXCursor_ObjCSuperClassRef => CursorKind::ObjCSuperClassRef,
            CXCursor_ObjCProtocolRef => CursorKind::ObjCProtocolRef,
            CXCursor_ObjCClassRef => CursorKind::ObjCClassRef,
            CXCursor_TypeRef => CursorKind::TypeRef,
            CXCursor_CXXBaseSpecifier => CursorKind::CXXBaseSpecifier,
            CXCursor_TemplateRef => CursorKind::TemplateRef,
            CXCursor_NamespaceRef => CursorKind::NamespaceRef,
            CXCursor_MemberRef => CursorKind::MemberRef,
            CXCursor_LabelRef => CursorKind::LabelRef,
            CXCursor_OverloadedDeclRef => CursorKind::OverloadedDeclRef,
            CXCursor_VariableRef => CursorKind::VariableRef,
            CXCursor_InvalidFile => CursorKind::InvalidFile,
            CXCursor_NoDeclFound => CursorKind::NoDeclFound,
            CXCursor_NotImplemented => CursorKind::NotImplemented,
            CXCursor_InvalidCode => CursorKind::InvalidCode,
            CXCursor_UnexposedExpr => CursorKind::UnexposedExpr,
            CXCursor_DeclRefExpr => CursorKind::DeclRefExpr,
            CXCursor_MemberRefExpr => CursorKind::MemberRefExpr,
            CXCursor_CallExpr => CursorKind::CallExpr,
            CXCursor_ObjCMessageExpr => CursorKind::ObjCMessageExpr,
            CXCursor_BlockExpr => CursorKind::BlockExpr,
            CXCursor_IntegerLiteral => CursorKind::IntegerLiteral,
            CXCursor_FloatingLiteral => CursorKind::FloatingLiteral,
            CXCursor_ImaginaryLiteral => CursorKind::ImaginaryLiteral,
            CXCursor_StringLiteral => CursorKind::StringLiteral,
            CXCursor_CharacterLiteral => CursorKind::CharacterLiteral,
            CXCursor_ParenExpr => CursorKind::ParenExpr,
            CXCursor_UnaryOperator => CursorKind::UnaryOperator,
            CXCursor_ArraySubscriptExpr => CursorKind::ArraySubscriptExpr,
            CXCursor_BinaryOperator => CursorKind::BinaryOperator,
            CXCursor_CompoundAssignOperator => CursorKind::CompoundAssignOperator,
            CXCursor_ConditionalOperator => CursorKind::ConditionalOperator,
            CXCursor_CStyleCastExpr => CursorKind::CStyleCastExpr,
            CXCursor_CompoundLiteralExpr => CursorKind::CompoundLiteralExpr,
            CXCursor_InitListExpr => CursorKind::InitListExpr,
            CXCursor_AddrLabelExpr => CursorKind::AddrLabelExpr,
            CXCursor_StmtExpr => CursorKind::StmtExpr,
            CXCursor_GenericSelectionExpr => CursorKind::GenericSelectionExpr,
            CXCursor_GNUNullExpr => CursorKind::GNUNullExpr,
            CXCursor_CXXStaticCastExpr => CursorKind::CXXStaticCastExpr,
            CXCursor_CXXDynamicCastExpr => CursorKind::CXXDynamicCastExpr,
            CXCursor_CXXReinterpretCastExpr => CursorKind::CXXReinterpretCastExpr,
            CXCursor_CXXConstCastExpr => CursorKind::CXXConstCastExpr,
            CXCursor_CXXFunctionalCastExpr => CursorKind::CXXFunctionalCastExpr,
            CXCursor_CXXTypeidExpr => CursorKind::CXXTypeidExpr,
            CXCursor_CXXBoolLiteralExpr => CursorKind::CXXBoolLiteralExpr,
            CXCursor_CXXNullPtrLiteralExpr => CursorKind::CXXNullPtrLiteralExpr,
            CXCursor_CXXThisExpr => CursorKind::CXXThisExpr,
            CXCursor_CXXThrowExpr => CursorKind::CXXThrowExpr,
            CXCursor_CXXNewExpr => CursorKind::CXXNewExpr,
            CXCursor_CXXDeleteExpr => CursorKind::CXXDeleteExpr,
            CXCursor_UnaryExpr => CursorKind::UnaryExpr,
            CXCursor_ObjCStringLiteral => CursorKind::ObjCStringLiteral,
            CXCursor_ObjCEncodeExpr => CursorKind::ObjCEncodeExpr,
            CXCursor_ObjCSelectorExpr => CursorKind::ObjCSelectorExpr,
            CXCursor_ObjCProtocolExpr => CursorKind::ObjCProtocolExpr,
            CXCursor_ObjCBridgedCastExpr => CursorKind::ObjCBridgedCastExpr,
            CXCursor_PackExpansionExpr => CursorKind::PackExpansionExpr,
            CXCursor_SizeOfPackExpr => CursorKind::SizeOfPackExpr,
            CXCursor_LambdaExpr => CursorKind::LambdaExpr,
            CXCursor_ObjCBoolLiteralExpr => CursorKind::ObjCBoolLiteralExpr,
            CXCursor_ObjCSelfExpr => CursorKind::ObjCSelfExpr,
            CXCursor_OMPArraySectionExpr => CursorKind::OMPArraySectionExpr,
            CXCursor_ObjCAvailabilityCheckExpr => CursorKind::ObjCAvailabilityCheckExpr,
            CXCursor_FixedPointLiteral => CursorKind::FixedPointLiteral,
            CXCursor_UnexposedStmt => CursorKind::UnexposedStmt,
            CXCursor_LabelStmt => CursorKind::LabelStmt,
            CXCursor_CompoundStmt => CursorKind::CompoundStmt,
            CXCursor_CaseStmt => CursorKind::CaseStmt,
            CXCursor_DefaultStmt => CursorKind::DefaultStmt,
            CXCursor_IfStmt => CursorKind::IfStmt,
            CXCursor_SwitchStmt => CursorKind::SwitchStmt,
            CXCursor_WhileStmt => CursorKind::WhileStmt,
            CXCursor_DoStmt => CursorKind::DoStmt,
            CXCursor_ForStmt => CursorKind::ForStmt,
            CXCursor_GotoStmt => CursorKind::GotoStmt,
            CXCursor_IndirectGotoStmt => CursorKind::IndirectGotoStmt,
            CXCursor_ContinueStmt => CursorKind::ContinueStmt,
            CXCursor_BreakStmt => CursorKind::BreakStmt,
            CXCursor_ReturnStmt => CursorKind::ReturnStmt,
            CXCursor_AsmStmt => CursorKind::AsmStmt,
            CXCursor_ObjCAtTryStmt => CursorKind::ObjCAtTryStmt,
            CXCursor_ObjCAtCatchStmt => CursorKind::ObjCAtCatchStmt,
            CXCursor_ObjCAtFinallyStmt => CursorKind::ObjCAtFinallyStmt,
            CXCursor_ObjCAtThrowStmt => CursorKind::ObjCAtThrowStmt,
            CXCursor_ObjCAtSynchronizedStmt => CursorKind::ObjCAtSynchronizedStmt,
            CXCursor_ObjCAutoreleasePoolStmt => CursorKind::ObjCAutoreleasePoolStmt,
            CXCursor_ObjCForCollectionStmt => CursorKind::ObjCForCollectionStmt,
            CXCursor_CXXCatchStmt => CursorKind::CXXCatchStmt,
            CXCursor_CXXTryStmt => CursorKind::CXXTryStmt,
            CXCursor_CXXForRangeStmt => CursorKind::CXXForRangeStmt,
            CXCursor_SEHTryStmt => CursorKind::SEHTryStmt,
            CXCursor_SEHExceptStmt => CursorKind::SEHExceptStmt,
            CXCursor_SEHFinallyStmt => CursorKind::SEHFinallyStmt,
            CXCursor_MSAsmStmt => CursorKind::MSAsmStmt,
            CXCursor_NullStmt => CursorKind::NullStmt,
            CXCursor_DeclStmt => CursorKind::DeclStmt,
            CXCursor_OMPParallelDirective => CursorKind::OMPParallelDirective,
            CXCursor_OMPSimdDirective => CursorKind::OMPSimdDirective,
            CXCursor_OMPForDirective => CursorKind::OMPForDirective,
            CXCursor_OMPSectionsDirective => CursorKind::OMPSectionsDirective,
            CXCursor_OMPSectionDirective => CursorKind::OMPSectionDirective,
            CXCursor_OMPSingleDirective => CursorKind::OMPSingleDirective,
            CXCursor_OMPParallelForDirective => CursorKind::OMPParallelForDirective,
            CXCursor_OMPParallelSectionsDirective => CursorKind::OMPParallelSectionsDirective,
            CXCursor_OMPTaskDirective => CursorKind::OMPTaskDirective,
            CXCursor_OMPMasterDirective => CursorKind::OMPMasterDirective,
            CXCursor_OMPCriticalDirective => CursorKind::OMPCriticalDirective,
            CXCursor_OMPTaskyieldDirective => CursorKind::OMPTaskyieldDirective,
            CXCursor_OMPBarrierDirective => CursorKind::OMPBarrierDirective,
            CXCursor_OMPTaskwaitDirective => CursorKind::OMPTaskwaitDirective,
            CXCursor_OMPFlushDirective => CursorKind::OMPFlushDirective,
            CXCursor_SEHLeaveStmt => CursorKind::SEHLeaveStmt,
            CXCursor_OMPOrderedDirective => CursorKind::OMPOrderedDirective,
            CXCursor_OMPAtomicDirective => CursorKind::OMPAtomicDirective,
            CXCursor_OMPForSimdDirective => CursorKind::OMPForSimdDirective,
            CXCursor_OMPParallelForSimdDirective => CursorKind::OMPParallelForSimdDirective,
            CXCursor_OMPTargetDirective => CursorKind::OMPTargetDirective,
            CXCursor_OMPTeamsDirective => CursorKind::OMPTeamsDirective,
            CXCursor_OMPTaskgroupDirective => CursorKind::OMPTaskgroupDirective,
            CXCursor_OMPCancellationPointDirective => CursorKind::OMPCancellationPointDirective,
            CXCursor_OMPCancelDirective => CursorKind::OMPCancelDirective,
            CXCursor_OMPTargetDataDirective => CursorKind::OMPTargetDataDirective,
            CXCursor_OMPTaskLoopDirective => CursorKind::OMPTaskLoopDirective,
            CXCursor_OMPTaskLoopSimdDirective => CursorKind::OMPTaskLoopSimdDirective,
            CXCursor_OMPDistributeDirective => CursorKind::OMPDistributeDirective,
            CXCursor_OMPTargetEnterDataDirective => CursorKind::OMPTargetEnterDataDirective,
            CXCursor_OMPTargetExitDataDirective => CursorKind::OMPTargetExitDataDirective,
            CXCursor_OMPTargetParallelDirective => CursorKind::OMPTargetParallelDirective,
            CXCursor_OMPTargetParallelForDirective => CursorKind::OMPTargetParallelForDirective,
            CXCursor_OMPTargetUpdateDirective => CursorKind::OMPTargetUpdateDirective,
            CXCursor_OMPDistributeParallelForDirective => {
                CursorKind::OMPDistributeParallelForDirective
            }
            CXCursor_OMPDistributeParallelForSimdDirective => {
                CursorKind::OMPDistributeParallelForSimdDirective
            }
            CXCursor_OMPDistributeSimdDirective => CursorKind::OMPDistributeSimdDirective,
            CXCursor_OMPTargetParallelForSimdDirective => {
                CursorKind::OMPTargetParallelForSimdDirective
            }
            CXCursor_OMPTargetSimdDirective => CursorKind::OMPTargetSimdDirective,
            CXCursor_OMPTeamsDistributeDirective => CursorKind::OMPTeamsDistributeDirective,
            CXCursor_OMPTeamsDistributeSimdDirective => CursorKind::OMPTeamsDistributeSimdDirective,
            CXCursor_OMPTeamsDistributeParallelForSimdDirective => {
                CursorKind::OMPTeamsDistributeParallelForSimdDirective
            }
            CXCursor_OMPTeamsDistributeParallelForDirective => {
                CursorKind::OMPTeamsDistributeParallelForDirective
            }
            CXCursor_OMPTargetTeamsDirective => CursorKind::OMPTargetTeamsDirective,
            CXCursor_OMPTargetTeamsDistributeDirective => {
                CursorKind::OMPTargetTeamsDistributeDirective
            }
            CXCursor_OMPTargetTeamsDistributeParallelForDirective => {
                CursorKind::OMPTargetTeamsDistributeParallelForDirective
            }
            CXCursor_OMPTargetTeamsDistributeParallelForSimdDirective => {
                CursorKind::OMPTargetTeamsDistributeParallelForSimdDirective
            }
            CXCursor_OMPTargetTeamsDistributeSimdDirective => {
                CursorKind::OMPTargetTeamsDistributeSimdDirective
            }
            CXCursor_TranslationUnit => CursorKind::TranslationUnit,
            CXCursor_UnexposedAttr => CursorKind::UnexposedAttr,
            CXCursor_IBActionAttr => CursorKind::IBActionAttr,
            CXCursor_IBOutletAttr => CursorKind::IBOutletAttr,
            CXCursor_IBOutletCollectionAttr => CursorKind::IBOutletCollectionAttr,
            CXCursor_CXXFinalAttr => CursorKind::CXXFinalAttr,
            CXCursor_CXXOverrideAttr => CursorKind::CXXOverrideAttr,
            CXCursor_AnnotateAttr => CursorKind::AnnotateAttr,
            CXCursor_AsmLabelAttr => CursorKind::AsmLabelAttr,
            CXCursor_PackedAttr => CursorKind::PackedAttr,
            CXCursor_PureAttr => CursorKind::PureAttr,
            CXCursor_ConstAttr => CursorKind::ConstAttr,
            CXCursor_NoDuplicateAttr => CursorKind::NoDuplicateAttr,
            CXCursor_CUDAConstantAttr => CursorKind::CUDAConstantAttr,
            CXCursor_CUDADeviceAttr => CursorKind::CUDADeviceAttr,
            CXCursor_CUDAGlobalAttr => CursorKind::CUDAGlobalAttr,
            CXCursor_CUDAHostAttr => CursorKind::CUDAHostAttr,
            CXCursor_CUDASharedAttr => CursorKind::CUDASharedAttr,
            CXCursor_VisibilityAttr => CursorKind::VisibilityAttr,
            CXCursor_DLLExport => CursorKind::DLLExport,
            CXCursor_DLLImport => CursorKind::DLLImport,
            CXCursor_NSReturnsRetained => CursorKind::NSReturnsRetained,
            CXCursor_NSReturnsNotRetained => CursorKind::NSReturnsNotRetained,
            CXCursor_NSReturnsAutoreleased => CursorKind::NSReturnsAutoreleased,
            CXCursor_NSConsumesSelf => CursorKind::NSConsumesSelf,
            CXCursor_NSConsumed => CursorKind::NSConsumed,
            CXCursor_ObjCException => CursorKind::ObjCException,
            CXCursor_ObjCNSObject => CursorKind::ObjCNSObject,
            CXCursor_ObjCIndependentClass => CursorKind::ObjCIndependentClass,
            CXCursor_ObjCPreciseLifetime => CursorKind::ObjCPreciseLifetime,
            CXCursor_ObjCReturnsInnerPointer => CursorKind::ObjCReturnsInnerPointer,
            CXCursor_ObjCRequiresSuper => CursorKind::ObjCRequiresSuper,
            CXCursor_ObjCRootClass => CursorKind::ObjCRootClass,
            CXCursor_ObjCSubclassingRestricted => CursorKind::ObjCSubclassingRestricted,
            CXCursor_ObjCExplicitProtocolImpl => CursorKind::ObjCExplicitProtocolImpl,
            CXCursor_ObjCDesignatedInitializer => CursorKind::ObjCDesignatedInitializer,
            CXCursor_ObjCRuntimeVisible => CursorKind::ObjCRuntimeVisible,
            CXCursor_ObjCBoxable => CursorKind::ObjCBoxable,
            CXCursor_FlagEnum => CursorKind::FlagEnum,
            CXCursor_PreprocessingDirective => CursorKind::PreprocessingDirective,
            CXCursor_MacroDefinition => CursorKind::MacroDefinition,
            CXCursor_MacroExpansion => CursorKind::MacroExpansion,
            CXCursor_InclusionDirective => CursorKind::InclusionDirective,
            CXCursor_ModuleImportDecl => CursorKind::ModuleImportDecl,
            CXCursor_TypeAliasTemplateDecl => CursorKind::TypeAliasTemplateDecl,
            CXCursor_StaticAssert => CursorKind::StaticAssert,
            CXCursor_FriendDecl => CursorKind::FriendDecl,
            CXCursor_OverloadCandidate => CursorKind::OverloadCandidate,
            _ => panic!("invalid cursor kind {:?}", cursor_kind),
        }
    }
}

pub enum Availability {
    Available,
    Deprecated,
    NotAvailable,
    NotAccessible,
}

impl From<clang_sys::CXAvailabilityKind> for Availability {
    fn from(availability: clang_sys::CXAvailabilityKind) -> Availability {
        use clang_sys::*;
        #[allow(non_upper_case_globals)]
        match availability {
            CXAvailability_Available => Availability::Available,
            CXAvailability_Deprecated => Availability::Deprecated,
            CXAvailability_NotAvailable => Availability::NotAvailable,
            CXAvailability_NotAccessible => Availability::NotAccessible,
            _ => panic!("invalid availability {:?}", availability),
        }
    }
}
