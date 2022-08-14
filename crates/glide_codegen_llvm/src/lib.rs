use std::{ffi::CStr, marker::PhantomData};

use llvm_sys::{
    core::{
        LLVMAddFunction, LLVMAppendBasicBlockInContext, LLVMConstInt, LLVMConstStringInContext,
        LLVMContextCreate, LLVMContextDispose, LLVMCreateBuilderInContext, LLVMDisposeBuilder,
        LLVMDisposeModule, LLVMDisposePassManager, LLVMFunctionType,
        LLVMInitializeFunctionPassManager, LLVMInt64TypeInContext, LLVMInt8TypeInContext,
        LLVMModuleCreateWithNameInContext, LLVMPointerType, LLVMPositionBuilderAtEnd,
        LLVMPrintModuleToString, LLVMRunFunctionPassManager, LLVMVoidTypeInContext,
    },
    prelude::{
        LLVMBasicBlockRef, LLVMBuilderRef, LLVMContextRef, LLVMModuleRef, LLVMPassManagerRef,
        LLVMTypeRef, LLVMValueRef,
    },
    transforms::scalar::{
        LLVMAddCFGSimplificationPass, LLVMAddGVNPass, LLVMAddInstructionCombiningPass,
        LLVMAddReassociatePass,
    },
};

pub use llvm_sys as llvm;

pub struct Context {
    ptr: LLVMContextRef,
}

impl Context {
    pub fn new() -> Self {
        let ptr = unsafe { LLVMContextCreate() };
        Self { ptr }
    }
}

impl Default for Context {
    fn default() -> Self {
        Self::new()
    }
}

impl Drop for Context {
    fn drop(&mut self) {
        unsafe {
            LLVMContextDispose(self.ptr);
        }
    }
}

pub struct Module<'ctx> {
    ptr: LLVMModuleRef,
    phantom: PhantomData<&'ctx Context>,
}

impl<'ctx> Module<'ctx> {
    pub fn new(name: &CStr, context: &'ctx Context) -> Self {
        let name = name.as_ptr();
        let ptr = unsafe { LLVMModuleCreateWithNameInContext(name, context.ptr) };
        Self {
            ptr,
            phantom: PhantomData,
        }
    }

    pub fn print(&self) {
        let ptr = unsafe { LLVMPrintModuleToString(self.ptr) };
        extern "C" {
            fn puts(ptr: *const i8);
        }
        unsafe {
            puts(ptr);
        }
    }
}

impl<'ctx> Drop for Module<'ctx> {
    fn drop(&mut self) {
        unsafe {
            LLVMDisposeModule(self.ptr);
        }
    }
}

pub struct Builder<'ctx> {
    ptr: LLVMBuilderRef,
    phantom: PhantomData<&'ctx Context>,
}

impl<'ctx> Builder<'ctx> {
    pub fn new(context: &'ctx Context) -> Self {
        let ptr = unsafe { LLVMCreateBuilderInContext(context.ptr) };
        Self {
            ptr,
            phantom: PhantomData,
        }
    }

    pub fn position_at_end<'mo>(&mut self, basic_block: &BasicBlock<'ctx, 'mo>) {
        unsafe {
            LLVMPositionBuilderAtEnd(self.ptr, basic_block.ptr);
        }
    }
}

impl<'ctx> Drop for Builder<'ctx> {
    fn drop(&mut self) {
        unsafe {
            LLVMDisposeBuilder(self.ptr);
        }
    }
}

pub struct PassManager<'ctx, 'mo> {
    ptr: LLVMPassManagerRef,
    phantom: PhantomData<&'mo Module<'ctx>>,
}

impl<'ctx, 'mo> PassManager<'ctx, 'mo> {
    pub fn new_function(module: &'mo Module<'ctx>) -> Self {
        let ptr = todo!();
        // let ptr = unsafe { LLVMCreateFunctionPassManager(module.ptr) };
        Self {
            ptr,
            phantom: PhantomData,
        }
    }

    pub fn add_instruction_combining(&mut self) {
        unsafe {
            LLVMAddInstructionCombiningPass(self.ptr);
        }
    }

    pub fn add_reassociate(&mut self) {
        unsafe {
            LLVMAddReassociatePass(self.ptr);
        }
    }

    pub fn add_gvn(&mut self) {
        unsafe {
            LLVMAddGVNPass(self.ptr);
        }
    }

    pub fn add_cfg_simplification(&mut self) {
        unsafe {
            LLVMAddCFGSimplificationPass(self.ptr);
        }
    }

    pub fn initialize(&mut self) -> bool {
        let b = unsafe { LLVMInitializeFunctionPassManager(self.ptr) };
        b != 0
    }

    fn run_function(&mut self, value: &mut Value<'ctx>) -> bool {
        let b = unsafe { LLVMRunFunctionPassManager(self.ptr, value.ptr) };
        b != 0
    }
}

impl<'ctx, 'mo> Drop for PassManager<'ctx, 'mo> {
    fn drop(&mut self) {
        unsafe {
            LLVMDisposePassManager(self.ptr);
        }
    }
}

#[repr(transparent)]
pub struct Type<'ctx> {
    ptr: LLVMTypeRef,
    phantom: PhantomData<&'ctx Context>,
}

impl<'ctx> Type<'ctx> {
    pub fn int8(context: &'ctx Context) -> Self {
        let ptr = unsafe { LLVMInt8TypeInContext(context.ptr) };
        Self {
            ptr,
            phantom: PhantomData,
        }
    }

    pub fn int64(context: &'ctx Context) -> Self {
        let ptr = unsafe { LLVMInt64TypeInContext(context.ptr) };
        Self {
            ptr,
            phantom: PhantomData,
        }
    }

    pub fn void(context: &'ctx Context) -> Self {
        let ptr = unsafe { LLVMVoidTypeInContext(context.ptr) };
        Self {
            ptr,
            phantom: PhantomData,
        }
    }

    pub fn pointer(element_type: &Type<'ctx>, address_space: u32) -> Self {
        let ptr = unsafe { LLVMPointerType(element_type.ptr, address_space) };
        Self {
            ptr,
            phantom: PhantomData,
        }
    }

    pub fn function(
        return_type: &Type<'ctx>,
        param_types: &mut [Type<'ctx>],
        is_var_arg: bool,
    ) -> Self {
        let param_types_ptr = param_types.as_mut_ptr().cast();
        let param_count = param_types.len().try_into().unwrap();
        let ptr = unsafe {
            LLVMFunctionType(
                return_type.ptr,
                param_types_ptr,
                param_count,
                if is_var_arg { 1 } else { 0 },
            )
        };
        Self {
            ptr,
            phantom: PhantomData,
        }
    }
}

pub struct Value<'ctx> {
    ptr: LLVMValueRef,
    phantom: PhantomData<&'ctx Context>,
}

impl<'ctx> Value<'ctx> {
    pub fn constant_int(ty: Type<'ctx>, n: u64, sign_extend: bool) -> Self {
        let ptr = unsafe { LLVMConstInt(ty.ptr, n, if sign_extend { 1 } else { 0 }) };
        Self {
            ptr,
            phantom: PhantomData,
        }
    }

    pub fn constant_string(context: &'ctx Context, data: &[u8], dont_null_terminate: bool) -> Self {
        let ptr = unsafe {
            LLVMConstStringInContext(
                context.ptr,
                data.as_ptr().cast(),
                data.len().try_into().unwrap(),
                if dont_null_terminate { 1 } else { 0 },
            )
        };
        Self {
            ptr,
            phantom: PhantomData,
        }
    }
}

pub struct Function<'ctx, 'mo> {
    ptr: LLVMValueRef,
    phantom: PhantomData<&'mo Module<'ctx>>,
}

impl<'ctx, 'mo> Function<'ctx, 'mo> {
    pub fn new(module: &'mo Module, name: &CStr, ty: &Type<'ctx>) -> Self {
        let ptr = unsafe { LLVMAddFunction(module.ptr, name.as_ptr(), ty.ptr) };
        Self {
            ptr,
            phantom: PhantomData,
        }
    }

    pub fn append_basic_block(
        &mut self,
        context: &'ctx Context,
        name: &CStr,
    ) -> BasicBlock<'ctx, 'mo> {
        let ptr = unsafe { LLVMAppendBasicBlockInContext(context.ptr, self.ptr, name.as_ptr()) };
        BasicBlock {
            ptr,
            phantom: PhantomData,
        }
    }
}

pub struct BasicBlock<'ctx, 'mo> {
    ptr: LLVMBasicBlockRef,
    phantom: PhantomData<&'mo Function<'ctx, 'mo>>,
}

impl<'ctx, 'mo> BasicBlock<'ctx, 'mo> {
    pub fn new(context: &'ctx Context, function: &'mo Function<'ctx, 'mo>, name: &CStr) -> Self {
        let ptr =
            unsafe { LLVMAppendBasicBlockInContext(context.ptr, function.ptr, name.as_ptr()) };
        Self {
            ptr,
            phantom: PhantomData,
        }
    }
}
