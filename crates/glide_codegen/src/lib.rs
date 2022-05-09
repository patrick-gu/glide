use std::ffi::{CStr, CString};

use glide_codegen_llvm::{
    llvm::{
        analysis::{LLVMVerifierFailureAction, LLVMVerifyFunction},
        core::{
            LLVMAddFunction, LLVMAppendBasicBlock, LLVMBuildCall2, LLVMBuildRetVoid,
            LLVMContextCreate, LLVMContextDispose, LLVMCreateBuilder, LLVMDisposeModule,
            LLVMFunctionType, LLVMInt64TypeInContext, LLVMInt8TypeInContext,
            LLVMModuleCreateWithNameInContext, LLVMPointerType, LLVMPositionBuilder,
            LLVMPositionBuilderAtEnd, LLVMPrintModuleToString, LLVMVoidTypeInContext,
        },
        object::LLVMDisposeBinary,
        prelude::{LLVMBuilderRef, LLVMContextRef, LLVMTypeRef, LLVMValueRef},
    },
    BasicBlock, Builder, Context, Module,
};
use glide_ir::{Func, FuncBody, Ir};

pub fn codegen(ir: &Ir) {
    unsafe {
        let context = LLVMContextCreate();
        let module =
            LLVMModuleCreateWithNameInContext(b"glide_codegen_module\0".as_ptr().cast(), context);

        for func in ir.funcs.inner() {
            let name = CString::new(func.name.as_bytes()).unwrap();
            let ty = gen_ty(context, &func.signature);
            let function = LLVMAddFunction(module, name.as_ptr().cast(), ty);
            let basic_block = LLVMAppendBasicBlock(function, b"entry\0".as_ptr().cast());
            let builder = LLVMCreateBuilder();

            LLVMPositionBuilderAtEnd(builder, basic_block);
            LLVMBuildRetVoid(builder);

            let invalid =
                LLVMVerifyFunction(function, LLVMVerifierFailureAction::LLVMPrintMessageAction)
                    != 0;
            if invalid {
                panic!()
            }
        }

        {
            let s = LLVMPrintModuleToString(module);
            extern "C" {
                fn puts(ptr: *const i8);
            }
            puts(s);
        }

        LLVMDisposeModule(module);
        LLVMContextDispose(context);
    }
}

unsafe fn gen_ty(ctx: LLVMContextRef, ty: &glide_ir::Ty) -> LLVMTypeRef {
    match ty {
        glide_ir::Ty::Void => LLVMVoidTypeInContext(ctx),
        glide_ir::Ty::Int => LLVMInt64TypeInContext(ctx),
        glide_ir::Ty::String => {
            let i8 = LLVMInt8TypeInContext(ctx);
            LLVMPointerType(i8, 0)
        }
        glide_ir::Ty::Slice(inner) => todo!(),
        glide_ir::Ty::Func(params, ret) => {
            let mut params: Vec<LLVMTypeRef> = params
                .iter()
                .filter_map(|ty| gen_ty_if_not_empty(ctx, ty))
                .collect();
            let ret = gen_ty(ctx, ret);
            LLVMFunctionType(
                ret,
                params.as_mut_ptr(),
                params.len().try_into().unwrap(),
                0,
            )
        }
    }
}

unsafe fn gen_ty_if_not_empty<'ctx>(ctx: LLVMContextRef, ty: &glide_ir::Ty) -> Option<LLVMTypeRef> {
    match ty {
        glide_ir::Ty::Void => None,
        ty => Some(gen_ty(ctx, ty)),
    }
}

fn gen_value<'ctx>(context: &'ctx Context, value: glide_ir::Value) -> LLVMValueRef {
    match value {
        glide_ir::Value::Void => todo!(),
        glide_ir::Value::ConstantInt(value) => todo!(),
        glide_ir::Value::ConstantString(data) => {
            todo!()
        }
        glide_ir::Value::Local(_) => todo!(),
        glide_ir::Value::Param(_) => todo!(),
        glide_ir::Value::Func(_) => todo!(),
        glide_ir::Value::Call(_, _) => todo!(),
        glide_ir::Value::Ret(_) => todo!(),
    }
}

unsafe fn gen_func(body: &FuncBody, builder: LLVMBuilderRef) {
    match body {
        glide_ir::FuncBody::Normal(_) => todo!(),
        glide_ir::FuncBody::Print => {
            todo!()
        }
    }
}
