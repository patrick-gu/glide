use std::{
    ffi::{CStr, CString},
    ptr,
};

use glide_codegen_llvm::llvm::{
    core::{
        LLVMAddFunction, LLVMAppendBasicBlock, LLVMBuildCall, LLVMBuildCall2,
        LLVMBuildGlobalStringPtr, LLVMBuildRet, LLVMBuildRetVoid, LLVMConstInt, LLVMContextCreate,
        LLVMCreateBuilder, LLVMFunctionType, LLVMGetParam, LLVMInt64TypeInContext,
        LLVMInt8TypeInContext, LLVMModuleCreateWithNameInContext, LLVMPointerType,
        LLVMPositionBuilderAtEnd, LLVMPrintModuleToString, LLVMSetLinkage, LLVMTypeIsSized,
        LLVMTypeOf, LLVMVoidTypeInContext,
    },
    prelude::{LLVMBuilderRef, LLVMContextRef, LLVMModuleRef, LLVMTypeRef, LLVMValueRef},
    LLVMLinkage,
};
use glide_ir::{FuncBody, Ir, Value};

pub fn codegen(ir: &Ir) {
    unsafe {
        let ctx = LLVMContextCreate();
        let module =
            LLVMModuleCreateWithNameInContext(b"glide_codegen_module\0".as_ptr().cast(), ctx);

        let mut llvm_funcs = Vec::new();

        for func in ir.funcs.inner() {
            let name = CString::new(func.name.as_bytes()).unwrap();
            let ty = gen_ty(ctx, &func.signature);
            let llvm_func = LLVMAddFunction(module, name.as_ptr().cast(), ty);
            llvm_funcs.push(llvm_func);
        }
        for (ir_func, &llvm_func) in ir.funcs.inner().iter().zip(llvm_funcs.iter()) {
            // dbg!(&ir_func.name);
            let builder = LLVMCreateBuilder();
            gen_func(ctx, module, builder, &llvm_funcs, llvm_func, &ir_func.body);
        }

        // let invalid = LLVMVerifyModule(
        //     module,
        //     LLVMVerifierFailureAction::LLVMPrintMessageAction,
        //     ptr::null_mut(),
        // ) != 0;
        // if invalid {
        //     panic!()
        // }

        {
            let s = LLVMPrintModuleToString(module);
            extern "C" {
                fn puts(ptr: *const i8);
            }
            puts(s);
        }

        // LLVMDisposeModule(module);
        // LLVMContextDispose(ctx);
    }
}

unsafe fn gen_func(
    ctx: LLVMContextRef,
    module: LLVMModuleRef,
    builder: LLVMBuilderRef,
    llvm_funcs: &[LLVMValueRef],
    llvm_func: LLVMValueRef,
    body: &FuncBody,
) {
    match body {
        FuncBody::Normal(values) => {
            let basic_block = LLVMAppendBasicBlock(llvm_func, b"entry\0".as_ptr().cast());

            LLVMPositionBuilderAtEnd(builder, basic_block);

            let mut llvm_values = Vec::new();
            for value in values {
                // dbg!(value);
                llvm_values.push(gen_value(ctx, builder, llvm_funcs, llvm_func, value));
            }
        }
        FuncBody::Print => {
            let basic_block = LLVMAppendBasicBlock(llvm_func, b"entry\0".as_ptr().cast());
            LLVMPositionBuilderAtEnd(builder, basic_block);

            let mut s = LLVMGetParam(llvm_func, 0);

            let ty = glide_ir::Ty::Func(vec![glide_ir::Ty::String], Box::new(glide_ir::Ty::Void));
            let ty = gen_ty(ctx, &ty);
            let f = LLVMAddFunction(module, "puts\0".as_ptr().cast(), ty);
            LLVMSetLinkage(f, LLVMLinkage::LLVMExternalLinkage);

            LLVMBuildCall2(builder, ty, f, &mut s, 1, "\0".as_ptr().cast());

            LLVMBuildRetVoid(builder);
        }
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

unsafe fn gen_ty_if_not_empty(ctx: LLVMContextRef, ty: &glide_ir::Ty) -> Option<LLVMTypeRef> {
    match ty {
        glide_ir::Ty::Void => None,
        ty => Some(gen_ty(ctx, ty)),
    }
}

unsafe fn gen_value(
    ctx: LLVMContextRef,
    builder: LLVMBuilderRef,
    llvm_funcs: &[LLVMValueRef],
    llvm_func: LLVMValueRef,
    value: &Value,
) -> LLVMValueRef {
    match value {
        Value::Void => ptr::null_mut(),
        Value::ConstantInt(value) => {
            let int64 = LLVMInt64TypeInContext(ctx);
            LLVMConstInt(int64, *value as u64, 0)
        }
        Value::ConstantString(value) => {
            let data = CString::new(&**value).unwrap();
            LLVMBuildGlobalStringPtr(builder, data.as_ptr(), "anonymous_string\0".as_ptr().cast())
        }
        Value::Local(idx) => todo!(),
        Value::Param(idx) => LLVMGetParam(llvm_func, (*idx).try_into().unwrap()),
        Value::Func(id) => llvm_funcs[id.inner()],
        Value::Call(receiver, args) => {
            let receiver = gen_value(ctx, builder, llvm_funcs, llvm_func, &*receiver);
            let mut args: Vec<LLVMValueRef> = args
                .iter()
                .map(|v| gen_value(ctx, builder, llvm_funcs, llvm_func, v))
                .filter(|v| !v.is_null())
                .collect();
            // let v = LLVMBuildCall2(
            //     builder,
            //     LLVMTypeOf(receiver),
            //     receiver,
            //     args.as_mut_ptr(),
            //     args.len().try_into().unwrap(),
            //     "\0".as_ptr().cast(),
            // );
            let v = LLVMBuildCall(
                builder,
                receiver,
                args.as_mut_ptr(),
                args.len().try_into().unwrap(),
                "\0".as_ptr().cast(),
            );
            if LLVMTypeIsSized(LLVMTypeOf(v)) != 0 {
                v
            } else {
                ptr::null_mut()
            }
        }
        Value::Ret(value) => {
            let value = gen_value(ctx, builder, llvm_funcs, llvm_func, value);
            if value.is_null() {
                LLVMBuildRetVoid(builder);
            } else {
                LLVMBuildRet(builder, value);
            }
            ptr::null_mut()
        }
    }
}
