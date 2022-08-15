use std::ffi::CString;

use glide_codegen_llvm::llvm::{
    core::{
        LLVMAddFunction, LLVMAddIncoming, LLVMAppendBasicBlock, LLVMAppendBasicBlockInContext,
        LLVMAppendExistingBasicBlock, LLVMBuildAdd, LLVMBuildBr, LLVMBuildCall2, LLVMBuildCondBr,
        LLVMBuildGlobalStringPtr, LLVMBuildICmp, LLVMBuildPhi, LLVMBuildRet, LLVMBuildSub,
        LLVMConstInt, LLVMConstStructInContext, LLVMContextCreate, LLVMCreateBasicBlockInContext,
        LLVMCreateBuilder, LLVMFunctionType, LLVMGetElementType, LLVMGetInsertBlock, LLVMGetParam,
        LLVMGetReturnType, LLVMInt1TypeInContext, LLVMInt32TypeInContext, LLVMInt64TypeInContext,
        LLVMInt8TypeInContext, LLVMModuleCreateWithNameInContext, LLVMPointerType,
        LLVMPositionBuilderAtEnd, LLVMPrintModuleToString, LLVMSetLinkage, LLVMStructTypeInContext,
        LLVMTypeOf,
    },
    prelude::{LLVMBuilderRef, LLVMContextRef, LLVMModuleRef, LLVMTypeRef, LLVMValueRef},
    LLVMIntPredicate, LLVMLinkage,
};
use glide_ir::{Func, FuncBody, Ir, Value};

pub fn codegen(ir: &Ir) {
    unsafe {
        let ctx = LLVMContextCreate();
        let module =
            LLVMModuleCreateWithNameInContext(b"glide_codegen_module\0".as_ptr().cast(), ctx);

        let mut llvm_funcs = Vec::new();

        for func in ir.funcs.inner() {
            let name = CString::new(func.name.as_bytes()).unwrap();
            let ty = LLVMGetElementType(gen_ty(ctx, &func.signature));
            let llvm_func = LLVMAddFunction(module, name.as_ptr().cast(), ty);
            llvm_funcs.push(llvm_func);
        }
        for (ir_func, &llvm_func) in ir.funcs.inner().iter().zip(llvm_funcs.iter()) {
            // dbg!(&ir_func.name);
            let builder = LLVMCreateBuilder();
            gen_func(ctx, module, builder, &llvm_funcs, llvm_func, ir_func);
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
    func: &Func,
) {
    match &func.body {
        FuncBody::Placeholder => unreachable!(),
        FuncBody::Normal(value) => {
            let mut locals = Vec::new();
            let num_params = match &func.signature {
                glide_ir::Ty::Func(params, _) => params.len(),
                _ => unreachable!(),
            };
            for idx in 0..num_params {
                locals.push(LLVMGetParam(llvm_func, idx.try_into().unwrap()));
            }

            let basic_block = LLVMAppendBasicBlock(llvm_func, b"entry\0".as_ptr().cast());
            LLVMPositionBuilderAtEnd(builder, basic_block);

            gen_value(ctx, builder, llvm_funcs, llvm_func, &mut locals, value);
        }
        FuncBody::Print => {
            let basic_block = LLVMAppendBasicBlock(llvm_func, b"entry\0".as_ptr().cast());
            LLVMPositionBuilderAtEnd(builder, basic_block);

            let mut s = LLVMGetParam(llvm_func, 0);

            let (puts, puts_ty) = {
                let ty =
                    glide_ir::Ty::Func(vec![glide_ir::Ty::String], Box::new(glide_ir::Ty::Void));
                let ty = LLVMGetElementType(gen_ty(ctx, &ty));
                let f = LLVMAddFunction(module, "puts\0".as_ptr().cast(), ty);
                LLVMSetLinkage(f, LLVMLinkage::LLVMExternalLinkage);
                (f, ty)
            };

            LLVMBuildCall2(builder, puts_ty, puts, &mut s, 1, "\0".as_ptr().cast());

            LLVMBuildRet(builder, empty_struct_value(ctx));
        }
        FuncBody::PrintInt => {
            let basic_block = LLVMAppendBasicBlock(llvm_func, b"entry\0".as_ptr().cast());
            LLVMPositionBuilderAtEnd(builder, basic_block);

            let value = LLVMGetParam(llvm_func, 0);

            let printf_format = LLVMBuildGlobalStringPtr(
                builder,
                "%lld\n\0".as_ptr().cast(),
                "printInt_printf_format\0".as_ptr().cast(),
            );

            let (printf, printf_ty) = {
                let mut param = gen_ty(ctx, &glide_ir::Ty::String);
                let ty = LLVMFunctionType(LLVMInt32TypeInContext(ctx), &mut param, 1, 1);

                let f = LLVMAddFunction(module, "printf\0".as_ptr().cast(), ty);
                LLVMSetLinkage(f, LLVMLinkage::LLVMExternalLinkage);
                (f, ty)
            };

            let mut args = [printf_format, value];

            LLVMBuildCall2(
                builder,
                printf_ty,
                printf,
                args.as_mut_ptr(),
                2,
                "\0".as_ptr().cast(),
            );

            LLVMBuildRet(builder, empty_struct_value(ctx));
        }
        FuncBody::Add => {
            let basic_block = LLVMAppendBasicBlock(llvm_func, b"entry\0".as_ptr().cast());
            LLVMPositionBuilderAtEnd(builder, basic_block);

            let lhs = LLVMGetParam(llvm_func, 0);
            let rhs = LLVMGetParam(llvm_func, 1);

            let ret = LLVMBuildAdd(builder, lhs, rhs, "add\0".as_ptr().cast());
            LLVMBuildRet(builder, ret);
        }
        FuncBody::Sub => {
            let basic_block = LLVMAppendBasicBlock(llvm_func, b"entry\0".as_ptr().cast());
            LLVMPositionBuilderAtEnd(builder, basic_block);

            let lhs = LLVMGetParam(llvm_func, 0);
            let rhs = LLVMGetParam(llvm_func, 1);

            let ret = LLVMBuildSub(builder, lhs, rhs, "sub\0".as_ptr().cast());
            LLVMBuildRet(builder, ret);
        }
        FuncBody::EqInt => {
            let basic_block = LLVMAppendBasicBlock(llvm_func, b"entry\0".as_ptr().cast());
            LLVMPositionBuilderAtEnd(builder, basic_block);

            let lhs = LLVMGetParam(llvm_func, 0);
            let rhs = LLVMGetParam(llvm_func, 1);

            let ret = LLVMBuildICmp(
                builder,
                LLVMIntPredicate::LLVMIntEQ,
                lhs,
                rhs,
                "sub\0".as_ptr().cast(),
            );
            LLVMBuildRet(builder, ret);
        }
    }
}

unsafe fn gen_ty(ctx: LLVMContextRef, ty: &glide_ir::Ty) -> LLVMTypeRef {
    match ty {
        glide_ir::Ty::Void => empty_struct_type(ctx),
        glide_ir::Ty::Int => LLVMInt64TypeInContext(ctx),
        glide_ir::Ty::Bool => LLVMInt1TypeInContext(ctx),
        glide_ir::Ty::String => {
            let i8 = LLVMInt8TypeInContext(ctx);
            LLVMPointerType(i8, 0)
        }
        glide_ir::Ty::Slice(inner) => todo!(),
        glide_ir::Ty::Func(params, ret) => {
            let mut params: Vec<LLVMTypeRef> = params.iter().map(|ty| gen_ty(ctx, ty)).collect();
            let ret = gen_ty(ctx, ret);
            let function = LLVMFunctionType(
                ret,
                params.as_mut_ptr(),
                params.len().try_into().unwrap(),
                0,
            );
            LLVMPointerType(function, 0)
        }
    }
}

unsafe fn gen_value(
    ctx: LLVMContextRef,
    builder: LLVMBuilderRef,
    llvm_funcs: &[LLVMValueRef],
    llvm_func: LLVMValueRef,
    locals: &mut Vec<LLVMValueRef>,
    value: &Value,
) -> LLVMValueRef {
    match value {
        Value::Void => empty_struct_value(ctx),
        Value::True => {
            let int1 = LLVMInt1TypeInContext(ctx);
            LLVMConstInt(int1, 1, 0)
        }
        Value::False => {
            let int1 = LLVMInt1TypeInContext(ctx);
            LLVMConstInt(int1, 0, 0)
        }
        Value::ConstantInt(value) => {
            let int64 = LLVMInt64TypeInContext(ctx);
            LLVMConstInt(int64, *value as u64, 0)
        }
        Value::ConstantString(value) => {
            let data = CString::new(&**value).unwrap();
            LLVMBuildGlobalStringPtr(builder, data.as_ptr(), "anonymous_string\0".as_ptr().cast())
        }
        Value::Local(idx) => locals[*idx],
        Value::Func(id) => llvm_funcs[id.inner()],
        Value::Call(receiver, args) => {
            let receiver = gen_value(ctx, builder, llvm_funcs, llvm_func, locals, &*receiver);
            let mut args: Vec<LLVMValueRef> = args
                .iter()
                .map(|v| gen_value(ctx, builder, llvm_funcs, llvm_func, locals, v))
                .collect();
            let return_type = LLVMGetReturnType(LLVMTypeOf(receiver));
            LLVMBuildCall2(
                builder,
                return_type,
                receiver,
                args.as_mut_ptr(),
                args.len().try_into().unwrap(),
                "\0".as_ptr().cast(),
            )
        }
        Value::Ret(value) => {
            let value = gen_value(ctx, builder, llvm_funcs, llvm_func, locals, value);
            LLVMBuildRet(builder, value);
            empty_struct_value(ctx)
        }
        Value::RetVoid(value) => {
            gen_value(ctx, builder, llvm_funcs, llvm_func, locals, value);
            LLVMBuildRet(builder, empty_struct_value(ctx));
            empty_struct_value(ctx)
        }
        Value::If { cond, then, els } => {
            let cond = gen_value(ctx, builder, llvm_funcs, llvm_func, locals, cond);

            let mut then_block =
                LLVMAppendBasicBlockInContext(ctx, llvm_func, b"then\0".as_ptr().cast());

            if let Some(els) = els {
                let mut else_block = LLVMCreateBasicBlockInContext(ctx, "else\0".as_ptr().cast());
                let after_block = LLVMCreateBasicBlockInContext(ctx, "after\0".as_ptr().cast());

                LLVMBuildCondBr(builder, cond, then_block, else_block);

                LLVMPositionBuilderAtEnd(builder, then_block);
                let mut then_value = gen_value(ctx, builder, llvm_funcs, llvm_func, locals, then);
                LLVMBuildBr(builder, after_block);
                then_block = LLVMGetInsertBlock(builder);

                LLVMAppendExistingBasicBlock(llvm_func, else_block);
                LLVMPositionBuilderAtEnd(builder, else_block);
                let mut else_value = gen_value(ctx, builder, llvm_funcs, llvm_func, locals, els);
                LLVMBuildBr(builder, after_block);
                else_block = LLVMGetInsertBlock(builder);

                LLVMAppendExistingBasicBlock(llvm_func, after_block);
                LLVMPositionBuilderAtEnd(builder, after_block);
                let phi = LLVMBuildPhi(builder, LLVMTypeOf(then_value), b"merge\0".as_ptr().cast());
                LLVMAddIncoming(phi, &mut then_value, &mut then_block, 1);
                LLVMAddIncoming(phi, &mut else_value, &mut else_block, 1);
                phi
            } else {
                todo!()
                // let after_block = LLVMAppendBasicBlock(llvm_func, b"after\0".as_ptr().cast());
                // LLVMPositionBuilderAtEnd(builder, init_block);
                // LLVMBuildCondBr(builder, cond, then_block, after_block);
                // LLVMPositionBuilderAtEnd(builder, then_block);
                // LLVMBuildBr(builder, after_block);

                // ptr::null_mut()
            }
        }
        Value::Block(values) => {
            let len = locals.len();

            let values: Vec<LLVMValueRef> = values
                .iter()
                .map(|v| gen_value(ctx, builder, llvm_funcs, llvm_func, locals, v))
                .collect();

            locals.truncate(len);

            *values.last().unwrap()
        }
        Value::StoreVar(value) => {
            let value = gen_value(ctx, builder, llvm_funcs, llvm_func, locals, &*value);
            locals.push(value);
            empty_struct_value(ctx)
        }
    }
}

unsafe fn empty_struct_type(ctx: LLVMContextRef) -> LLVMTypeRef {
    let mut types: [LLVMTypeRef; 0] = [];
    LLVMStructTypeInContext(ctx, (&mut types).as_mut_ptr().cast(), 0, 0)
}

unsafe fn empty_struct_value(ctx: LLVMContextRef) -> LLVMValueRef {
    let mut values: [LLVMValueRef; 0] = [];
    LLVMConstStructInContext(ctx, (&mut values).as_mut_ptr().cast(), 0, 0)
}
