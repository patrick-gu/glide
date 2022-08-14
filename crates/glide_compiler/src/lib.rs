use std::{fmt::Write, mem};

use glide_ast::{
    def::Def,
    expr::{Block, Call, Expr, If},
    stmt::{Stmt, VarDecl},
    Ast,
};
use glide_ir::Ir;

use self::{
    engine::Engine,
    error::{Error, Result},
    func::{Func, FuncBody, FuncCompiled, FuncUsage, FuncsCompiled},
    namespace::Namespace,
    ty::{Ty, TyId},
    ty_constr::TyConstr,
    value::{Value, ValueRef},
};

mod engine;
pub mod error;
mod func;
mod namespace;
mod ty;
mod ty_constr;
mod value;

pub fn compile(ast: &Ast<'_>) -> Result<Ir> {
    let mut engine = Engine::new();
    let mut global_namespace = Namespace::new();

    {
        global_namespace
            .insert_ty_constr("Void".to_owned(), engine.ty_constrs.add(TyConstr::Void))
            .unwrap();
        global_namespace
            .insert_ty_constr("Int".to_owned(), engine.ty_constrs.add(TyConstr::Int))
            .unwrap();
        global_namespace
            .insert_ty_constr("Bool".to_owned(), engine.ty_constrs.add(TyConstr::Bool))
            .unwrap();
        global_namespace
            .insert_ty_constr("String".to_owned(), engine.ty_constrs.add(TyConstr::String))
            .unwrap();
        global_namespace
            .insert_ty_constr("Slice".to_owned(), engine.ty_constrs.add(TyConstr::Slice))
            .unwrap();
    }

    macro_rules! builtin_func {
        ($upname:ident $name:ident ($($param:ident),*) $ret:ident) => {{
            let params = vec![$(TyId::$param),*];
            let signature = engine.tys.add(Ty::Func(params, TyId::$ret));
            let id = engine.funcs.add(Func {
                name: stringify!($name).to_owned(),
                ty_params: Vec::new(),
                signature,
                body: FuncBody::$upname,
                cur_mono: false,
            });
            global_namespace
                .insert_value(stringify!($name).to_owned(), ValueRef::Func(id))
                .unwrap();
        }};
    }
    builtin_func!(Print print(STRING) VOID);
    builtin_func!(PrintInt printInt(INT) VOID);
    builtin_func!(Add add(INT, INT) INT);
    builtin_func!(Sub sub(INT, INT) INT);
    builtin_func!(EqInt eqInt(INT, INT) BOOL);

    let mut funcs = Vec::new();

    // Register names in the global namespace first.
    for def in &ast.defs {
        let Def::Func(glide_ast::def::Func {
            name,
            generics,
            params,
            ret,
            block,
        }) = def;

        let func_id = engine.funcs.add(Func {
            name: name.data().to_owned(),
            ty_params: Vec::new(),
            signature: TyId::PLACEHOLDER,
            body: FuncBody::Placeholder,
            cur_mono: false,
        });
        global_namespace.insert_value(name.data().to_owned(), ValueRef::Func(func_id))?;

        funcs.push((func_id, generics, params, ret, block));
    }

    let mut funcs_2 = Vec::new();

    // Resolve and compile each function semantically.
    for (func_id, generics, params, ret, block) in funcs {
        let mut func_namespace = global_namespace.sub();
        let func = engine.funcs.get_mut(func_id);

        let mut ty_params = Vec::new();
        for generic in generics {
            let ty = engine.tys.add(Ty::Param);
            let ty_constr = engine.ty_constrs.add(TyConstr::Param(ty));
            func_namespace.insert_ty_constr(generic.data().to_owned(), ty_constr)?;
            ty_params.push(ty);
        }
        func.ty_params = ty_params;

        let param_tys: Vec<TyId> = params
            .iter()
            .enumerate()
            .map(|(idx, (name, ty))| {
                let ty = resolve_ty(&mut engine, &func_namespace, ty)?;
                func_namespace.insert_value(name.data().to_owned(), ValueRef::Local(idx))?;
                Ok(ty)
            })
            .collect::<Result<Vec<TyId>>>()?;

        let ret_ty = match ret {
            Some(ty) => resolve_ty(&mut engine, &func_namespace, ty)?,
            None => engine.tys.add(Ty::Void),
        };

        let signature = engine.tys.add(Ty::Func(param_tys.clone(), ret_ty));
        let func = engine.funcs.get_mut(func_id);
        func.signature = signature;

        funcs_2.push((block, func_namespace, param_tys, ret_ty, func_id));
    }

    for (
        block,
        func_namespace,
        // Arguments become locals
        mut locals,
        ret_ty,
        func_id,
    ) in funcs_2
    {
        let (value, found_ty) =
            compile_block(&mut engine, &func_namespace, &mut locals, ret_ty, block)?;
        let value = match (engine.tys.is_void(ret_ty), engine.tys.is_void(found_ty)) {
            (true, true | false) => {
                // If the last statement was not void, do not try to make it, as it may cause
                // incorrent inferrence.
                Value::RetVoid(Box::new(value))
            }
            (false, true) => {
                return Err(Error::TyMismatch);
            }
            (false, false) => {
                engine.tys.unify(ret_ty, found_ty)?;
                Value::Ret(Box::new(value))
            }
        };
        let func = engine.funcs.get_mut(func_id);
        func.body = FuncBody::Normal(value);
    }

    // Monomorphize functions beginning with main.
    let mut compiled = FuncsCompiled::new();
    let mut ir_funcs = glide_ir::Funcs::new();

    let main_id = match global_namespace.get_value("main") {
        Some(ValueRef::Func(func)) => func,
        _ => return Err(Error::NoMain),
    };
    let main = engine.funcs.get(main_id);
    if !main.ty_params.is_empty() {
        return Err(Error::NoMain);
    }
    let void = engine.tys.add(Ty::Void);
    let expected_main_ty = engine.tys.add(Ty::Func(Vec::new(), void));
    engine.tys.unify(expected_main_ty, main.signature)?;

    let main_usage = engine.use_func(main_id);

    let main_id = monomorphize_func(&mut engine, &mut ir_funcs, &mut compiled, &main_usage)?;

    Ok(Ir {
        funcs: ir_funcs,
        main_func: main_id,
    })
}

fn resolve_ty(
    engine: &mut Engine,
    namespace: &Namespace<'_>,
    glide_ast::ty::Ty { name, generics }: &glide_ast::ty::Ty<'_>,
) -> Result<TyId> {
    let ty_constr = namespace
        .get_ty_constr(name.data())
        .ok_or(Error::UnresolvedName)?;
    let mut ty_args = Vec::new();
    for generic in generics {
        ty_args.push(resolve_ty(engine, namespace, generic)?);
    }
    engine.instantiate_ty(ty_constr, ty_args)
}

fn compile_ty(engine: &Engine, ty: TyId) -> Result<glide_ir::Ty> {
    match engine.tys.get(ty) {
        Ty::Void => Ok(glide_ir::Ty::Void),
        Ty::Int => Ok(glide_ir::Ty::Int),
        Ty::Bool => Ok(glide_ir::Ty::Bool),
        Ty::String => Ok(glide_ir::Ty::String),
        Ty::Slice(elem) => Ok(glide_ir::Ty::Slice(Box::new(compile_ty(engine, *elem)?))),
        Ty::Func(params, ret) => {
            let params = params
                .iter()
                .map(|&param| compile_ty(engine, param))
                .collect::<Result<Vec<glide_ir::Ty>>>()?;
            let ret = compile_ty(engine, *ret)?;
            Ok(glide_ir::Ty::Func(params, Box::new(ret)))
        }
        Ty::Param => panic!(),
        Ty::Infer => Err(Error::CannotInfer),
        Ty::Equal(ty) => compile_ty(engine, *ty),
    }
}

fn compile_block(
    engine: &mut Engine,
    namespace: &Namespace<'_>,
    locals: &mut Vec<TyId>,
    return_ty: TyId,
    block: &Block<'_>,
) -> Result<(Value, TyId)> {
    let mut namespace = namespace.sub();
    let previous_locals_len = locals.len();

    let (values, ty) = match &block.stmts[..] {
        [] => (vec![Value::Ret(Box::new(Value::Void))], TyId::VOID),
        [rest @ .., last] => {
            let mut values = Vec::new();
            for stmt in rest {
                let (value, ty) = compile_stmt(engine, &mut namespace, locals, return_ty, stmt)?;
                values.push(value);
            }
            let (last_value, last_ty) =
                compile_stmt(engine, &mut namespace, locals, return_ty, last)?;
            values.push(last_value);
            (values, last_ty)
        }
    };

    locals.truncate(previous_locals_len);

    let value = Value::Block(values);
    Ok((value, ty))
}

fn compile_stmt(
    engine: &mut Engine,
    namespace: &mut Namespace<'_>,
    locals: &mut Vec<TyId>,
    return_ty: TyId,
    stmt: &Stmt<'_>,
) -> Result<(Value, TyId)> {
    match stmt {
        Stmt::Var(VarDecl { name, ty, value }) => {
            let (value, found_ty) = compile_expr(engine, namespace, locals, return_ty, value)?;
            if let Some(expect_ty) = ty {
                let expect_ty = resolve_ty(engine, namespace, expect_ty)?;
                engine.tys.unify(expect_ty, found_ty)?;
            }
            let idx = locals.len();
            locals.push(found_ty);
            namespace.insert_value(name.data().to_owned(), ValueRef::Local(idx))?;
            Ok((Value::StoreVar(Box::new(value)), TyId::VOID))
        }
        Stmt::Expr(expr) => compile_expr(engine, namespace, locals, return_ty, expr),
        Stmt::Return(expr) => {
            let (value, ty) = compile_expr(engine, namespace, locals, return_ty, expr)?;
            engine.tys.unify(return_ty, ty)?;
            Ok((Value::Ret(Box::new(value)), TyId::VOID))
        }
    }
}

fn compile_expr(
    engine: &mut Engine,
    namespace: &Namespace<'_>,
    locals: &mut Vec<TyId>,
    return_ty: TyId,
    expr: &Expr<'_>,
) -> Result<(Value, TyId)> {
    match expr {
        Expr::Integer(span) => {
            let value: i64 = span.data().parse().map_err(|_| Error::IntegerOverflow)?;
            Ok((Value::ConstantInt(value), engine.tys.add(Ty::Int)))
        }
        Expr::String { data } => Ok((
            Value::ConstantString(data.clone()),
            engine.tys.add(Ty::String),
        )),
        Expr::Var(name) => {
            let v = namespace
                .get_value(name.data())
                .ok_or(Error::UnresolvedName)?;
            match v {
                ValueRef::Local(idx) => Ok((Value::Local(idx), locals[idx])),
                ValueRef::Func(func) => {
                    let usage = engine.use_func(func);
                    let signature = usage.signature;
                    Ok((Value::Func(usage), signature))
                }
            }
        }
        Expr::Call(Call { receiver, args }) => {
            let (receiver_value, receiver_ty) =
                compile_expr(engine, namespace, locals, return_ty, receiver)?;
            let mut arg_values = Vec::new();
            let mut arg_tys = Vec::new();
            for arg in args {
                let (value, ty) = compile_expr(engine, namespace, locals, return_ty, arg)?;
                arg_values.push(value);
                arg_tys.push(ty);
            }
            let ret_ty = engine.tys.add(Ty::Infer);
            let expect_receiver_ty = engine.tys.add(Ty::Func(arg_tys, ret_ty));
            engine.tys.unify(expect_receiver_ty, receiver_ty)?;
            Ok((Value::Call(Box::new(receiver_value), arg_values), ret_ty))
        }
        Expr::If(If { branches, els }) => {
            let mut base = None;
            let mut cur_else = &mut base;

            let mut tys = Vec::new();

            for (cond, block) in branches {
                let (cond_value, cond_ty) =
                    compile_expr(engine, namespace, locals, return_ty, cond)?;
                engine.tys.unify(cond_ty, TyId::BOOL)?;
                let (block_value, ty) = compile_block(engine, namespace, locals, return_ty, block)?;
                tys.push(ty);
                *cur_else = Some(Box::new(Value::If {
                    cond: Box::new(cond_value),
                    then: Box::new(block_value),
                    els: None,
                }));
                cur_else = match &mut **cur_else.as_mut().unwrap() {
                    glide_ir::Value::If { cond, then, els } => els,
                    _ => unreachable!(),
                };
            }

            match els {
                Some(block) => {
                    let (value, ty) = compile_block(engine, namespace, locals, return_ty, block)?;
                    tys.push(ty);
                    *cur_else = Some(Box::new(value));
                }
                None => todo!(),
            }

            for s in tys.windows(2) {
                if let [l, r] = s {
                    engine.tys.unify(*l, *r)?;
                } else {
                    unreachable!();
                }
            }

            Ok((*base.unwrap(), tys[0]))
        }
        Expr::True => Ok((Value::True, TyId::BOOL)),
        Expr::False => Ok((Value::False, TyId::BOOL)),
    }
}

fn monomorphize_func(
    engine: &mut Engine,
    ir_funcs: &mut glide_ir::Funcs,
    funcs_compiled: &mut FuncsCompiled,
    usage: &FuncUsage,
) -> Result<glide_ir::FuncId> {
    let compiled = {
        let mut ty_args = Vec::new();
        for &ty_arg in &usage.ty_args {
            ty_args.push(compile_ty(engine, ty_arg)?);
        }
        FuncCompiled {
            func: usage.func,
            ty_args,
        }
    };

    if let Some(id) = funcs_compiled.get(&compiled) {
        return Ok(id);
    }

    if engine.funcs.get(usage.func).cur_mono {
        return Err(Error::TyRecursion);
    }
    engine.funcs.get_mut(usage.func).cur_mono = true;

    let mut name = engine.funcs.get(usage.func).name.clone();
    if let [rest @ .., last] = &compiled.ty_args[..] {
        write!(name, "<").unwrap();
        for ty_arg in rest {
            write!(name, "{}, ", ty_arg).unwrap();
        }
        write!(name, "{}>", last).unwrap();
    }

    let signature = compile_ty(engine, usage.signature)?;

    let id = ir_funcs.add(glide_ir::Func {
        name,
        signature,
        body: glide_ir::FuncBody::Placeholder,
    });
    funcs_compiled.insert(compiled, id);

    let body = engine.funcs.get(usage.func).body.clone();
    let body = match body {
        FuncBody::Placeholder => unreachable!(),
        FuncBody::Normal(value) => {
            glide_ir::FuncBody::Normal(lower_value(engine, ir_funcs, funcs_compiled, value)?)
        }
        FuncBody::Print => glide_ir::FuncBody::Print,
        FuncBody::PrintInt => glide_ir::FuncBody::PrintInt,
        FuncBody::Add => glide_ir::FuncBody::Add,
        FuncBody::Sub => glide_ir::FuncBody::Sub,
        FuncBody::EqInt => glide_ir::FuncBody::EqInt,
    };

    ir_funcs.get_mut(id).body = body;

    engine.funcs.get_mut(usage.func).cur_mono = false;

    Ok(id)
}

fn lower_value(
    engine: &mut Engine,
    ir_funcs: &mut glide_ir::Funcs,
    funcs_compiled: &mut FuncsCompiled,
    value: Value,
) -> Result<glide_ir::Value> {
    Ok(match value {
        Value::Void => glide_ir::Value::Void,
        Value::True => glide_ir::Value::True,
        Value::False => glide_ir::Value::False,
        Value::ConstantInt(value) => glide_ir::Value::ConstantInt(value),
        Value::ConstantString(data) => glide_ir::Value::ConstantString(data),
        Value::Local(idx) => glide_ir::Value::Local(idx),
        Value::Func(usage) => {
            let id = monomorphize_func(engine, ir_funcs, funcs_compiled, &usage)?;
            glide_ir::Value::Func(id)
        }
        Value::Call(receiver, args) => {
            let receiver = lower_value(engine, ir_funcs, funcs_compiled, *receiver)?;
            let args = args
                .into_iter()
                .map(|v| lower_value(engine, ir_funcs, funcs_compiled, v))
                .collect::<Result<Vec<glide_ir::Value>>>()?;
            glide_ir::Value::Call(Box::new(receiver), args)
        }
        Value::Ret(value) => glide_ir::Value::Ret(Box::new(lower_value(
            engine,
            ir_funcs,
            funcs_compiled,
            *value,
        )?)),
        Value::RetVoid(value) => glide_ir::Value::RetVoid(Box::new(lower_value(
            engine,
            ir_funcs,
            funcs_compiled,
            *value,
        )?)),
        Value::If { cond, then, els } => {
            let cond = lower_value(engine, ir_funcs, funcs_compiled, *cond)?;
            let then = lower_value(engine, ir_funcs, funcs_compiled, *then)?;
            let els = els
                .map(|els| lower_value(engine, ir_funcs, funcs_compiled, *els))
                .transpose()?
                .map(Box::new);

            glide_ir::Value::If {
                cond: Box::new(cond),
                then: Box::new(then),
                els,
            }
        }
        Value::Block(values) => glide_ir::Value::Block(
            values
                .into_iter()
                .map(|v| lower_value(engine, ir_funcs, funcs_compiled, v))
                .collect::<Result<Vec<glide_ir::Value>>>()?,
        ),
        Value::StoreVar(value) => glide_ir::Value::StoreVar(Box::new(lower_value(
            engine,
            ir_funcs,
            funcs_compiled,
            *value,
        )?)),
    })
}
