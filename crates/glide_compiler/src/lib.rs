use std::{fmt::Write, mem};

use glide_ast::{
    def::Def,
    expr::{Call, Expr, If},
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
    {
        let params = vec![engine.tys.add(Ty::String)];
        let ret = engine.tys.add(Ty::Void);
        let signature = engine.tys.add(Ty::Func(params, ret));
        let id = engine.funcs.add(Func {
            name: "print".to_owned(),
            ty_params: Vec::new(),
            signature,
            body: FuncBody::Print,
            cur_mono: false,
        });
        global_namespace
            .insert_value("print".to_owned(), ValueRef::Func(id))
            .unwrap();
    }
    {
        let params = vec![engine.tys.add(Ty::Int)];
        let ret = engine.tys.add(Ty::Void);
        let signature = engine.tys.add(Ty::Func(params, ret));
        let id = engine.funcs.add(Func {
            name: "printInt".to_owned(),
            ty_params: Vec::new(),
            signature,
            body: FuncBody::PrintInt,
            cur_mono: false,
        });
        global_namespace
            .insert_value("printInt".to_owned(), ValueRef::Func(id))
            .unwrap();
    }
    {
        let int = engine.tys.add(Ty::Int);
        let params = vec![int, int];
        let ret = int;
        let signature = engine.tys.add(Ty::Func(params, ret));
        let id = engine.funcs.add(Func {
            name: "add".to_owned(),
            ty_params: Vec::new(),
            signature,
            body: FuncBody::Add,
            cur_mono: false,
        });
        global_namespace
            .insert_value("add".to_owned(), ValueRef::Func(id))
            .unwrap();
    }
    {
        let int = engine.tys.add(Ty::Int);
        let params = vec![int, int];
        let ret = int;
        let signature = engine.tys.add(Ty::Func(params, ret));
        let id = engine.funcs.add(Func {
            name: "sub".to_owned(),
            ty_params: Vec::new(),
            signature,
            body: FuncBody::Sub,
            cur_mono: false,
        });
        global_namespace
            .insert_value("sub".to_owned(), ValueRef::Func(id))
            .unwrap();
    }
    {
        let int = engine.tys.add(Ty::Int);
        let params = vec![int, int];
        let ret = engine.tys.add(Ty::Bool);
        let signature = engine.tys.add(Ty::Func(params, ret));
        let id = engine.funcs.add(Func {
            name: "eqInt".to_owned(),
            ty_params: Vec::new(),
            signature,
            body: FuncBody::EqInt,
            cur_mono: false,
        });
        global_namespace
            .insert_value("eqInt".to_owned(), ValueRef::Func(id))
            .unwrap();
    }

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
            body: FuncBody::Normal(Vec::new()),
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
                func_namespace.insert_value(name.data().to_owned(), ValueRef::Param(idx))?;
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

    for (block, mut func_namespace, param_tys, ret_ty, func_id) in funcs_2 {
        let mut values = Vec::new();

        match &block.stmts[..] {
            [] => {
                let void = engine.tys.add(Ty::Void);
                engine.tys.unify(ret_ty, void)?;
                values.push((Value::Ret(Box::new(Value::Void)), engine.tys.add(Ty::Void)));
            }
            [rest @ .., last] => {
                for stmt in rest {
                    compile_stmt(
                        &mut engine,
                        &mut func_namespace,
                        &param_tys,
                        &mut values,
                        stmt,
                    )?;
                }
                let last_idx = compile_stmt(
                    &mut engine,
                    &mut func_namespace,
                    &param_tys,
                    &mut values,
                    last,
                )?;
                let (last_value, last_ty) = &mut values[last_idx];
                match (engine.tys.is_void(ret_ty), engine.tys.is_void(*last_ty)) {
                    (true, true | false) => {
                        // If the last statement was not void, do not try to make it, as it may cause
                        // incorrent inferrence.
                        values.push((Value::Ret(Box::new(Value::Void)), engine.tys.add(Ty::Void)));
                    }
                    (false, true) => {
                        return Err(Error::TyMismatch);
                    }
                    (false, false) => {
                        engine.tys.unify(ret_ty, *last_ty)?;
                        let value = mem::replace(last_value, Value::Void); // placeholder
                        *last_value = Value::Ret(Box::new(value));
                    }
                }
            }
        }

        let func = engine.funcs.get_mut(func_id);
        func.body = FuncBody::Normal(values.into_iter().map(|(value, _)| value).collect());
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

fn compile_stmt(
    engine: &mut Engine,
    namespace: &mut Namespace<'_>,
    params: &[TyId],
    values: &mut Vec<(Value, TyId)>,
    stmt: &Stmt<'_>,
) -> Result<usize> {
    let idx = values.len();
    let (value, ty) = match stmt {
        Stmt::Var(VarDecl { name, ty, value }) => {
            let (value, found_ty) = compile_expr(engine, namespace, params, values, value)?;
            if let Some(expect_ty) = ty {
                let expect_ty = resolve_ty(engine, namespace, expect_ty)?;
                engine.tys.unify(expect_ty, found_ty)?;
            }
            namespace.insert_value(name.data().to_owned(), ValueRef::Local(idx))?;
            (value, found_ty)
        }
        Stmt::Expr(expr) => compile_expr(engine, namespace, params, values, expr)?,
    };
    values.push((value, ty));
    Ok(idx)
}

fn compile_expr(
    engine: &mut Engine,
    namespace: &Namespace<'_>,
    params: &[TyId],
    values: &[(Value, TyId)],
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
                ValueRef::Param(idx) => Ok((Value::Param(idx), params[idx])),
                ValueRef::Local(idx) => Ok((Value::Local(idx), values[idx].1)),
                ValueRef::Func(func) => {
                    let usage = engine.use_func(func);
                    let signature = usage.signature;
                    Ok((Value::Func(usage), signature))
                }
            }
        }
        Expr::Call(Call { receiver, args }) => {
            let (receiver_value, receiver_ty) =
                compile_expr(engine, namespace, params, values, receiver)?;
            let mut arg_values = Vec::new();
            let mut arg_tys = Vec::new();
            for arg in args {
                let (value, ty) = compile_expr(engine, namespace, params, values, arg)?;
                arg_values.push(value);
                arg_tys.push(ty);
            }
            let ret_ty = engine.tys.add(Ty::Infer);
            let expect_receiver_ty = engine.tys.add(Ty::Func(arg_tys, ret_ty));
            engine.tys.unify(expect_receiver_ty, receiver_ty)?;
            Ok((Value::Call(Box::new(receiver_value), arg_values), ret_ty))
        }
        Expr::If(If { branches, els }) => {
            let bool = engine.tys.add(Ty::Bool);

            // if statement:
            //    if has an else
            //       evaluate to Void and discard values
            //    otherwise
            //       evaluate to Void or unify?
            // if expression:
            //    if has an else
            //       error
            //    else
            //       unify

            if let Some(els) = els {
                let (first_cond, first_value) = &branches[0];
                let (first_cond_value, first_cond_ty) =
                    compile_expr(engine, namespace, params, values, first_cond)?;
                // let (first_value, first_value_ty) =
                //     compile_expr(engine, namespace, params, values, first_value);

                todo!()
                // for (cond, value) in branches {
                //     let (cond_value, cond_ty) =
                //         compile_expr(engine, namespace, params, values, cond)?;
                //     engine.tys.unify(cond_ty, bool)?;

                //     // let (value, value_ty) = compile_expr(engine, namespace, params, values, value);
                // }
            } else {
                todo!()
            }
        }
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
        body: glide_ir::FuncBody::Normal(Vec::new()),
    });
    funcs_compiled.insert(compiled, id);

    let body = engine.funcs.get(usage.func).body.clone();
    let body = match body {
        FuncBody::Normal(values) => {
            let ir_values = values
                .into_iter()
                .map(|v| lower_value(engine, ir_funcs, funcs_compiled, v))
                .collect::<Result<Vec<glide_ir::Value>>>()?;
            glide_ir::FuncBody::Normal(ir_values)
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
        Value::ConstantInt(value) => glide_ir::Value::ConstantInt(value),
        Value::ConstantString(data) => glide_ir::Value::ConstantString(data),
        Value::Local(idx) => glide_ir::Value::Local(idx),
        Value::Param(idx) => glide_ir::Value::Param(idx),
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
    })
}
