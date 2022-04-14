use std::fmt::Write;

use glide_ast::{
    expr::Expr,
    stmt::{Stmt, VarDecl},
    Ast,
};
use glide_ir::Ir;

use crate::func::Func;

use self::{
    engine::Engine,
    error::{Error, Result},
    func::{FuncBody, Insn, InstantiatedFuncId},
    namespace::Namespace,
    ty::{Ty, TyId},
    ty_constr::TyConstr,
    value::Value,
};

mod engine;
pub mod error;
mod func;
mod namespace;
mod ty;
mod ty_constr;
mod value;

pub fn compile(ast: &Ast) -> Result<Ir> {
    let mut engine = Engine::new();
    let mut global_namespace = Namespace::new();

    let builtin_ty_constrs = [
        ("Void", TyConstr::Void),
        ("Int", TyConstr::Int),
        ("String", TyConstr::String),
        ("Slice", TyConstr::Slice),
    ];
    for (name, constr) in builtin_ty_constrs {
        global_namespace
            .insert_ty_constr(name.to_owned(), engine.ty_constrs.add(constr))
            .unwrap();
    }

    let mut funcs = Vec::new();

    {
        let string = engine.tys.add(Ty::String);
        let void = engine.tys.add(Ty::Void);
        let signature = engine.tys.add(Ty::Func(vec![string], void));
        let print = engine.values.add(Value::Func(Func {
            name: "print".to_owned(),
            ty_params: Vec::new(),
            signature,
            body: FuncBody::Print,
            cur_mono: false,
        }));
        global_namespace
            .insert_value("print".to_owned(), print)
            .unwrap();
    }

    for glide_ast::def::Def::Func(glide_ast::def::Func {
        name,
        generics,
        params,
        ret,
        stmts,
    }) in &ast.defs
    {
        let name = name.data().to_owned();

        let mut func_namespace = global_namespace.sub();

        let mut ty_params = Vec::new();
        for generic in generics {
            let ty = engine.tys.add(Ty::Param);
            let ty_constr = engine.ty_constrs.add(TyConstr::Param(ty));
            func_namespace.insert_ty_constr(generic.data().to_owned(), ty_constr)?;
            ty_params.push(ty);
        }

        let mut param_tys = Vec::new();
        for (_, ty) in params {
            let ty = resolve_ty(&mut engine, &func_namespace, ty)?;
            param_tys.push(ty);
        }

        let ret = match ret {
            Some(ty) => resolve_ty(&mut engine, &func_namespace, ty)?,
            None => engine.tys.add(Ty::Void),
        };

        let signature = engine.tys.add(Ty::Func(param_tys.clone(), ret));

        let mut locals = vec![signature];

        for (&ty, (name, _)) in param_tys.iter().zip(params) {
            let idx = locals.len();
            locals.push(ty);
            let value = engine.values.add(Value::Local(idx));
            func_namespace.insert_value(name.data().to_owned(), value)?;
        }

        let func = Func {
            name: name.clone(),
            ty_params,
            signature,
            body: FuncBody::Normal(Vec::new()),
            cur_mono: false,
        };
        let func_id = engine.values.add(Value::Func(func));

        let func_namespace = func_namespace.detach();
        global_namespace.insert_value(name, func_id)?;

        funcs.push((func_id, func_namespace, locals, ret, stmts));
    }

    for (func_id, func_namespace, mut locals, ret, stmts) in funcs {
        let mut func_namespace = func_namespace.attach(&global_namespace);

        let mut insns = Vec::new();

        match &stmts[..] {
            [] => {
                if engine.tys.is_void(ret) {
                    insns.push(Insn::PushVoid);
                    locals.push(ret);
                } else {
                    return Err(Error::TyMismatch);
                }
            }
            [rest @ .., last] => {
                for stmt in rest {
                    compile_stmt(
                        &mut engine,
                        &mut func_namespace,
                        &mut locals,
                        &mut insns,
                        stmt,
                    )?;
                    insns.push(Insn::Pop);
                    locals.pop().unwrap();
                }
                let last_ty = compile_stmt(
                    &mut engine,
                    &mut func_namespace,
                    &mut locals,
                    &mut insns,
                    last,
                )?;
                match (engine.tys.is_void(ret), engine.tys.is_void(last_ty)) {
                    (true, true) => (),
                    (true, false) => {
                        insns.push(Insn::Pop);
                        locals.pop().unwrap();
                        insns.push(Insn::PushVoid);
                        locals.push(ret);
                    }
                    (false, true) => return Err(Error::TyMismatch),
                    (false, false) => engine.tys.unify(ret, last_ty)?,
                }
            }
        }

        insns.push(Insn::Ret);

        engine.values.get_mut(func_id).as_mut_func().unwrap().body = FuncBody::Normal(insns);
    }

    let main_id = global_namespace.get_value("main").ok_or(Error::NoMain)?;
    let main = engine.values.get(main_id).as_func().unwrap();
    if !main.ty_params.is_empty() {
        return Err(Error::NoMain);
    }
    let void = engine.tys.add(Ty::Void);
    let expected_main_ty = engine.tys.add(Ty::Func(Vec::new(), void));
    engine.tys.unify(expected_main_ty, main.signature)?;

    let instantiated_main = engine.instantiate_func(main_id);

    let mut ir = Ir::new();

    let main_id = monomorphize(&mut engine, &mut ir, instantiated_main)?;
    ir.main_func = main_id;

    Ok(ir)
}

fn resolve_ty(
    engine: &mut Engine,
    namespace: &Namespace<'_>,
    ty: &glide_ast::ty::Ty<'_>,
) -> Result<TyId> {
    let ty_constr = namespace
        .get_ty_constr(ty.name.data())
        .ok_or(Error::UnresolvedName)?;
    let mut ty_args = Vec::new();
    for generic in &ty.generics {
        ty_args.push(resolve_ty(engine, namespace, generic)?);
    }
    engine.instantiate_ty(ty_constr, ty_args)
}

fn compile_stmt(
    engine: &mut Engine,
    namespace: &mut Namespace<'_>,
    locals: &mut Vec<TyId>,
    insns: &mut Vec<Insn>,
    stmt: &Stmt<'_>,
) -> Result<TyId> {
    match stmt {
        Stmt::Var(VarDecl { name, ty, value }) => {
            let value_ty = compile_expr(engine, namespace, locals, insns, value)?;
            if let Some(ty) = ty {
                let ty = resolve_ty(engine, namespace, ty)?;
                engine.tys.unify(ty, value_ty)?;
            }
            let idx = locals.len() - 1;
            let value_id = engine.values.add(Value::Local(idx));
            namespace.insert_value(name.data().to_owned(), value_id)?;
            let void = engine.tys.add(Ty::Void);
            locals.push(void);
            insns.push(Insn::PushVoid);
            Ok(void)
        }
        Stmt::Expr(expr) => compile_expr(engine, namespace, locals, insns, expr),
    }
}

fn compile_expr(
    engine: &mut Engine,
    namespace: &Namespace<'_>,
    locals: &mut Vec<TyId>,
    insns: &mut Vec<Insn>,
    expr: &Expr<'_>,
) -> Result<TyId> {
    match expr {
        Expr::Integer(span) => {
            let value: isize = span.data().parse().map_err(|_| Error::IntegerOverflow)?;
            let ty = engine.tys.add(Ty::Int);
            insns.push(Insn::PushInt(value));
            locals.push(ty);
            Ok(ty)
        }
        Expr::String { data } => {
            let ty = engine.tys.add(Ty::String);
            insns.push(Insn::PushString(data.clone()));
            locals.push(ty);
            Ok(ty)
        }
        Expr::Var(name) => {
            let value_id = namespace
                .get_value(name.data())
                .ok_or(Error::UnresolvedName)?;
            let value = engine.values.get(value_id);
            match value {
                Value::Local(idx) => {
                    let idx = *idx;
                    insns.push(Insn::PushLocal(idx));
                    let ty = locals[idx];
                    locals.push(ty);
                    Ok(ty)
                }
                Value::Func(_) => {
                    let instantiated_func = engine.instantiate_func(value_id);
                    let signature = engine.instantiated_funcs.get(instantiated_func).signature;
                    insns.push(Insn::PushFunc(instantiated_func));
                    locals.push(signature);
                    Ok(signature)
                }
            }
        }
        Expr::Call(glide_ast::expr::Call { receiver, args }) => {
            let call_idx = locals.len();
            let receiver_ty = compile_expr(engine, namespace, locals, insns, receiver)?;
            let mut arg_tys = Vec::new();
            for arg in args {
                arg_tys.push(compile_expr(engine, namespace, locals, insns, arg)?);
            }
            let ret_ty = engine.tys.add(Ty::Infer);
            let expect_ty = engine.tys.add(Ty::Func(arg_tys, ret_ty));
            engine.tys.unify(expect_ty, receiver_ty)?;
            insns.push(Insn::Call {
                at: call_idx,
                ret: ret_ty,
            });
            locals.truncate(call_idx);
            locals.push(ret_ty);
            Ok(ret_ty)
        }
    }
}

fn monomorphize(
    engine: &mut Engine,
    ir: &mut Ir,
    instantiated_func_id: InstantiatedFuncId,
) -> Result<glide_ir::FuncId> {
    let instantiated_func = engine.instantiated_funcs.get(instantiated_func_id);
    if let Some(idx) = instantiated_func.comp_idx {
        return Ok(idx);
    }

    let signature = instantiated_func.signature;
    let func_id = instantiated_func.func;

    let mut name = engine
        .values
        .get(instantiated_func.func)
        .as_func()
        .unwrap()
        .name
        .clone();
    if let Some((last, rest)) = instantiated_func.ty_args.split_last() {
        write!(name, "<").unwrap();
        for &ty_arg in rest {
            let ty_arg = compile_ty(engine, &mut ir.tys, ty_arg)?;
            write!(name, "{}, ", ir.tys.display(ty_arg)).unwrap();
        }
        let ty_arg = compile_ty(engine, &mut ir.tys, *last)?;
        write!(name, "{}>", ir.tys.display(ty_arg)).unwrap();
    }

    let ir_signature = compile_ty(engine, &mut ir.tys, signature)?;
    let ir_func_id = ir.funcs.add(glide_ir::Func {
        name,
        signature: ir_signature,
        data: glide_ir::FuncData::Normal(Vec::new()),
    });

    engine
        .instantiated_funcs
        .get_mut(instantiated_func_id)
        .comp_idx = Some(ir_func_id);

    let func = engine.values.get_mut(func_id).as_mut_func().unwrap();
    if func.cur_mono {
        return Err(Error::TyRecursion);
    }
    func.cur_mono = true;

    let body = match func.body.clone() {
        FuncBody::Normal(insns) => {
            // let mut locals = Vec::new();
            // let (params, ret) = match engine.tys.get(signature) {
            //     Ty::Func(params, ret) => (params, ret),
            //     _ => unreachable!(),
            // };
            // for &param in params {
            //     locals.push(param);
            // }

            let mut ir_insns = Vec::new();
            for insn in insns {
                match &insn {
                    Insn::PushVoid => ir_insns.push(glide_ir::Insn::PushVoid),
                    Insn::PushInt(value) => ir_insns.push(glide_ir::Insn::PushInt(*value)),
                    Insn::PushString(string) => {
                        ir_insns.push(glide_ir::Insn::PushString(string.clone()))
                    }
                    Insn::PushLocal(idx) => ir_insns.push(glide_ir::Insn::PushLocal(*idx)),
                    Insn::PushFunc(func) => {
                        let id = monomorphize(engine, ir, *func)?;
                        ir_insns.push(glide_ir::Insn::PushFunc(id));
                    }
                    Insn::Pop => ir_insns.push(glide_ir::Insn::Pop),
                    Insn::Call { at, ret } => {
                        let ret = compile_ty(engine, &mut ir.tys, *ret)?;
                        ir_insns.push(glide_ir::Insn::Call { at: *at, ret })
                    }
                    Insn::Ret => ir_insns.push(glide_ir::Insn::Ret),
                }
            }

            glide_ir::FuncData::Normal(ir_insns)
        }
        FuncBody::Print => glide_ir::FuncData::Print,
    };
    ir.funcs.get_mut(ir_func_id).data = body;

    let func = engine.values.get_mut(func_id).as_mut_func().unwrap();
    func.cur_mono = false;

    Ok(ir_func_id)
}

fn compile_ty(engine: &Engine, tys: &mut glide_ir::Tys, ty: TyId) -> Result<glide_ir::TyId> {
    match engine.tys.get(ty) {
        Ty::Void => Ok(tys.add(glide_ir::Ty::Void)),
        Ty::Int => Ok(tys.add(glide_ir::Ty::Int)),
        Ty::String => Ok(tys.add(glide_ir::Ty::String)),
        Ty::Slice(elem) => {
            let elem = compile_ty(engine, tys, *elem)?;
            Ok(tys.add(glide_ir::Ty::Slice(elem)))
        }
        Ty::Func(params, ret) => {
            let mut ir_params = Vec::new();
            for &param in params {
                ir_params.push(compile_ty(engine, tys, param)?);
            }
            let ret = compile_ty(engine, tys, *ret)?;
            Ok(tys.add(glide_ir::Ty::Func(ir_params, ret)))
        }
        Ty::Param => unreachable!(),
        Ty::Infer => Err(Error::CannotInfer),
        Ty::Equal(ty) => compile_ty(engine, tys, *ty),
    }
}
