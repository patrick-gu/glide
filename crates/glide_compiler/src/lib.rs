use std::{
    collections::{hash_map, HashMap},
    fmt::Write,
};

use glide_ast::{
    def::{Def, DefKind, Field},
    expr::{Block, Call, Expr, If},
    path::Path,
    stmt::{Stmt, VarDecl},
};
use glide_ir::Ir;

use crate::package::Packages;

use self::{
    attribute::Attribute,
    engine::Engine,
    error::{Error, Result},
    func::{Func, FuncBody, FuncCompiled, FuncUsage, FuncsCompiled},
    namespace::{Namespace, NamespaceEntry, NamespaceEntryKind},
    package::{Package, PackageId},
    struc::Struct,
    ty::{Ty, TyId},
    value::Value,
    visibility::Visibility,
};

mod attribute;
mod engine;
pub mod error;
mod func;
mod namespace;
mod package;
mod registry;
mod struc;
mod ty;
mod value;
mod visibility;

pub struct Compilation {
    engine: Engine,
    packages: Packages,
    packages_lookup: HashMap<String, PackageId>,
    prelude_package: PackageId,
    runtime_package: PackageId,
}

pub struct StandardAsts<'a> {
    pub core: glide_ast::Package<'a>,
    pub prelude: glide_ast::Package<'a>,
    pub runtime: glide_ast::Package<'a>,
}

impl Compilation {
    pub fn new(
        StandardAsts {
            core,
            prelude,
            runtime,
        }: &StandardAsts,
    ) -> Self {
        let engine = Engine::new();
        let packages = Packages::new();
        let packages_lookup = HashMap::new();
        let mut compilation = Self {
            engine,
            packages,
            packages_lookup,
            prelude_package: PackageId::PLACEHOLDER,
            runtime_package: PackageId::PLACEHOLDER,
        };
        compilation.compile_core(core);

        let prelude_package = compilation
            .compile_package_with_namespace(prelude, Namespace::new())
            .unwrap();
        compilation.prelude_package = compilation
            .add_package("prelude".to_owned(), prelude_package)
            .unwrap();

        compilation.runtime_package = compilation
            .compile_add_package("runtime".to_owned(), runtime)
            .unwrap();

        compilation
    }

    pub fn compile_core(&mut self, package: &glide_ast::Package<'_>) {
        let mut package_namespace = Namespace::new();

        let mut funcs = Vec::new();
        let mut attribute_defs = Vec::new();

        for Def {
            attributes,
            visibility,
            kind,
        } in &package.defs
        {
            let visibility = match visibility {
                glide_ast::def::Visibility::Pub => Visibility::Public,
                glide_ast::def::Visibility::Priv => todo!("priv keyword"),
                glide_ast::def::Visibility::Unspecified => Visibility::Private,
            };
            match kind {
                DefKind::Func(glide_ast::def::Func {
                    name,
                    generics,
                    params,
                    ret,
                    block,
                }) => {
                    let body = match name.data() {
                        "println" => FuncBody::Println,
                        "printlnInt" => FuncBody::PrintlnInt,
                        "add" => FuncBody::Add,
                        "sub" => FuncBody::Sub,
                        "eqInt" => FuncBody::EqInt,
                        _ => panic!(),
                    };

                    let func_id = self.engine.funcs.add(Func {
                        name: name.data().to_owned(),
                        ty_params: Vec::new(),
                        signature: TyId::PLACEHOLDER,
                        body,
                        cur_mono: false,
                    });

                    let entry = NamespaceEntry {
                        visibility,
                        kind: NamespaceEntryKind::Func(func_id),
                    };
                    package_namespace
                        .insert(name.data().to_owned(), entry)
                        .unwrap();

                    assert!(block.stmts.is_empty());

                    funcs.push((func_id, attributes, generics, params, ret));
                }
                DefKind::Attribute(glide_ast::def::AttributeDef { name }) => {
                    let attribute = Attribute {
                        name: name.data().to_owned(),
                    };
                    let id = self.engine.attributes.add(attribute);
                    let entry = NamespaceEntry {
                        visibility,
                        kind: NamespaceEntryKind::Attribute(id),
                    };
                    package_namespace
                        .insert(name.data().to_owned(), entry)
                        .unwrap();

                    attribute_defs.push((attributes,));
                }
                DefKind::Struct(glide_ast::def::Struct {
                    name,
                    generics,
                    fields,
                }) => {
                    let kind = match name.data() {
                        "Void" => {
                            assert!(generics.is_empty());
                            NamespaceEntryKind::Void
                        }
                        "Bool" => {
                            assert!(generics.is_empty());
                            NamespaceEntryKind::Bool
                        }
                        "Int" => {
                            assert!(generics.is_empty());
                            NamespaceEntryKind::Int
                        }
                        "String" => {
                            assert!(generics.is_empty());
                            NamespaceEntryKind::String
                        }
                        "Slice" => {
                            assert_eq!(generics.len(), 1);
                            assert_eq!(generics[0].data(), "T");
                            NamespaceEntryKind::Slice
                        }
                        _ => panic!(),
                    };
                    package_namespace
                        .insert(name.data().to_owned(), NamespaceEntry { visibility, kind })
                        .unwrap();
                    assert!(fields.is_empty());
                }
                DefKind::Use(_) => panic!(),
            }
        }

        for (attributes,) in attribute_defs {
            for glide_ast::def::AttributeUsage { path } in attributes {
                let _ = self.resolve_path(&package_namespace, path).unwrap();
            }
        }

        for (func_id, attributes, generics, params, ret) in funcs {
            for glide_ast::def::AttributeUsage { path } in attributes {
                let _ = self.resolve_path(&package_namespace, path).unwrap();
            }

            let mut func_namespace = package_namespace.sub();
            let func = self.engine.funcs.get_mut(func_id);

            let mut ty_params = Vec::new();
            for generic in generics {
                let ty = self.engine.tys.add(Ty::Param);
                let entry = NamespaceEntry {
                    visibility: Visibility::Private,
                    kind: NamespaceEntryKind::TyParam(ty),
                };
                func_namespace
                    .insert(generic.data().to_owned(), entry)
                    .unwrap();
                ty_params.push(ty);
            }
            func.ty_params = ty_params;

            let param_tys: Vec<TyId> = params
                .iter()
                .enumerate()
                .map(|(idx, Field { name, ty })| {
                    let ty = self.compile_ty(&func_namespace, ty)?;
                    let entry = NamespaceEntry {
                        visibility: Visibility::Private,
                        kind: NamespaceEntryKind::Local(idx),
                    };
                    func_namespace.insert(name.data().to_owned(), entry)?;
                    Ok(ty)
                })
                .collect::<Result<Vec<TyId>>>()
                .unwrap();

            let ret_ty = match ret {
                Some(ty) => self.compile_ty(&func_namespace, ty).unwrap(),
                None => TyId::VOID,
            };

            let signature = self.engine.tys.add(Ty::Func(param_tys.clone(), ret_ty));
            let func = self.engine.funcs.get_mut(func_id);
            func.signature = signature;
        }

        self.add_package(
            "core".to_owned(),
            Package {
                namespace: package_namespace,
            },
        )
        .unwrap();
    }

    pub fn compile_library(
        &mut self,
        name: String,
        package: &glide_ast::Package<'_>,
    ) -> Result<()> {
        let prelude = self.packages.get(self.prelude_package);
        let package_namespace = prelude.namespace.clone();
        let package = self.compile_package_with_namespace(package, package_namespace)?;
        self.add_package(name, package)?;
        Ok(())
    }

    fn compile_add_package(
        &mut self,
        name: String,
        package: &glide_ast::Package<'_>,
    ) -> Result<PackageId> {
        let prelude = self.packages.get(self.prelude_package);
        let package_namespace = prelude.namespace.clone();
        let package = self.compile_package_with_namespace(package, package_namespace)?;
        self.add_package(name, package)
    }

    fn compile_package_with_namespace(
        &mut self,
        package: &glide_ast::Package<'_>,
        mut package_namespace: Namespace<'static>,
    ) -> Result<Package> {
        let mut funcs = Vec::new();
        let mut attribute_defs = Vec::new();
        let mut uses = Vec::new();

        // Register names in the package namespace first.
        for Def {
            attributes,
            visibility,
            kind,
        } in &package.defs
        {
            let visibility = match visibility {
                glide_ast::def::Visibility::Pub => Visibility::Public,
                glide_ast::def::Visibility::Priv => todo!("priv keyword"),
                glide_ast::def::Visibility::Unspecified => Visibility::Private,
            };
            match kind {
                DefKind::Func(glide_ast::def::Func {
                    name,
                    generics,
                    params,
                    ret,
                    block,
                }) => {
                    let func_id = self.engine.funcs.add(Func {
                        name: name.data().to_owned(),
                        ty_params: Vec::new(),
                        signature: TyId::PLACEHOLDER,
                        body: FuncBody::Placeholder,
                        cur_mono: false,
                    });

                    let entry = NamespaceEntry {
                        visibility,
                        kind: NamespaceEntryKind::Func(func_id),
                    };
                    package_namespace.insert(name.data().to_owned(), entry)?;

                    funcs.push((func_id, attributes, generics, params, ret, block));
                }
                DefKind::Attribute(glide_ast::def::AttributeDef { name }) => {
                    let attribute = Attribute {
                        name: name.data().to_owned(),
                    };
                    let id = self.engine.attributes.add(attribute);
                    let entry = NamespaceEntry {
                        visibility,
                        kind: NamespaceEntryKind::Attribute(id),
                    };
                    package_namespace.insert(name.data().to_owned(), entry)?;

                    attribute_defs.push((attributes,));
                }
                DefKind::Struct(glide_ast::def::Struct {
                    name,
                    generics,
                    fields,
                }) => {
                    let struc = Struct {};
                    todo!()
                }
                DefKind::Use(glide_ast::def::Use { path }) => {
                    let kind = self.resolve_path(&package_namespace, path)?;
                    package_namespace.insert(
                        path.element.data().to_owned(),
                        NamespaceEntry { visibility, kind },
                    )?;
                    uses.push((attributes,));
                }
            }
        }

        for (attributes,) in attribute_defs {
            // Attributes can be applied to themselves
            // Just need to check if they actually exist
            for glide_ast::def::AttributeUsage { path } in attributes {
                let _ = self.resolve_path(&package_namespace, path)?;
            }
        }

        for (attributes,) in uses {
            for glide_ast::def::AttributeUsage { path } in attributes {
                let _ = self.resolve_path(&package_namespace, path)?;
            }
        }

        let mut funcs_2 = Vec::new();

        // Resolve and compile each function semantically.
        for (func_id, attributes, generics, params, ret, block) in funcs {
            for glide_ast::def::AttributeUsage { path } in attributes {
                let _ = self.resolve_path(&package_namespace, path)?;
            }

            let mut func_namespace = package_namespace.sub();
            let func = self.engine.funcs.get_mut(func_id);

            let mut ty_params = Vec::new();
            for generic in generics {
                let ty = self.engine.tys.add(Ty::Param);
                let entry = NamespaceEntry {
                    visibility: Visibility::Private,
                    kind: NamespaceEntryKind::TyParam(ty),
                };
                func_namespace.insert(generic.data().to_owned(), entry)?;
                ty_params.push(ty);
            }
            func.ty_params = ty_params;

            let param_tys: Vec<TyId> = params
                .iter()
                .enumerate()
                .map(|(idx, Field { name, ty })| {
                    let ty = self.compile_ty(&func_namespace, ty)?;
                    let entry = NamespaceEntry {
                        visibility: Visibility::Private,
                        kind: NamespaceEntryKind::Local(idx),
                    };
                    func_namespace.insert(name.data().to_owned(), entry)?;
                    Ok(ty)
                })
                .collect::<Result<Vec<TyId>>>()?;

            let ret_ty = match ret {
                Some(ty) => self.compile_ty(&func_namespace, ty)?,
                None => TyId::VOID,
            };

            let signature = self.engine.tys.add(Ty::Func(param_tys.clone(), ret_ty));
            let func = self.engine.funcs.get_mut(func_id);
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
                self.compile_block(&func_namespace, &mut locals, ret_ty, block)?;
            let value = match (
                self.engine.tys.is_void(ret_ty),
                self.engine.tys.is_void(found_ty),
            ) {
                (true, true | false) => {
                    // If the last statement was not void, do not try to make it, as it may cause
                    // incorrent inferrence.
                    Value::RetVoid(Box::new(value))
                }
                (false, true) => {
                    return Err(Error::TyMismatch);
                }
                (false, false) => {
                    self.engine.tys.unify(ret_ty, found_ty)?;
                    Value::Ret(Box::new(value))
                }
            };
            let func = self.engine.funcs.get_mut(func_id);
            func.body = FuncBody::Normal(value);
        }

        Ok(Package {
            namespace: package_namespace,
        })
    }

    fn add_package(&mut self, name: String, package: Package) -> Result<PackageId> {
        let package_id = self.packages.add(package);
        match self.packages_lookup.entry(name) {
            hash_map::Entry::Occupied(_) => Err(Error::Shadow),
            hash_map::Entry::Vacant(entry) => {
                entry.insert(package_id);
                Ok(package_id)
            }
        }
    }

    fn resolve_path(
        &self,
        namespace: &Namespace<'_>,
        path: &Path<'_>,
    ) -> Result<NamespaceEntryKind> {
        match path.package {
            Some(package) => {
                let &package_id = self
                    .packages_lookup
                    .get(package.data())
                    .ok_or(Error::UnresolvedName)?;
                let entry = self
                    .packages
                    .get(package_id)
                    .namespace
                    .get(path.element.data())
                    .ok_or(Error::UnresolvedName)?;
                if entry.visibility >= Visibility::Public {
                    Ok(entry.kind)
                } else {
                    Err(Error::VisiblityRestricted)
                }
            }
            None => Ok(namespace
                .get(path.element.data())
                .ok_or(Error::UnresolvedName)?
                .kind),
        }
    }

    fn compile_ty(
        &mut self,
        namespace: &Namespace<'_>,
        glide_ast::ty::Ty { name, generics }: &glide_ast::ty::Ty<'_>,
    ) -> Result<TyId> {
        let NamespaceEntry {
            visibility: _,
            kind,
        } = namespace.get(name.data()).ok_or(Error::UnresolvedName)?;
        let mut ty_args = Vec::new();
        for generic in generics {
            ty_args.push(self.compile_ty(namespace, generic)?);
        }
        match kind {
            NamespaceEntryKind::Void if ty_args.is_empty() => Ok(TyId::VOID),
            NamespaceEntryKind::Void => Err(Error::WrongTyArgs),
            NamespaceEntryKind::Int if ty_args.is_empty() => Ok(TyId::INT),
            NamespaceEntryKind::Int => Err(Error::WrongTyArgs),
            NamespaceEntryKind::Bool if ty_args.is_empty() => Ok(TyId::BOOL),
            NamespaceEntryKind::Bool => Err(Error::WrongTyArgs),
            NamespaceEntryKind::String if ty_args.is_empty() => Ok(TyId::STRING),
            NamespaceEntryKind::String => Err(Error::WrongTyArgs),
            NamespaceEntryKind::Slice if ty_args.len() == 1 => {
                Ok(self.engine.tys.add(Ty::Slice(ty_args[0])))
            }
            NamespaceEntryKind::Slice => Err(Error::WrongTyArgs),
            NamespaceEntryKind::TyParam(ty) if ty_args.is_empty() => Ok(ty),
            NamespaceEntryKind::TyParam(_) => Err(Error::WrongTyArgs),
            NamespaceEntryKind::Struct(_) => todo!(),
            NamespaceEntryKind::Local(_) => Err(Error::NotTy),
            NamespaceEntryKind::Func(_) => Err(Error::NotTy),
            NamespaceEntryKind::Attribute(_) => Err(Error::NotTy),
        }
    }

    fn compile_block(
        &mut self,
        namespace: &Namespace<'_>,
        locals: &mut Vec<TyId>,
        return_ty: TyId,
        block: &Block<'_>,
    ) -> Result<(Value, TyId)> {
        let mut namespace = namespace.sub();
        let previous_locals_len = locals.len();

        let (values, ty) = match &block.stmts[..] {
            [] => (vec![Value::Void], TyId::VOID),
            [rest @ .., last] => {
                let mut values = Vec::new();
                for stmt in rest {
                    let (value, ty) = self.compile_stmt(&mut namespace, locals, return_ty, stmt)?;
                    values.push(value);
                }
                let (last_value, last_ty) =
                    self.compile_stmt(&mut namespace, locals, return_ty, last)?;
                values.push(last_value);
                (values, last_ty)
            }
        };

        locals.truncate(previous_locals_len);

        let value = Value::Block(values);
        Ok((value, ty))
    }

    fn compile_stmt(
        &mut self,
        namespace: &mut Namespace<'_>,
        locals: &mut Vec<TyId>,
        return_ty: TyId,
        stmt: &Stmt<'_>,
    ) -> Result<(Value, TyId)> {
        match stmt {
            Stmt::Var(VarDecl { name, ty, value }) => {
                let (value, found_ty) = self.compile_expr(namespace, locals, return_ty, value)?;
                if let Some(expect_ty) = ty {
                    let expect_ty = self.compile_ty(namespace, expect_ty)?;
                    self.engine.tys.unify(expect_ty, found_ty)?;
                }
                let idx = locals.len();
                locals.push(found_ty);
                let entry = NamespaceEntry {
                    visibility: Visibility::Private,
                    kind: NamespaceEntryKind::Local(idx),
                };
                namespace.insert(name.data().to_owned(), entry)?;
                Ok((Value::StoreVar(Box::new(value)), TyId::VOID))
            }
            Stmt::Expr(expr) => self.compile_expr(namespace, locals, return_ty, expr),
            Stmt::Return(expr) => {
                let (value, ty) = self.compile_expr(namespace, locals, return_ty, expr)?;
                self.engine.tys.unify(return_ty, ty)?;
                Ok((Value::Ret(Box::new(value)), TyId::VOID))
            }
        }
    }

    fn compile_expr(
        &mut self,
        namespace: &Namespace<'_>,
        locals: &mut Vec<TyId>,
        return_ty: TyId,
        expr: &Expr<'_>,
    ) -> Result<(Value, TyId)> {
        match expr {
            Expr::Integer(span) => {
                let value: i64 = span.data().parse().map_err(|_| Error::IntegerOverflow)?;
                Ok((Value::ConstantInt(value), TyId::INT))
            }
            Expr::String { data } => Ok((Value::ConstantString(data.clone()), TyId::STRING)),
            Expr::Var(path) => match self.resolve_path(namespace, path)? {
                NamespaceEntryKind::Void
                | NamespaceEntryKind::Int
                | NamespaceEntryKind::Bool
                | NamespaceEntryKind::String
                | NamespaceEntryKind::Slice
                | NamespaceEntryKind::TyParam(_)
                | NamespaceEntryKind::Struct(_)
                | NamespaceEntryKind::Attribute(_) => Err(Error::NotValue),
                NamespaceEntryKind::Local(idx) => Ok((Value::Local(idx), locals[idx])),
                NamespaceEntryKind::Func(func) => {
                    let usage = self.engine.use_func(func);
                    let signature = usage.signature;
                    Ok((Value::Func(usage), signature))
                }
            },
            Expr::Call(Call { receiver, args }) => {
                let (receiver_value, receiver_ty) =
                    self.compile_expr(namespace, locals, return_ty, receiver)?;
                let mut arg_values = Vec::new();
                let mut arg_tys = Vec::new();
                for arg in args {
                    let (value, ty) = self.compile_expr(namespace, locals, return_ty, arg)?;
                    arg_values.push(value);
                    arg_tys.push(ty);
                }
                let ret_ty = self.engine.tys.add(Ty::Infer);
                let expect_receiver_ty = self.engine.tys.add(Ty::Func(arg_tys, ret_ty));
                self.engine.tys.unify(expect_receiver_ty, receiver_ty)?;
                Ok((Value::Call(Box::new(receiver_value), arg_values), ret_ty))
            }
            Expr::If(If { branches, els }) => {
                let mut base = None;
                let mut cur_else = &mut base;

                let mut tys = Vec::new();

                for (cond, block) in branches {
                    let (cond_value, cond_ty) =
                        self.compile_expr(namespace, locals, return_ty, cond)?;
                    self.engine.tys.unify(cond_ty, TyId::BOOL)?;
                    let (block_value, ty) =
                        self.compile_block(namespace, locals, return_ty, block)?;
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
                        let (value, ty) =
                            self.compile_block(namespace, locals, return_ty, block)?;
                        tys.push(ty);
                        *cur_else = Some(Box::new(value));
                    }
                    None => todo!(),
                }

                for s in tys.windows(2) {
                    if let [l, r] = s {
                        self.engine.tys.unify(*l, *r)?;
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

    pub fn compile_binary(&mut self, name: String, package: &glide_ast::Package<'_>) -> Result<Ir> {
        let package_id = self.compile_add_package(name, package)?;

        let main_id = match self.packages.get(package_id).namespace.get("main") {
            Some(NamespaceEntry {
                visibility,
                kind: NamespaceEntryKind::Func(main_id),
            }) => {
                // main can be public or private
                let _ = visibility;
                main_id
            }
            _ => return Err(Error::NoMain),
        };

        // Monomorphize functions beginning with main.
        let mut compiled = FuncsCompiled::new();
        let mut ir_funcs = glide_ir::Funcs::new();

        let main = self.engine.funcs.get(main_id);
        if !main.ty_params.is_empty() {
            return Err(Error::NoMain);
        }
        let expected_main_ty = self.engine.tys.add(Ty::Func(Vec::new(), TyId::VOID));
        self.engine.tys.unify(expected_main_ty, main.signature)?;

        let main_usage = self.engine.use_func(main_id);

        let main_id = self.monomorphize_func(&mut ir_funcs, &mut compiled, &main_usage)?;

        Ok(Ir {
            funcs: ir_funcs,
            main_func: main_id,
        })
    }

    fn monomorphize_func(
        &mut self,
        ir_funcs: &mut glide_ir::Funcs,
        funcs_compiled: &mut FuncsCompiled,
        usage: &FuncUsage,
    ) -> Result<glide_ir::FuncId> {
        let compiled = {
            let mut ty_args = Vec::new();
            for &ty_arg in &usage.ty_args {
                ty_args.push(self.lower_ty(ty_arg)?);
            }
            FuncCompiled {
                func: usage.func,
                ty_args,
            }
        };

        if let Some(id) = funcs_compiled.get(&compiled) {
            return Ok(id);
        }

        if self.engine.funcs.get(usage.func).cur_mono {
            return Err(Error::TyRecursion);
        }
        self.engine.funcs.get_mut(usage.func).cur_mono = true;

        let mut name = self.engine.funcs.get(usage.func).name.clone();
        if let [rest @ .., last] = &compiled.ty_args[..] {
            write!(name, "<").unwrap();
            for ty_arg in rest {
                write!(name, "{}, ", ty_arg).unwrap();
            }
            write!(name, "{}>", last).unwrap();
        }

        let signature = self.lower_ty(usage.signature)?;

        let id = ir_funcs.add(glide_ir::Func {
            name,
            signature,
            body: glide_ir::FuncBody::Placeholder,
        });
        funcs_compiled.insert(compiled, id);

        let body = self.engine.funcs.get(usage.func).body.clone();
        let body = match body {
            FuncBody::Placeholder => unreachable!(),
            FuncBody::Normal(value) => {
                glide_ir::FuncBody::Normal(self.lower_value(ir_funcs, funcs_compiled, value)?)
            }
            FuncBody::Println => glide_ir::FuncBody::Print,
            FuncBody::PrintlnInt => glide_ir::FuncBody::PrintInt,
            FuncBody::Add => glide_ir::FuncBody::Add,
            FuncBody::Sub => glide_ir::FuncBody::Sub,
            FuncBody::EqInt => glide_ir::FuncBody::EqInt,
        };

        ir_funcs.get_mut(id).body = body;

        self.engine.funcs.get_mut(usage.func).cur_mono = false;

        Ok(id)
    }

    fn lower_value(
        &mut self,
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
                let id = self.monomorphize_func(ir_funcs, funcs_compiled, &usage)?;
                glide_ir::Value::Func(id)
            }
            Value::Call(receiver, args) => {
                let receiver = self.lower_value(ir_funcs, funcs_compiled, *receiver)?;
                let args = args
                    .into_iter()
                    .map(|v| self.lower_value(ir_funcs, funcs_compiled, v))
                    .collect::<Result<Vec<glide_ir::Value>>>()?;
                glide_ir::Value::Call(Box::new(receiver), args)
            }
            Value::Ret(value) => glide_ir::Value::Ret(Box::new(self.lower_value(
                ir_funcs,
                funcs_compiled,
                *value,
            )?)),
            Value::RetVoid(value) => glide_ir::Value::RetVoid(Box::new(self.lower_value(
                ir_funcs,
                funcs_compiled,
                *value,
            )?)),
            Value::If { cond, then, els } => {
                let cond = self.lower_value(ir_funcs, funcs_compiled, *cond)?;
                let then = self.lower_value(ir_funcs, funcs_compiled, *then)?;
                let els = els
                    .map(|els| self.lower_value(ir_funcs, funcs_compiled, *els))
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
                    .map(|v| self.lower_value(ir_funcs, funcs_compiled, v))
                    .collect::<Result<Vec<glide_ir::Value>>>()?,
            ),
            Value::StoreVar(value) => glide_ir::Value::StoreVar(Box::new(self.lower_value(
                ir_funcs,
                funcs_compiled,
                *value,
            )?)),
        })
    }

    fn lower_ty(&self, ty: TyId) -> Result<glide_ir::Ty> {
        match self.engine.tys.get(ty) {
            Ty::Void => Ok(glide_ir::Ty::Void),
            Ty::Int => Ok(glide_ir::Ty::Int),
            Ty::Bool => Ok(glide_ir::Ty::Bool),
            Ty::String => Ok(glide_ir::Ty::String),
            Ty::Slice(elem) => Ok(glide_ir::Ty::Slice(Box::new(self.lower_ty(*elem)?))),
            Ty::Func(params, ret) => {
                let params = params
                    .iter()
                    .map(|&param| self.lower_ty(param))
                    .collect::<Result<Vec<glide_ir::Ty>>>()?;
                let ret = self.lower_ty(*ret)?;
                Ok(glide_ir::Ty::Func(params, Box::new(ret)))
            }
            Ty::Param => panic!(),
            Ty::Infer => Err(Error::CannotInfer),
            Ty::Equal(ty) => self.lower_ty(*ty),
        }
    }
}
