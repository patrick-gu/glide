use glide_codegen::codegen;
use glide_ir::{Func, FuncBody, Funcs, Ir, Ty, Value};

#[test]
fn fibonacci() {
    let mut funcs = Funcs::new();
    let eq_int = funcs.add(Func {
        name: "eqInt".to_owned(),
        signature: Ty::Func(vec![Ty::Int, Ty::Int], Box::new(Ty::Bool)),
        body: FuncBody::EqInt,
    });
    let add = funcs.add(Func {
        name: "add".to_owned(),
        signature: Ty::Func(vec![Ty::Int, Ty::Int], Box::new(Ty::Int)),
        body: FuncBody::Add,
    });
    let sub = funcs.add(Func {
        name: "sub".to_owned(),
        signature: Ty::Func(vec![Ty::Int, Ty::Int], Box::new(Ty::Int)),
        body: FuncBody::Sub,
    });
    let print_int = funcs.add(Func {
        name: "printInt".to_owned(),
        signature: Ty::Func(vec![Ty::Int], Box::new(Ty::Void)),
        body: FuncBody::PrintInt,
    });
    let fibonacci_func = funcs.add(Func {
        name: "fibonacci".to_owned(),
        signature: Ty::Func(vec![Ty::Int], Box::new(Ty::Int)),
        body: FuncBody::Normal(Vec::new()),
    });
    funcs.get_mut(fibonacci_func).body = FuncBody::Normal(vec![Value::Ret(Box::new(Value::If {
        cond: Box::new(Value::Call(
            Box::new(Value::Func(eq_int)),
            vec![Value::Param(0), Value::ConstantInt(0)],
        )),
        then: vec![Value::ConstantInt(0)],
        els: Some(vec![Value::If {
            cond: Box::new(Value::Call(
                Box::new(Value::Func(eq_int)),
                vec![Value::Param(0), Value::ConstantInt(1)],
            )),
            then: vec![Value::ConstantInt(1)],
            els: Some(vec![Value::Call(
                Box::new(Value::Func(add)),
                vec![
                    Value::Call(
                        Box::new(Value::Func(fibonacci_func)),
                        vec![Value::Call(
                            Box::new(Value::Func(sub)),
                            vec![Value::Param(0), Value::ConstantInt(1)],
                        )],
                    ),
                    Value::Call(
                        Box::new(Value::Func(fibonacci_func)),
                        vec![Value::Call(
                            Box::new(Value::Func(sub)),
                            vec![Value::Param(0), Value::ConstantInt(2)],
                        )],
                    ),
                ],
            )]),
        }]),
    }))]);
    let main_func = funcs.add(Func {
        name: "main".to_owned(),
        signature: Ty::Func(vec![], Box::new(Ty::Void)),
        body: FuncBody::Normal(
            (0..=40)
                .map(|v| {
                    Value::Call(
                        Box::new(Value::Func(print_int)),
                        vec![Value::Call(
                            Box::new(Value::Func(fibonacci_func)),
                            vec![Value::ConstantInt(v)],
                        )],
                    )
                })
                .chain(Some(Value::Ret(Box::new(Value::Void))))
                .collect(),
        ),
    });
    let ir = Ir { funcs, main_func };
    codegen(&ir);
}
