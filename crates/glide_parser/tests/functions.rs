use glide_ast::{
    def::{Def, Func},
    expr::{Block, Call, Expr},
    stmt::Stmt,
};
use glide_parser::parse;
use glide_span::source::Source;

#[test]
fn generic_func() {
    let source = Source::anonymous(
        r#"
func foo<A, B, C>(a A, b B, c C, foo Foo<A, B, C,>,) Foo<A, B, C> {
    foo(a, b, c)
}

func bar<T>(baz Bar<Bar<Bar<Bar<T, Slice<T>>, T>, T,>, Bar<T, T>>) {}
"#,
    );
    let ast = parse(&source).unwrap();
    assert_eq!(ast.defs.len(), 2);
    match &ast.defs[0] {
        Def::Func(Func {
            name,
            generics,
            params,
            ret,
            block: Block { stmts },
        }) => {
            assert_eq!(name.data(), "foo");
            assert_eq!(generics.len(), 3);
            assert_eq!(generics[0].data(), "A");
            assert_eq!(generics[1].data(), "B");
            assert_eq!(generics[2].data(), "C");

            assert_eq!(params.len(), 4);
            assert_eq!(params[0].0.data(), "a");
            assert_eq!(params[0].1.name.data(), "A");
            assert!(params[1].1.generics.is_empty());
            assert_eq!(params[1].0.data(), "b");
            assert_eq!(params[1].1.name.data(), "B");
            assert!(params[1].1.generics.is_empty());
            assert_eq!(params[2].0.data(), "c");
            assert_eq!(params[2].1.name.data(), "C");
            assert!(params[2].1.generics.is_empty());
            assert_eq!(params[3].0.data(), "foo");
            assert_eq!(params[3].1.name.data(), "Foo");
            assert_eq!(params[3].1.generics.len(), 3);
            assert_eq!(params[3].1.generics[0].name.data(), "A");
            assert!(params[3].1.generics[0].generics.is_empty());
            assert_eq!(params[3].1.generics[1].name.data(), "B");
            assert!(params[3].1.generics[1].generics.is_empty());
            assert_eq!(params[3].1.generics[2].name.data(), "C");
            assert!(params[3].1.generics[2].generics.is_empty());

            let ret = ret.as_ref().unwrap();
            assert_eq!(ret.name.data(), "Foo");
            assert_eq!(ret.generics[0].name.data(), "A");
            assert!(ret.generics[0].generics.is_empty());
            assert_eq!(ret.generics[1].name.data(), "B");
            assert!(ret.generics[1].generics.is_empty());
            assert_eq!(ret.generics[2].name.data(), "C");
            assert!(ret.generics[2].generics.is_empty());

            assert_eq!(stmts.len(), 1);
            match &stmts[0] {
                Stmt::Expr(Expr::Call(Call { receiver, args })) => {
                    match &**receiver {
                        Expr::Var(name) => assert_eq!(name.data(), "foo"),
                        _ => panic!(),
                    }
                    assert_eq!(args.len(), 3);
                    match &args[0] {
                        Expr::Var(name) => assert_eq!(name.data(), "a"),
                        _ => panic!(),
                    }
                    match &args[1] {
                        Expr::Var(name) => assert_eq!(name.data(), "b"),
                        _ => panic!(),
                    }
                    match &args[2] {
                        Expr::Var(name) => assert_eq!(name.data(), "c"),
                        _ => panic!(),
                    }
                }
                _ => panic!(),
            }
        }
    }

    match &ast.defs[1] {
        Def::Func(Func {
            name,
            generics,
            params,
            ret,
            block: Block { stmts },
        }) => {
            assert_eq!(name.data(), "bar");
            assert_eq!(generics.len(), 1);
            assert_eq!(generics[0].data(), "T");

            assert_eq!(params.len(), 1);
            assert_eq!(params[0].0.data(), "baz");
            let ty = &params[0].1;
            assert_eq!(ty.name.data(), "Bar");
            assert_eq!(ty.generics.len(), 2);
            assert_eq!(ty.generics[1].name.data(), "Bar");
            assert_eq!(ty.generics[1].generics.len(), 2);
            assert_eq!(ty.generics[1].generics[0].name.data(), "T");
            assert!(ty.generics[1].generics[0].generics.is_empty());
            assert_eq!(ty.generics[1].generics[1].name.data(), "T");
            assert!(ty.generics[1].generics[1].generics.is_empty());

            let ty = &ty.generics[0];
            assert_eq!(ty.generics.len(), 2);
            assert_eq!(ty.generics[1].name.data(), "T");
            assert!(ty.generics[1].generics.is_empty());

            let ty = &ty.generics[0];
            assert_eq!(ty.generics.len(), 2);
            assert_eq!(ty.generics[1].name.data(), "T");
            assert!(ty.generics[1].generics.is_empty());

            let ty = &ty.generics[0];
            assert_eq!(ty.generics.len(), 2);
            assert_eq!(ty.generics[0].name.data(), "T");
            assert!(ty.generics[0].generics.is_empty());
            assert_eq!(ty.generics[1].name.data(), "Slice");
            assert_eq!(ty.generics[1].generics.len(), 1);
            assert_eq!(ty.generics[1].generics[0].name.data(), "T");
            assert!(ty.generics[1].generics[0].generics.is_empty());

            assert!(ret.is_none());
            assert!(stmts.is_empty());
        }
    }
}
