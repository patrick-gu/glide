use glide_ast::{
    def::{Def, Func},
    expr::{Block, Call, Expr},
    stmt::{Stmt, VarDecl},
    ty::Ty,
};
use glide_parser::parse;
use glide_span::source::Source;

#[test]
fn simple_string_var() {
    let source = Source::anonymous(
        r#"
func main() {
    let x = "x"
    let y String = "yy"
    y
    print(x)
}
"#,
    );
    let ast = parse(&source).unwrap();
    assert_eq!(ast.defs.len(), 1);
    let Def::Func(Func {
        name,
        generics,
        params,
        ret,
        block: Block { stmts },
    }) = &ast.defs[0];
    assert_eq!(name.data(), "main");
    assert!(generics.is_empty());
    assert!(params.is_empty());
    assert!(ret.is_none());
    match &stmts[..] {
        [Stmt::Var(VarDecl {
            name: x_name,
            ty: None,
            value: Expr::String { data: x_data },
        }), Stmt::Var(VarDecl {
            name: y_name,
            ty:
                Some(Ty {
                    name: string_name,
                    generics: string_generics,
                }),
            value: Expr::String { data: y_data },
        }), Stmt::Expr(Expr::Var(y_usage)), Stmt::Expr(Expr::Call(Call { receiver, args }))] => {
            assert_eq!(x_name.data(), "x");
            assert_eq!(x_data, b"x");
            assert_eq!(y_name.data(), "y");
            assert_eq!(string_name.data(), "String");
            assert!(string_generics.is_empty());
            assert_eq!(y_data, b"yy");
            assert_eq!(y_usage.data(), "y");
            match (&**receiver, &args[..]) {
                (Expr::Var(print_name), [Expr::Var(x_name)]) => {
                    assert_eq!(print_name.data(), "print");
                    assert_eq!(x_name.data(), "x");
                }
                _ => panic!(),
            }
        }
        _ => panic!(),
    }
}
