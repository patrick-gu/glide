use glide_ast::{
    def::Def,
    expr::{Call, Expr},
    stmt::Stmt,
};
use glide_parser::parse;
use glide_span::source::Source;

#[test]
fn hello_world() {
    let source = Source::anonymous(
        r#"
func main() {
    print("Hello World!")
}
"#,
    );
    let ast = parse(&source).unwrap();
    assert_eq!(ast.defs.len(), 1);
    let Def::Func(main) = &ast.defs[0];
    assert_eq!(main.name.data(), "main");
    assert!(main.generics.is_empty());
    assert!(main.params.is_empty());
    assert!(main.ret.is_none());
    assert_eq!(main.stmts.len(), 1);
    let print = &main.stmts[0];
    match print {
        Stmt::Expr(Expr::Call(Call { receiver, args })) => {
            match &**receiver {
                Expr::Var(name) => assert_eq!(name.data(), "print"),
                _ => panic!(),
            }
            assert_eq!(args.len(), 1);
            let string = &args[0];
            match string {
                Expr::String { data } => assert_eq!(data, b"Hello World!"),
                _ => panic!(),
            }
        }
        _ => panic!(),
    }
}

#[test]
fn escape_quotation_backslash() {
    let source = Source::anonymous(
        r#"
func main() {
    "aaa\"aa\"\"\\b"
}
"#,
    );
    let ast = parse(&source).unwrap();
    assert_eq!(ast.defs.len(), 1);
    let Def::Func(main) = &ast.defs[0];
    assert_eq!(main.name.data(), "main");
    assert!(main.generics.is_empty());
    assert!(main.params.is_empty());
    assert!(main.ret.is_none());
    assert_eq!(main.stmts.len(), 1);
    match &main.stmts[0] {
        Stmt::Expr(Expr::String { data }) => assert_eq!(data, br#"aaa"aa""\b"#),
        _ => panic!(),
    }
}
