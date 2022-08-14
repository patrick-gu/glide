use std::result;

use glide_ast::{
    def::{Def, Func},
    expr::{Block, Call, Expr, If},
    stmt::{Stmt, VarDecl},
    ty::Ty,
    Ast,
};
use glide_span::{source::Source, Span};

#[derive(Copy, Clone, Debug)]
pub struct Token<'a> {
    span: Span<'a>,
    kind: TokenKind,
}

macro_rules! token_kinds {
    (
        $($simple:ident,)+
        -
        $($keyword:ident => $text:literal,)+
    ) => {
        #[derive(Copy, Clone, Eq, PartialEq, Debug)]
        enum TokenKind {
            $($simple,)+
            $($keyword,)+
        }

        impl TokenKind {
            fn ident_kw(ident: &str) -> Self {
                match ident {
                    $($text => Self::$keyword,)+
                    _ => Self::Ident,
                }
            }
        }
    };
}

token_kinds!(
    LParen,
    RParen,
    LBrace,
    RBrace,
    LessThan,
    GreaterThan,
    Quote,
    Apostrophe,
    Comma,
    Equal,
    Slash,

    Ident,
    Integer,

    -

    KwElse => "else",
    KwFalse => "false",
    KwFor => "for",
    KwFunc => "func",
    KwIf => "if",
    KwLet => "let",
    KwTrue => "true",
);

struct Lexer<'a> {
    next: Option<Token<'a>>,
    rem: Span<'a>,
}

impl<'a> Lexer<'a> {
    fn next_as(&mut self, kind: TokenKind) -> Result<'a, Option<Span<'a>>> {
        self.update_next()?;
        if let Some(Token {
            span,
            kind: found_kind,
        }) = self.next
        {
            if found_kind == kind {
                self.next = None;
                return Ok(Some(span));
            }
        }
        Ok(None)
    }

    fn next(&mut self) -> Result<'a, Option<Token<'a>>> {
        self.update_next()?;
        Ok(self.next.take())
    }

    fn update_next(&mut self) -> Result<'a, ()> {
        if self.next.is_some() {
            return Ok(());
        }
        macro_rules! do_match {
            (
                $($byte:literal => $kind:expr,)+
            ) => {
                match self.rem.data().as_bytes().get(0).copied() {
                    Some(b' ' | b'\t' | b'\n' | b'\r') => {
                        self.advance(1);
                    }
                    $(
                        Some($byte) => {
                            break Token {
                                span: self.advance(1),
                                kind: $kind,
                            };
                        }
                    ),+
                    Some(b'/') => {
                        if let Some(&b'/') = self.rem.data().as_bytes().get(1) {
                            let mut end = 2;
                            loop {
                                match self.rem.data().as_bytes().get(end) {
                                    Some(b'\n') | None => break,
                                    Some(_) => {
                                        end += 1;
                                    }
                                }
                            }
                            self.advance(end);
                        } else {
                            break Token {
                                span: self.advance(1),
                                kind: TokenKind::Slash,
                            };
                        }
                    }
                    Some(b'a'..=b'z' | b'A'..=b'Z' | b'_') => {
                        let mut end = 1;
                        while let Some(b'a'..=b'z' | b'A'..=b'Z' | b'_' | b'0'..=b'9') = self.rem.data().as_bytes().get(end) {
                            end += 1;
                        }
                        let span = self.advance(end);
                        break Token {
                            span,
                            kind: TokenKind::ident_kw(span.data()),
                        };
                    }
                    Some(b'0'..=b'9') => {
                        let mut end = 1;
                        while let Some(b'0'..=b'9') = self.rem.data().as_bytes().get(end) {
                            end += 1;
                        }
                        break Token {
                            span: self.advance(end),
                            kind: TokenKind::Integer,
                        };
                    }
                    Some(_) => return Err(Error::UnexpectedChar(self.advance(1))),
                    None => return Ok(()),
                }
            };
        }
        self.next = Some(loop {
            do_match!(
                b'(' => TokenKind::LParen,
                b')' => TokenKind::RParen,
                b'{' => TokenKind::LBrace,
                b'}' => TokenKind::RBrace,
                b'<' => TokenKind::LessThan,
                b'>' => TokenKind::GreaterThan,
                b'"' => TokenKind::Quote,
                b'\'' => TokenKind::Apostrophe,
                b',' => TokenKind::Comma,
                b'=' => TokenKind::Equal,
            );
        });
        Ok(())
    }

    fn advance(&mut self, count: usize) -> Span<'a> {
        self.rem.advance(count)
    }

    fn expect_as(&mut self, kind: TokenKind) -> Result<'a, Span<'a>> {
        if let Some(span) = self.next_as(kind)? {
            Ok(span)
        } else {
            Err(self.unexpected_token())
        }
    }

    fn unexpected_token(&mut self) -> Error<'a> {
        match self.next() {
            Ok(Some(token)) => Error::UnexpectedToken(token),
            Ok(None) => Error::UnexpectedEof,
            Err(error) => error,
        }
    }
}

#[derive(Debug)]
pub enum Error<'a> {
    UnexpectedChar(Span<'a>),
    UnexpectedToken(Token<'a>),
    UnexpectedEof,
    UnexpectedEscape,
}

type Result<'a, T> = result::Result<T, Error<'a>>;

pub fn parse(source: &Source) -> Result<'_, Ast<'_>> {
    let mut lexer = Lexer {
        next: None,
        rem: Span::full(source),
    };
    let mut defs = Vec::new();
    while let Some(def) = def(&mut lexer)? {
        defs.push(def);
    }
    Ok(Ast { defs })
}

fn def<'a>(lexer: &mut Lexer<'a>) -> Result<'a, Option<Def<'a>>> {
    if lexer.next_as(TokenKind::KwFunc)?.is_some() {
        let name = lexer.expect_as(TokenKind::Ident)?;

        let generics = match lexer.next_as(TokenKind::LessThan)? {
            Some(_) => {
                list1(
                    lexer,
                    |lexer| lexer.expect_as(TokenKind::Ident),
                    TokenKind::GreaterThan,
                )?
                .0
            }
            None => Vec::new(),
        };

        let _ = lexer.expect_as(TokenKind::LParen)?;

        let (params, _) = list(
            lexer,
            |lexer| {
                let name = lexer.expect_as(TokenKind::Ident)?;
                let ty = ty(lexer)?;
                Ok((name, ty))
            },
            TokenKind::RParen,
        )?;

        let (ret, _) = match lexer.next_as(TokenKind::LBrace)? {
            Some(l_brace) => (None, l_brace),
            None => {
                let ret = ty(lexer)?;
                let l_brace = lexer.expect_as(TokenKind::LBrace)?;
                (Some(ret), l_brace)
            }
        };

        let block = block_after_l_paren(lexer)?;

        return Ok(Some(Def::Func(Func {
            name,
            generics,
            params,
            ret,
            block,
        })));
    }
    if let Some(token) = lexer.next()? {
        Err(Error::UnexpectedToken(token))
    } else {
        Ok(None)
    }
}

fn ty<'a>(lexer: &mut Lexer<'a>) -> Result<'a, Ty<'a>> {
    let ident = lexer.expect_as(TokenKind::Ident)?;
    let generics = if lexer.next_as(TokenKind::LessThan)?.is_some() {
        let (generics, _) = list1(lexer, ty, TokenKind::GreaterThan)?;
        generics
    } else {
        Vec::new()
    };
    Ok(Ty {
        name: ident,
        generics,
    })
}

fn stmt<'a>(lexer: &mut Lexer<'a>) -> Result<'a, Stmt<'a>> {
    if lexer.next_as(TokenKind::KwLet)?.is_some() {
        let name = lexer.expect_as(TokenKind::Ident)?;
        let ty = match lexer.next_as(TokenKind::Equal)? {
            Some(_) => None,
            None => {
                let ty = ty(lexer)?;
                lexer.expect_as(TokenKind::Equal)?;
                Some(ty)
            }
        };
        let value = expr(lexer)?;
        return Ok(Stmt::Var(VarDecl { name, ty, value }));
    }
    Ok(Stmt::Expr(expr(lexer)?))
}

fn expr<'a>(lexer: &mut Lexer<'a>) -> Result<'a, Expr<'a>> {
    if_else(lexer)
}

fn if_else<'a>(lexer: &mut Lexer<'a>) -> Result<'a, Expr<'a>> {
    if lexer.next_as(TokenKind::KwIf)?.is_some() {
        let mut branches = vec![(call(lexer)?, block(lexer)?)];
        let els = loop {
            if lexer.next_as(TokenKind::KwElse)?.is_none() {
                break None;
            }
            if lexer.next_as(TokenKind::KwIf)?.is_none() {
                break Some(block(lexer)?);
            }
            branches.push((call(lexer)?, block(lexer)?));
        };
        return Ok(Expr::If(If { branches, els }));
    }
    call(lexer)
}

fn call<'a>(lexer: &mut Lexer<'a>) -> Result<'a, Expr<'a>> {
    let mut cur_expr = var_literal(lexer)?;
    while lexer.next_as(TokenKind::LParen)?.is_some() {
        let (args, _) = list(lexer, expr, TokenKind::RParen)?;
        cur_expr = Expr::Call(Call {
            receiver: Box::new(cur_expr),
            args,
        });
    }
    Ok(cur_expr)
}

fn var_literal<'a>(lexer: &mut Lexer<'a>) -> Result<'a, Expr<'a>> {
    if let Some(ident) = lexer.next_as(TokenKind::Ident)? {
        return Ok(Expr::Var(ident));
    }
    if let Some(integer) = lexer.next_as(TokenKind::Integer)? {
        return Ok(Expr::Integer(integer));
    }
    if let Some(_true) = lexer.next_as(TokenKind::KwTrue)? {
        return Ok(Expr::True);
    }
    if let Some(_false) = lexer.next_as(TokenKind::KwFalse)? {
        return Ok(Expr::False);
    }
    if lexer.next_as(TokenKind::Quote)?.is_some() {
        let mut data = Vec::new();
        loop {
            match lexer.rem.data().as_bytes().get(0) {
                Some(b'"') => {
                    lexer.advance(1);
                    break;
                }
                Some(b'\\') => {
                    lexer.advance(1);
                    match lexer.rem.data().as_bytes().get(0) {
                        Some(byte @ (b'"' | b'\\')) => data.push(*byte),
                        Some(_) => {
                            lexer.advance(1);
                            return Err(Error::UnexpectedEscape);
                        }
                        None => {
                            lexer.advance(1);
                            return Err(Error::UnexpectedEof);
                        }
                    }
                    lexer.advance(1);
                }
                Some(byte @ 0..=127) => {
                    data.push(*byte);
                    lexer.advance(1);
                }
                Some(128..=255) => {
                    lexer.advance(1);
                    return Err(Error::UnexpectedChar(lexer.advance(1)));
                }
                None => {
                    lexer.advance(1);
                    return Err(Error::UnexpectedEof);
                }
            }
        }
        return Ok(Expr::String { data });
    }
    Err(lexer.unexpected_token())
}

fn list<'a, T>(
    lexer: &mut Lexer<'a>,
    mut elem_gen: impl FnMut(&mut Lexer<'a>) -> Result<'a, T>,
    terminal: TokenKind,
) -> Result<'a, (Vec<T>, Span<'a>)> {
    let mut elems = Vec::new();
    let span = loop {
        if let Some(span) = lexer.next_as(terminal)? {
            break span;
        }
        elems.push(elem_gen(lexer)?);
        match lexer.next_as(TokenKind::Comma)? {
            Some(_) => (),
            None => {
                break lexer.expect_as(terminal)?;
            }
        }
    };
    Ok((elems, span))
}

fn list1<'a, T>(
    lexer: &mut Lexer<'a>,
    mut elem_gen: impl FnMut(&mut Lexer<'a>) -> Result<'a, T>,
    terminal: TokenKind,
) -> Result<'a, (Vec<T>, Span<'a>)> {
    let mut elems = vec![elem_gen(lexer)?];
    let span = if let Some(span) = lexer.next_as(terminal)? {
        span
    } else {
        lexer.expect_as(TokenKind::Comma)?;
        loop {
            if let Some(span) = lexer.next_as(terminal)? {
                break span;
            }
            elems.push(elem_gen(lexer)?);
            match lexer.next_as(TokenKind::Comma)? {
                Some(_) => (),
                None => {
                    break lexer.expect_as(terminal)?;
                }
            }
        }
    };
    Ok((elems, span))
}

fn block<'a>(lexer: &mut Lexer<'a>) -> Result<'a, Block<'a>> {
    let _ = lexer.expect_as(TokenKind::LBrace)?;
    block_after_l_paren(lexer)
}

fn block_after_l_paren<'a>(lexer: &mut Lexer<'a>) -> Result<'a, Block<'a>> {
    let mut stmts = Vec::new();
    let _ = loop {
        match lexer.next_as(TokenKind::RBrace)? {
            Some(r_brace) => break r_brace,
            None => stmts.push(stmt(lexer)?),
        }
    };
    Ok(Block { stmts })
}
