#[macro_use]
extern crate lalrpop_util;

mod ast;

lalrpop_mod!(pub syntax);

use ast::*;

#[test]
fn parse_num_literal_by_ok() {
    assert_eq!(
        syntax::Expr0Parser::new().parse("22").ok(),
        Some(Expr0::Expr1(Expr1::Expr2(Expr2::Term(Term::Literal(
            Literal::Num(22.0)
        )))))
    );
}

#[test]
fn parse_num_literal_by_ok_int() {
    assert_eq!(
        syntax::Expr0Parser::new().parse("22").ok(),
        Some(Expr0::Expr1(Expr1::Expr2(Expr2::Term(Term::Literal(
            Literal::Num(22.0)
        )))))
    );
}

#[test]
fn parse_num_literal_by_ok_float() {
    assert_eq!(
        syntax::Expr0Parser::new().parse("22.01").ok(),
        Some(Expr0::Expr1(Expr1::Expr2(Expr2::Term(Term::Literal(
            Literal::Num(22.01)
        )))))
    );
}

#[test]
fn parse_num_literal_by_err_0() {
    assert_eq!(syntax::Expr0Parser::new().parse("2.").ok(), None);
}

#[test]
fn parse_num_literal_by_err_1() {
    assert_eq!(syntax::Expr0Parser::new().parse("#4").ok(), None);
}
