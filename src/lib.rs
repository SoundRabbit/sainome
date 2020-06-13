#[macro_use]
extern crate lalrpop_util;

mod ast;

lalrpop_mod!(pub syntax);

use ast::*;

fn expr0_literal(literal: Literal) -> Expr0 {
    Expr0::Expr1(Expr1::Expr2(Expr2::Expr3(Expr3::Term(
        (Term::Literal(literal)),
    ))))
}

#[test]
fn parse_num_literal_by_ok_int() {
    assert_eq!(
        syntax::Expr0Parser::new().parse("22").ok(),
        Some(expr0_literal(Literal::Num(22.0)))
    );
}

#[test]
fn parse_num_literal_by_ok_float() {
    assert_eq!(
        syntax::Expr0Parser::new().parse("22.01").ok(),
        Some(expr0_literal(Literal::Num(22.01)))
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

#[test]
fn parse_list_term_by_ok_empty() {
    assert_eq!(
        syntax::Expr0Parser::new().parse("[ ]").ok(),
        Some(Expr0::Expr1(Expr1::Expr2(Expr2::Expr3(Expr3::Term(
            Term::List(vec![])
        )))))
    );
}

#[test]
fn parse_list_term_by_ok_expr0() {
    assert_eq!(
        syntax::Expr0Parser::new().parse("[1]").ok(),
        Some(Expr0::Expr1(Expr1::Expr2(Expr2::Expr3(Expr3::Term(
            Term::List(vec![expr0_literal(Literal::Num(1.0))])
        )))))
    );
}

#[test]
fn parse_list_term_by_ok_list_1() {
    assert_eq!(
        syntax::Expr0Parser::new().parse("[1,2,3]").ok(),
        Some(Expr0::Expr1(Expr1::Expr2(Expr2::Expr3(Expr3::Term(
            Term::List(vec![
                expr0_literal(Literal::Num(1.0)),
                expr0_literal(Literal::Num(2.0)),
                expr0_literal(Literal::Num(3.0))
            ])
        )))))
    );
}

#[test]
fn parse_list_term_by_ok_list_2() {
    assert_eq!(
        syntax::Expr0Parser::new().parse("[1,2,3,]").ok(),
        Some(Expr0::Expr1(Expr1::Expr2(Expr2::Expr3(Expr3::Term(
            Term::List(vec![
                expr0_literal(Literal::Num(1.0)),
                expr0_literal(Literal::Num(2.0)),
                expr0_literal(Literal::Num(3.0))
            ])
        )))))
    );
}
