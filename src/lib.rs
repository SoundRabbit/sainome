#[macro_use]
extern crate lalrpop_util;

mod ast;

lalrpop_mod!(pub syntax);

#[test]
fn calculator1() {}
