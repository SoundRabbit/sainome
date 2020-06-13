use std::rc::Rc;

#[derive(Debug, PartialEq)]
pub enum Expr {
    Assign(Rc<String>, FncDef),
    FncDef(FncDef),
}

#[derive(Debug, PartialEq)]
pub enum FncDef {
    FncDef(Rc<String>, Rc<FncDef>),
    Expr0(Expr0),
}

#[derive(Debug, PartialEq)]
pub enum Expr0 {
    Expr0(Box<Expr0>, Expr1, OpCode0),
    Expr1(Expr1),
}

#[derive(Debug, PartialEq)]
pub enum OpCode0 {
    At,
}

#[derive(Debug, PartialEq)]
pub enum Expr1 {
    Expr1(Box<Expr1>, Expr2, OpCode1),
    Expr2(Expr2),
}

#[derive(Debug, PartialEq)]
pub enum OpCode1 {
    Equal,
    NotEq,
    GreaterThan,
    LessThan,
    EqGreaterThan,
    EqLessThan,
}

#[derive(Debug, PartialEq)]
pub enum Expr2 {
    Expr2(Box<Expr2>, Expr3, OpCode2),
    Expr3(Expr3),
}

#[derive(Debug, PartialEq)]
pub enum OpCode2 {
    Add,
    Sub,
}

#[derive(Debug, PartialEq)]
pub enum Expr3 {
    Expr3(Box<Expr3>, Expr4, OpCode3),
    Expr4(Expr4),
}

#[derive(Debug, PartialEq)]
pub enum OpCode3 {
    Multi,
    Div,
    Mod,
}

#[derive(Debug, PartialEq)]
pub enum Expr4 {
    Expr4(Box<Expr4>, Term, OpCode4),
    Term(Term),
}

#[derive(Debug, PartialEq)]
pub enum OpCode4 {
    Dice,
}

#[derive(Debug, PartialEq)]
pub enum Term {
    Literal(Literal),
    List(Vec<Expr>),
    Expr(Vec<Expr>),
}

#[derive(Debug, PartialEq)]
pub enum Literal {
    Num(f64),
    Str(Rc<String>),
    Ident(Rc<String>),
    Ref(Vec<String>),
}
