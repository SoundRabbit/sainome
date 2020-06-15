use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Assign(Rc<String>, FncChain),
    Branch(Branch),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Branch {
    Branch(FncChain, Box<Branch>, Box<Branch>),
    FncChain(FncChain),
}

#[derive(Debug, PartialEq, Clone)]
pub enum FncChain {
    FncChain(Box<FncChain>, FncDef),
    FncDef(FncDef),
}

#[derive(Debug, PartialEq, Clone)]
pub enum FncDef {
    FncDef(Rc<String>, Rc<FncDef>),
    Expr0(Expr0),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr0 {
    Expr0(Box<Expr0>, Expr1, OpCode0),
    Expr1(Expr1),
}

#[derive(Debug, PartialEq, Clone)]
pub enum OpCode0 {
    At,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr1 {
    Expr1(Box<Expr1>, Expr2, OpCode1),
    Expr2(Expr2),
}

#[derive(Debug, PartialEq, Clone)]
pub enum OpCode1 {
    Equal,
    NotEq,
    GreaterThan,
    LessThan,
    EqGreaterThan,
    EqLessThan,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr2 {
    Expr2(Box<Expr2>, Expr3, OpCode2),
    Expr3(Expr3),
}

#[derive(Debug, PartialEq, Clone)]
pub enum OpCode2 {
    Add,
    Sub,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr3 {
    Expr3(Box<Expr3>, Expr4, OpCode3),
    Expr4(Expr4),
}

#[derive(Debug, PartialEq, Clone)]
pub enum OpCode3 {
    Multi,
    Div,
    Mod,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr4 {
    Expr4(Box<Expr4>, Term, OpCode4),
    Unary(Unary),
}

#[derive(Debug, PartialEq, Clone)]
pub enum OpCode4 {
    Dice,
    Bice,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Unary {
    Plus(FncCall),
    Minus(FncCall),
    FncCall(FncCall),
}

#[derive(Debug, PartialEq, Clone)]
pub enum FncCall {
    FncCall(Box<FncCall>, Term),
    Term(Term),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Term {
    Literal(Literal),
    List(Vec<Expr>),
    Expr(Vec<Expr>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Num(f64),
    Str(Rc<String>),
    Ident(Rc<String>),
    Ref(Vec<String>),
}
