use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Assign(Rc<String>, Branch),
    Branch(Branch),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Branch {
    Branch(FncChain, Box<Branch>, Box<Branch>),
    FncChain(FncChain),
}

#[derive(Debug, PartialEq, Clone)]
pub enum FncChain {
    Pipeline(Box<FncChain>, Box<FncChain>),
    Concat(Box<FncChain>, Box<FncChain>),
    FncDef(FncDef),
}

#[derive(Debug, PartialEq, Clone)]
pub enum FncDef {
    FncDef(Rc<String>, Rc<FncDef>),
    Expr0(Expr0),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr0 {
    Expr0(Box<Expr0>, Box<Expr0>, OpCode0),
    Expr1(Expr1),
}

#[derive(Debug, PartialEq, Clone)]
pub enum OpCode0 {
    At,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr1 {
    Expr1(Box<Expr1>, Box<Expr1>, OpCode1),
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
    Expr2(Box<Expr2>, Box<Expr2>, OpCode2),
    Expr3(Expr3),
}

#[derive(Debug, PartialEq, Clone)]
pub enum OpCode2 {
    Add,
    Sub,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr3 {
    Expr3(Box<Expr3>, Box<Expr3>, OpCode3),
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
    Expr4(Box<Expr4>, Box<Expr4>, OpCode4),
    Unary(Unary),
}

#[derive(Debug, PartialEq, Clone)]
pub enum OpCode4 {
    SDice,
    LDice,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Unary {
    Plus(FncCall),
    Minus(FncCall),
    FncCall(FncCall),
}

#[derive(Debug, PartialEq, Clone)]
pub enum FncCall {
    FncCall(Box<FncCall>, Box<FncCall>),
    Reducer(Reducer),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Reducer {
    RLeft(Box<Reducer>, Box<Reducer>),
    RRight(Box<Reducer>, Box<Reducer>),
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
