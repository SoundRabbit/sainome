#[derive(Debug, PartialEq)]
pub enum Expr0 {
    Expr0 {
        left: Box<Expr0>,
        right: Expr1,
        op_code: OpCode0,
    },
    Expr1(Expr1),
}

#[derive(Debug, PartialEq)]
pub enum OpCode0 {
    Equal,
    NotEq,
    GreaterThan,
    LessThan,
    EqGreaterThan,
    EqLessThan,
}

#[derive(Debug, PartialEq)]
pub enum Expr1 {
    Expr1 {
        left: Box<Expr1>,
        right: Expr2,
        op_code: OpCode1,
    },
    Expr2(Expr2),
}

#[derive(Debug, PartialEq)]
pub enum OpCode1 {
    Add,
    Sub,
}

#[derive(Debug, PartialEq)]
pub enum Expr2 {
    Expr2 {
        left: Box<Expr2>,
        right: Term,
        op_code: OpCode2,
    },
    Term(Term),
}

#[derive(Debug, PartialEq)]
pub enum OpCode2 {
    Multi,
    Div,
    Mod,
}

#[derive(Debug, PartialEq)]
pub enum Term {
    Literal(Literal),
    Expr0(Box<Expr0>),
}

#[derive(Debug, PartialEq)]
pub enum Literal {
    Num(f64),
    Str(String),
    Ident(String),
    Ref(Vec<String>),
}
