#[derive(Debug)]
pub enum Expr {
    Expr {
        left: Box<Expr>,
        right: Factor,
        opr: Opr,
    },
    Factor(Factor),
}

#[derive(Debug)]
pub enum Factor {
    Factor {
        left: Box<Factor>,
        right: Term,
        opr: Opr,
    },
    Term(Term),
}

#[derive(Debug)]
pub enum Term {
    Num(i32),
    Expr(Box<Expr>),
}

#[derive(Debug)]
pub enum Opr {
    Add,
    Sub,
    Multi,
    Div,
}
