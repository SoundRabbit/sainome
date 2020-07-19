use super::ast::*;
use peg;
use std::rc::Rc;

peg::parser! {
    pub grammar parse() for str {
        pub rule d() = [' ' | '\n' | '\t']*

        pub rule dlm() = [' ' | '\n' | '\t']+

        pub rule num() -> f64
            = x:$(['0'..='9']+("."['0'..='9']+)?) { x.parse().unwrap() }

        pub rule ident() -> String
            = x:$(['A'..='Z' | 'a'..='z']+) { x.to_string() }

        pub rule zero_len_str() -> String
            = "\"" "\"" { "".to_string() }

        pub rule len_str() -> String
            = "\"" xs:$(([_]!"\"")*) x:$([_]) "\"" { xs.to_string() + x }

        pub rule string() -> String
            = x:zero_len_str() / x:len_str() { x }

        pub rule reference_item() -> String
            = xs:$(([_]!['.' | '}'])*) x:$([_]) { xs.to_string() + x }

        pub rule reference() -> Vec<String>
            = "{" x:reference_item() ** "." "}" { x.iter().map(|x| x.to_string()).collect() }

        pub rule literal() -> Literal = precedence! {
            x:num() { Literal::Num(x) }
            x:ident() {Literal::Ident(Rc::new(x))}
            x:string() {Literal::Str(Rc::new(x))}
            x:reference() {Literal::Ref(x)}
        }

        pub rule term() -> Term = precedence! {
            x:literal() { Term::Literal(x) }
            --
            "(" d() x:expr() ** (d() ";" d()) d() ")" { Term::Expr(x) }
            --
            "[" d() x:expr() ** (d() "," d()) d() "]" { Term::List(x) }
        }

        pub rule reducer() -> Reducer = precedence! {
            x:(@) d() "#>" d() y:@ { Reducer::RLeft(Box::new(x), Box::new(y)) }
            x:(@) d() "<#" d() y:@ { Reducer::RRight(Box::new(x), Box::new(y)) }
            --
            x:term() { Reducer::Term(x) }
        }

        pub rule fnc_call() -> FncCall = precedence! {
            x:(@) d() "." d() y:@ { FncCall::FncCall(Box::new(x), Box::new(y)) }
            --
            x:reducer() { FncCall::Reducer(x) }
        }

        pub rule unary() -> Unary = precedence! {
            "+" d() x:fnc_call() { Unary::Plus(x) }
            "-" d() x:fnc_call() { Unary::Minus(x) }
            --
            x:fnc_call() {Unary::FncCall(x)}
        }

        pub rule expr_4() -> Expr4 = precedence! {
            x:(@) d() "d" d() y:@ { Expr4::Expr4(Box::new(x), Box::new(y), OpCode4::SDice) }
            x:(@) d() "D" d() y:@ { Expr4::Expr4(Box::new(x), Box::new(y), OpCode4::SDice) }
            x:(@) d() "b" d() y:@ { Expr4::Expr4(Box::new(x), Box::new(y), OpCode4::LDice) }
            x:(@) d() "B" d() y:@ { Expr4::Expr4(Box::new(x), Box::new(y), OpCode4::LDice) }
            --
            x:unary() { Expr4::Unary(x) }
        }

        pub rule expr_3() -> Expr3 = precedence! {
            x:(@) d() "*" d() y:@ { Expr3::Expr3(Box::new(x), Box::new(y), OpCode3::Multi) }
            x:(@) d() "/" d() y:@ { Expr3::Expr3(Box::new(x), Box::new(y), OpCode3::Div) }
            x:(@) d() "%" d() y:@ { Expr3::Expr3(Box::new(x), Box::new(y), OpCode3::Mod) }
            --
            x:expr_4() { Expr3::Expr4(x) }
        }

        pub rule expr_2() -> Expr2 = precedence! {
            x:(@) d() "+" d() y:@ { Expr2::Expr2(Box::new(x), Box::new(y), OpCode2::Add) }
            x:(@) d() "-" d() y:@ { Expr2::Expr2(Box::new(x), Box::new(y), OpCode2::Sub) }
            --
            x:expr_3() { Expr2::Expr3(x) }
        }

        pub rule expr_1() -> Expr1 = precedence! {
            x:(@) d() "==" d() y:@ { Expr1::Expr1(Box::new(x), Box::new(y), OpCode1::Equal) }
            x:(@) d() "!=" d() y:@ { Expr1::Expr1(Box::new(x), Box::new(y), OpCode1::NotEq) }
            x:(@) d() ">=" d() y:@ { Expr1::Expr1(Box::new(x), Box::new(y), OpCode1::EqGreaterThan) }
            x:(@) d() "<=" d() y:@ { Expr1::Expr1(Box::new(x), Box::new(y), OpCode1::EqLessThan) }
            x:(@) d() ">" d() y:@ { Expr1::Expr1(Box::new(x), Box::new(y), OpCode1::GreaterThan) }
            x:(@) d() "<" d() y:@ { Expr1::Expr1(Box::new(x), Box::new(y), OpCode1::LessThan) }
            --
            x:expr_2() { Expr1::Expr2(x) }
        }

        pub rule expr_0() -> Expr0 = precedence! {
            x:(@) d() "@" d() y:@ { Expr0::Expr0(Box::new(x), Box::new(y), OpCode0::At) }
            --
            x:expr_1() { Expr0::Expr1(x) }
        }

        pub rule fnc_def() -> FncDef = precedence! {
            "\\" d() x:ident() d() "." d() y:fnc_def() { FncDef::FncDef(Rc::new(x), Rc::new(y)) }
            --
            x:expr_0() { FncDef::Expr0(x) }
        }

        pub rule fnc_chain() -> FncChain = precedence! {
            x:(@) d() ">>" d() y:@ { FncChain::Concat(Box::new(x), Box::new(y)) }
            x:(@) d() "|>" d() y:@ { FncChain::Pipeline(Box::new(x), Box::new(y)) }
            --
            x:fnc_def() { FncChain::FncDef(x) }
        }

        pub rule branch() -> Branch = precedence! {
            "if" d() x: fnc_chain() d() "then" d() y:branch() d() "else" d() z:branch() {Branch::Branch(x, Box::new(y), Box::new(z))}
            --
            x:fnc_chain() { Branch::FncChain(x) }
        }

        pub rule expr() -> Expr = precedence! {
            x:ident() d() ":=" d() y:branch() { Expr::Assign(Rc::new(x), y) }
            --
            x:branch() { Expr::Branch(x) }
        }

        pub rule impl_exprs() -> Vec<Expr>
            = x:expr() ** (d() ";" d()) { x }

        pub rule impl_exprs_message() -> String
            = dlm() x:$([_]*) { x.to_string() }

        pub rule exprs() -> (Option<Vec<Expr>>, Option<String>)
            = x:impl_exprs()? y:impl_exprs_message()? { (x, y) }
    }
}
