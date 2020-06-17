use crate::ast::*;
use peg;
use std::rc::Rc;

peg::parser! {
    pub grammar parse() for str {
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

        pub rule reference() -> Vec<String>
            = "{" x:$(!"."*) ** "." "}" { x.iter().map(|x| x.to_string()).collect() }

        pub rule literal() -> Literal = precedence! {
            x:num() { Literal::Num(x) }
            x:ident() {Literal::Ident(Rc::new(x))}
            x:string() {Literal::Str(Rc::new(x))}
            x:reference() {Literal::Ref(x)}
        }

        pub rule term() -> Term = precedence! {
            x:literal() { Term::Literal(x) }
            --
            "(" x:expr() ** ";" ")" { Term::Expr(x) }
            --
            "[" x:expr() ** "," "]" { Term::List(x) }
        }

        pub rule reducer() -> Reducer = precedence! {
            x:(@) "#>" y:@ { Reducer::RLeft(Box::new(x), Box::new(y)) }
            x:(@) "<#" y:@ { Reducer::RRight(Box::new(x), Box::new(y)) }
            --
            x:term() { Reducer::Term(x) }
        }

        pub rule fnc_call() -> FncCall = precedence! {
            x:(@) "." y:@ { FncCall::FncCall(Box::new(x), Box::new(y)) }
            --
            x:reducer() { FncCall::Reducer(x) }
        }

        pub rule unary() -> Unary = precedence! {
            "+" x:fnc_call() { Unary::Plus(x) }
            "-" x:fnc_call() { Unary::Minus(x) }
            --
            x:fnc_call() {Unary::FncCall(x)}
        }

        pub rule expr_4() -> Expr4 = precedence! {
            x:(@) "d" y:@ { Expr4::Expr4(Box::new(x), Box::new(y), OpCode4::SDice) }
            x:(@) "D" y:@ { Expr4::Expr4(Box::new(x), Box::new(y), OpCode4::SDice) }
            x:(@) "b" y:@ { Expr4::Expr4(Box::new(x), Box::new(y), OpCode4::LDice) }
            x:(@) "B" y:@ { Expr4::Expr4(Box::new(x), Box::new(y), OpCode4::LDice) }
            --
            x:unary() { Expr4::Unary(x) }
        }

        pub rule expr_3() -> Expr3 = precedence! {
            x:(@) "*" y:@ { Expr3::Expr3(Box::new(x), Box::new(y), OpCode3::Multi) }
            x:(@) "/" y:@ { Expr3::Expr3(Box::new(x), Box::new(y), OpCode3::Div) }
            x:(@) "%" y:@ { Expr3::Expr3(Box::new(x), Box::new(y), OpCode3::Mod) }
            --
            x:expr_4() { Expr3::Expr4(x) }
        }

        pub rule expr_2() -> Expr2 = precedence! {
            x:(@) "+" y:@ { Expr2::Expr2(Box::new(x), Box::new(y), OpCode2::Add) }
            x:(@) "-" y:@ { Expr2::Expr2(Box::new(x), Box::new(y), OpCode2::Sub) }
            --
            x:expr_3() { Expr2::Expr3(x) }
        }

        pub rule expr_1() -> Expr1 = precedence! {
            x:(@) "==" y:@ { Expr1::Expr1(Box::new(x), Box::new(y), OpCode1::Equal) }
            x:(@) "!=" y:@ { Expr1::Expr1(Box::new(x), Box::new(y), OpCode1::NotEq) }
            x:(@) ">=" y:@ { Expr1::Expr1(Box::new(x), Box::new(y), OpCode1::EqGreaterThan) }
            x:(@) "<=" y:@ { Expr1::Expr1(Box::new(x), Box::new(y), OpCode1::EqLessThan) }
            x:(@) ">" y:@ { Expr1::Expr1(Box::new(x), Box::new(y), OpCode1::GreaterThan) }
            x:(@) "<" y:@ { Expr1::Expr1(Box::new(x), Box::new(y), OpCode1::LessThan) }
            --
            x:expr_2() { Expr1::Expr2(x) }
        }

        pub rule expr_0() -> Expr0 = precedence! {
            x:(@) "@" y:@ { Expr0::Expr0(Box::new(x), Box::new(y), OpCode0::At) }
            --
            x:expr_1() { Expr0::Expr1(x) }
        }

        pub rule fnc_def() -> FncDef = precedence! {
            "\\" x:ident() "." y:fnc_def() { FncDef::FncDef(Rc::new(x), Rc::new(y)) }
            --
            x:expr_0() { FncDef::Expr0(x) }
        }

        pub rule fnc_chain() -> FncChain = precedence! {
            x:(@) ">>" y:@ { FncChain::FncChain(Box::new(x), Box::new(y)) }
            --
            x:fnc_def() { FncChain::FncDef(x) }
        }

        pub rule branch() -> Branch = precedence! {
            "if" x: fnc_chain() "=>" y:branch() "else" z:branch() {Branch::Branch(x, Box::new(y), Box::new(z))}
            --
            x:fnc_chain() { Branch::FncChain(x) }
        }

        pub rule expr() -> Expr = precedence! {
            x:ident() ":=" y:branch() { Expr::Assign(Rc::new(x), y) }
            --
            x:branch() { Expr::Branch(x) }
        }
    }
}
