#[macro_use]
extern crate lalrpop_util;

mod ast;
lalrpop_mod!(syntax);

use ast::*;
use std::collections::HashMap;
use std::rc::Rc;

struct RunTime<'a> {
    stack: Vec<HashMap<Rc<String>, Rc<Value>>>,
    parser: syntax::ExprParser,
    rand: Box<dyn FnMut(u32) -> u32 + 'a>,
}

#[derive(Debug, PartialEq)]
enum Value {
    None,
    Bool(bool),
    Str(Rc<String>),
    Num(f64),
    List(Vec<Rc<Value>>),
    Fnc(Rc<String>, Rc<FncDef>),
}

impl<'a> RunTime<'a> {
    pub fn new(rand: impl FnMut(u32) -> u32 + 'a) -> Self {
        Self {
            stack: vec![HashMap::new()],
            parser: syntax::ExprParser::new(),
            rand: Box::new(rand),
        }
    }

    pub fn exec(&mut self, code: &str) -> Option<Rc<Value>> {
        let ast = self.parser.parse(code).ok();
        let value = if let Some(expr) = &ast {
            self.exec_expr(expr)
        } else {
            None
        };
        value
    }

    fn exec_expr(&mut self, expr: &Expr) -> Option<Rc<Value>> {
        match expr {
            Expr::Assign(ident, fnc_def) => {
                let len = self.stack.len();
                let value = self.exec_func_def(fnc_def);
                if let Some(value) = value {
                    self.stack[len - 1].insert(Rc::clone(ident), Rc::clone(&value));
                }
                Some(Rc::new(Value::None))
            }
            Expr::FncDef(fnc_def) => self.exec_func_def(fnc_def),
        }
    }

    fn exec_func_def(&mut self, fnc_def: &FncDef) -> Option<Rc<Value>> {
        match fnc_def {
            FncDef::FncDef(arg, implement) => {
                Some(Rc::new(Value::Fnc(Rc::clone(arg), Rc::clone(implement))))
            }
            FncDef::Expr0(expr_0) => self.exec_expr_0(expr_0),
        }
    }

    fn exec_expr_0(&mut self, expr_0: &Expr0) -> Option<Rc<Value>> {
        match expr_0 {
            Expr0::Expr0(left, right, op_code) => {
                let right = self.exec_expr_1(right);
                if let Some(right) = right {
                    match op_code {
                        OpCode0::At => match right.as_ref() {
                            Value::Fnc(a, i) => {
                                let mut value = vec![];
                                loop {
                                    let left = self.exec_expr_0(left);
                                    let f = left
                                        .and_then(|left| {
                                            value.push(Rc::clone(&left));
                                            self.call_fnc((Rc::clone(&a), &i), Rc::clone(&left))
                                        })
                                        .and_then(|f| match f.as_ref() {
                                            Value::Bool(f) => Some(*f),
                                            _ => None,
                                        });
                                    if let Some(f) = f {
                                        if !f {
                                            break;
                                        }
                                    } else {
                                        return None;
                                    }
                                }
                                Some(Rc::new(Value::List(value)))
                            }
                            Value::Num(n) => {
                                let mut value = vec![];
                                loop {
                                    let left = self.exec_expr_0(left);
                                    let f = left.and_then(|left| {
                                        value.push(Rc::clone(&left));
                                        match left.as_ref() {
                                            Value::Num(m) => Some(*m >= *n),
                                            _ => None,
                                        }
                                    });
                                    if let Some(f) = f {
                                        if !f {
                                            break;
                                        }
                                    } else {
                                        return None;
                                    }
                                }
                                Some(Rc::new(Value::List(value)))
                            }
                            _ => None,
                        },
                    }
                } else {
                    None
                }
            }
            Expr0::Expr1(expr_1) => self.exec_expr_1(expr_1),
        }
    }

    fn exec_expr_1(&mut self, expr_1: &Expr1) -> Option<Rc<Value>> {
        match expr_1 {
            Expr1::Expr1(left, right, op_code) => {
                let left = self.exec_expr_1(left);
                let right = self.exec_expr_2(right);
                if let (Some(left), Some(right)) = (left, right) {
                    let (left, right) = (left.as_ref(), right.as_ref());
                    if let (Value::Bool(left), Value::Bool(right)) = (left, right) {
                        Self::exec_expr_1_bool(*left, *right, op_code)
                    } else if let (Value::Str(left), Value::Str(right)) = (left, right) {
                        Self::exec_expr_1_str(&left, &right, op_code)
                    } else if let (Value::Num(left), Value::Num(right)) = (left, right) {
                        Self::exec_expr_1_num(*left, *right, op_code)
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            Expr1::Expr2(expr_2) => self.exec_expr_2(expr_2),
        }
    }

    fn exec_expr_2(&mut self, expr_2: &Expr2) -> Option<Rc<Value>> {
        match expr_2 {
            Expr2::Expr2(left, right, op_code) => {
                let left = self.exec_expr_2(left);
                let right = self.exec_expr_3(right);
                if let (Some(left), Some(right)) = (left, right) {
                    let (left, right) = (left.as_ref(), right.as_ref());
                    if let (Value::Bool(left), Value::Bool(right)) = (left, right) {
                        Self::exec_expr_2_bool(*left, *right, op_code)
                    } else if let (Value::Str(left), Value::Str(right)) = (left, right) {
                        Self::exec_expr_2_str(&left, &right, op_code)
                    } else if let (Value::Num(left), Value::Num(right)) = (left, right) {
                        Self::exec_expr_2_num(*left, *right, op_code)
                    } else if let (Value::List(left), Value::List(right)) = (left, right) {
                        Self::exec_expr_2_list(&left, &right, op_code)
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            Expr2::Expr3(expr_3) => self.exec_expr_3(expr_3),
        }
    }

    fn exec_expr_3(&mut self, expr_3: &Expr3) -> Option<Rc<Value>> {
        match expr_3 {
            Expr3::Expr3(left, right, op_code) => {
                let left = self.exec_expr_3(left);
                let right = self.exec_expr_4(right);
                if let (Some(left), Some(right)) = (&left, &right) {
                    let (left, right) = (left.as_ref(), right.as_ref());
                    if let (Value::Bool(left), Value::Bool(right)) = (left, right) {
                        Self::exec_expr_3_bool(*left, *right, op_code)
                    } else if let (Value::Num(left), Value::Num(right)) = (left, right) {
                        Self::exec_expr_3_num(*left, *right, op_code)
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            Expr3::Expr4(expr_4) => self.exec_expr_4(expr_4),
        }
    }

    fn exec_expr_4(&mut self, expr_4: &Expr4) -> Option<Rc<Value>> {
        match expr_4 {
            Expr4::Expr4(left, right, op_code) => {
                let left = self.exec_expr_4(left);
                let right = self.exec_term(right);
                if let (Some(left), Some(right)) = (left, right) {
                    let (left, right) = (left.as_ref(), right.as_ref());
                    if let (Value::Num(left), Value::Num(right)) = (left, right) {
                        self.exec_expr_4_num(*left, *right, op_code)
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            Expr4::Term(term) => self.exec_term(term),
        }
    }

    fn exec_term(&mut self, term: &Term) -> Option<Rc<Value>> {
        match term {
            Term::Literal(literal) => self.exec_literal(literal),
            Term::List(list) => {
                let mut values = vec![];
                for item in list {
                    if let Some(value) = self.exec_expr(item) {
                        values.push(value);
                    } else {
                        return None;
                    }
                }
                Some(Rc::new(Value::List(values)))
            }
            Term::Expr(exprs) => {
                self.stack.push(HashMap::new());
                let mut res = None;
                for expr in exprs {
                    res = self.exec_expr(expr);
                }
                self.stack.pop();
                res
            }
        }
    }

    fn exec_literal(&mut self, literal: &Literal) -> Option<Rc<Value>> {
        match literal {
            Literal::Ident(ident) => {
                let ident = Rc::clone(&ident);
                if let Some(value) = self.stack.iter().rev().find_map(|x| x.get(&ident)) {
                    Some(Rc::clone(value))
                } else {
                    Some(Rc::new(Value::None))
                }
            }
            Literal::Num(num) => Some(Rc::new(Value::Num(*num))),
            Literal::Str(str) => Some(Rc::new(Value::Str(Rc::clone(str)))),
            _ => Some(Rc::new(Value::None)),
        }
    }

    fn call_fnc(&mut self, fnc: (Rc<String>, &FncDef), arg: Rc<Value>) -> Option<Rc<Value>> {
        let mut vars = HashMap::new();
        vars.insert(fnc.0, arg);
        self.stack.push(vars);
        let res = self.exec_func_def(fnc.1);
        self.stack.pop();
        res
    }

    fn exec_expr_1_bool(left: bool, right: bool, op_code: &OpCode1) -> Option<Rc<Value>> {
        match op_code {
            OpCode1::Equal => Some(Rc::new(Value::Bool(left == right))),
            OpCode1::NotEq => Some(Rc::new(Value::Bool(left != right))),
            OpCode1::EqGreaterThan => Some(Rc::new(Value::Bool(left >= right))),
            OpCode1::EqLessThan => Some(Rc::new(Value::Bool(left <= right))),
            OpCode1::GreaterThan => Some(Rc::new(Value::Bool(left > right))),
            OpCode1::LessThan => Some(Rc::new(Value::Bool(left < right))),
        }
    }

    fn exec_expr_1_str(left: &String, right: &String, op_code: &OpCode1) -> Option<Rc<Value>> {
        match op_code {
            OpCode1::Equal => Some(Rc::new(Value::Bool(left == right))),
            OpCode1::NotEq => Some(Rc::new(Value::Bool(left != right))),
            OpCode1::EqGreaterThan => Some(Rc::new(Value::Bool(left >= right))),
            OpCode1::EqLessThan => Some(Rc::new(Value::Bool(left <= right))),
            OpCode1::GreaterThan => Some(Rc::new(Value::Bool(left > right))),
            OpCode1::LessThan => Some(Rc::new(Value::Bool(left < right))),
        }
    }

    fn exec_expr_1_num(left: f64, right: f64, op_code: &OpCode1) -> Option<Rc<Value>> {
        match op_code {
            OpCode1::Equal => Some(Rc::new(Value::Bool(left == right))),
            OpCode1::NotEq => Some(Rc::new(Value::Bool(left != right))),
            OpCode1::EqGreaterThan => Some(Rc::new(Value::Bool(left >= right))),
            OpCode1::EqLessThan => Some(Rc::new(Value::Bool(left <= right))),
            OpCode1::GreaterThan => Some(Rc::new(Value::Bool(left > right))),
            OpCode1::LessThan => Some(Rc::new(Value::Bool(left < right))),
        }
    }

    fn exec_expr_2_bool(left: bool, right: bool, op_code: &OpCode2) -> Option<Rc<Value>> {
        match op_code {
            OpCode2::Add => Some(Rc::new(Value::Bool(left || right))),
            _ => None,
        }
    }

    fn exec_expr_2_str(left: &String, right: &String, op_code: &OpCode2) -> Option<Rc<Value>> {
        match op_code {
            OpCode2::Add => Some(Rc::new(Value::Str(Rc::new(format!("{}{}", left, right))))),
            _ => None,
        }
    }

    fn exec_expr_2_num(left: f64, right: f64, op_code: &OpCode2) -> Option<Rc<Value>> {
        match op_code {
            OpCode2::Add => Some(Rc::new(Value::Num(left + right))),
            OpCode2::Sub => Some(Rc::new(Value::Num(left - right))),
        }
    }

    fn exec_expr_2_list(
        left: &Vec<Rc<Value>>,
        right: &Vec<Rc<Value>>,
        op_code: &OpCode2,
    ) -> Option<Rc<Value>> {
        match op_code {
            OpCode2::Add => Some(Rc::new(Value::List({
                let mut res = vec![];
                for i in left {
                    res.push(Rc::clone(i));
                }
                for i in right {
                    res.push(Rc::clone(i));
                }
                res
            }))),
            OpCode2::Sub => None,
        }
    }

    fn exec_expr_3_bool(left: bool, right: bool, op_code: &OpCode3) -> Option<Rc<Value>> {
        match op_code {
            OpCode3::Multi => Some(Rc::new(Value::Bool(left && right))),
            _ => None,
        }
    }

    fn exec_expr_3_num(left: f64, right: f64, op_code: &OpCode3) -> Option<Rc<Value>> {
        match op_code {
            OpCode3::Multi => Some(Rc::new(Value::Num(left * right))),
            OpCode3::Div => Some(Rc::new(Value::Num(left / right))),
            OpCode3::Mod => Some(Rc::new(Value::Num(left % right))),
        }
    }

    fn exec_expr_4_num(&mut self, left: f64, right: f64, op_code: &OpCode4) -> Option<Rc<Value>> {
        match op_code {
            OpCode4::Dice => {
                let num = left.floor() as usize;
                let a = right.floor() as u32;
                let mut res = 0;
                for _ in 0..num {
                    res += (self.rand)(a) + 1;
                }
                Some(Rc::new(Value::Num(res as f64)))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rand::prelude::*;

    #[test]
    fn it_works() {
        assert_eq!(4, 2 + 2);
    }

    #[test]
    fn addition() {
        let mut rng = rand::thread_rng();
        let mut run_time = RunTime::new(move |x| rng.gen::<u32>() % x);
        let result = run_time.exec("1+1");
        assert_eq!(result, Some(Rc::new(Value::Num(2.0))));
    }
}
