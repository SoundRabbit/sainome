#[macro_use]
extern crate lalrpop_util;

mod ast;
lalrpop_mod!(syntax);

use ast::*;
use std::collections::HashMap;
use std::rc::Rc;

struct RunTime {
    stack: Vec<HashMap<Rc<String>, Value>>,
    parser: syntax::ExprParser,
}

#[derive(Clone)]
enum Value {
    None,
    Bool(bool),
    Str(String),
    Num(f64),
    List(Vec<Value>),
    Fnc(Rc<String>, Rc<FncDef>),
}

impl RunTime {
    pub fn new() -> Self {
        Self {
            stack: vec![HashMap::new()],
            parser: syntax::ExprParser::new(),
        }
    }

    pub fn exec(&mut self, code: &str) -> Option<Value> {
        let ast = self.parser.parse(code).ok();
        let value = if let Some(expr) = &ast {
            self.exec_expr(expr)
        } else {
            None
        };
        value
    }

    fn exec_expr(&mut self, expr: &Expr) -> Option<Value> {
        match expr {
            Expr::Assign(ident, fnc_def) => {
                let len = self.stack.len();
                let value = self.exec_func_def(fnc_def);
                if let Some(value) = value {
                    self.stack[len - 1].insert(Rc::clone(ident), value);
                }
                Some(Value::None)
            }
            Expr::FncDef(fnc_def) => self.exec_func_def(fnc_def),
        }
    }

    fn exec_func_def(&mut self, fnc_def: &FncDef) -> Option<Value> {
        match fnc_def {
            FncDef::FncDef(arg, implement) => {
                Some(Value::Fnc(Rc::clone(arg), Rc::clone(implement)))
            }
            FncDef::Expr0(expr_0) => self.exec_expr_0(expr_0),
        }
    }

    fn exec_expr_0(&mut self, expr_0: &Expr0) -> Option<Value> {
        match expr_0 {
            Expr0::Expr0(left, right, op_code) => {
                let right = self.exec_expr_1(right);
                if let Some(right) = right {
                    match op_code {
                        OpCode0::At => match right {
                            Value::Fnc(a, i) => {
                                let mut value = vec![];
                                loop {
                                    let left = self.exec_expr_0(left);
                                    let f = left
                                        .and_then(|left| {
                                            value.push(left.clone());
                                            self.call_fnc((a, &i), left)
                                        })
                                        .and_then(|f| match f {
                                            Value::Bool(f) => Some(f),
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
                                Some(Value::List(value))
                            }
                            Value::Num(n) => {
                                let mut value = vec![];
                                loop {
                                    let left = self.exec_expr_0(left);
                                    let f = left.and_then(|left| {
                                        value.push(left.clone());
                                        match left {
                                            Value::Num(m) => Some(m >= n),
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
                                Some(Value::List(value))
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

    fn exec_expr_1(&mut self, expr_1: &Expr1) -> Option<Value> {
        match expr_1 {
            Expr1::Expr1(left, right, op_code) => {
                let left = self.exec_expr_1(left);
                let right = self.exec_expr_2(right);
                if let (Some(left), Some(right)) = (left, right) {
                    if let (Value::Bool(left), Value::Bool(right)) = (left, right) {
                        Some(Self::exec_expr_1_bool(left, right, op_code))
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

    fn exec_expr_2(&mut self, expr_2: &Expr2) -> Option<Value> {
        unimplemented!();
    }

    fn exec_expr_3(&mut self, expr_3: &Expr3) -> Option<Value> {
        unimplemented!();
    }

    fn exec_expr_4(&mut self, expr_4: &Expr4) -> Option<Value> {
        unimplemented!();
    }

    fn exec_expr_1_bool(left: bool, right: bool, op_code: &OpCode1) -> Value {
        match op_code {
            OpCode1::Equal => Value::Bool(left == right),
            OpCode1::NotEq => Value::Bool(left != right),
            OpCode1::EqGreaterThan => Value::Bool(left >= right),
            OpCode1::EqLessThan => Value::Bool(left <= right),
            OpCode1::GreaterThan => Value::Bool(left > right),
            OpCode1::LessThan => Value::Bool(left < right),
        }
    }

    fn call_fnc(&mut self, fnc: (Rc<String>, &FncDef), arg: Value) -> Option<Value> {
        let mut vars = HashMap::new();
        vars.insert(fnc.0, arg);
        self.stack.push(vars);
        self.exec_func_def(fnc.1)
    }
}
