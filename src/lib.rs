extern crate peg;

mod ast;
mod parser;

use ast::*;
use std::collections::HashMap;
use std::rc::Rc;

pub struct RunTime<'a> {
    rand: Box<dyn FnMut(u32) -> u32 + 'a>,
}

#[derive(Debug, PartialEq)]
enum Value {
    None,
    Bool(bool),
    Str(Rc<String>),
    Num(f64),
    List(Rc<Vec<Rc<Value>>>),
    Fnc(Rc<String>, Rc<FncDef>, Env),
    RLeft(Rc<Value>, Rc<Vec<Rc<Value>>>),
    RRight(Rc<Value>, Rc<Vec<Rc<Value>>>),
}

type Env = HashMap<Rc<String>, Rc<Value>>;

#[derive(Debug, PartialEq)]
pub enum ExecResult {
    None,
    Bool(bool),
    Str(Rc<String>),
    Num(f64),
    List(Vec<ExecResult>),
}

impl<'a> RunTime<'a> {
    pub fn new(rand: impl FnMut(u32) -> u32 + 'a) -> Self {
        Self {
            rand: Box::new(rand),
        }
    }

    pub fn exec(&mut self, code: &str) -> Option<ExecResult> {
        let mut ast = parser::parse::expr(code).ok();
        let value = if let Some(expr) = &mut ast {
            self.exec_expr(expr, &mut HashMap::new())
                .map(|x| ExecResult::from(x.as_ref()))
        } else {
            None
        };
        value
    }

    fn exec_expr(&mut self, expr: &mut Expr, env: &mut Env) -> Option<Rc<Value>> {
        match expr {
            Expr::Assign(ident, fnc_chain) => {
                let value = self.exec_branch(fnc_chain, env);
                if let Some(value) = value {
                    env.insert(Rc::clone(ident), Rc::clone(&value));
                }
                Some(Rc::new(Value::None))
            }
            Expr::Branch(branch) => self.exec_branch(branch, env),
        }
    }

    fn exec_branch(&mut self, branch: &mut Branch, env: &mut Env) -> Option<Rc<Value>> {
        match branch {
            Branch::Branch(c, left, right) => {
                if let Some(c) = self.exec_fnc_chain(c, env) {
                    if let Value::Bool(c) = c.as_ref() {
                        if *c {
                            self.exec_branch(left, env)
                        } else {
                            self.exec_branch(right, env)
                        }
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            Branch::FncChain(fnc_chain) => self.exec_fnc_chain(fnc_chain, env),
        }
    }

    fn exec_fnc_chain(&mut self, fnc_chain: &mut FncChain, env: &mut Env) -> Option<Rc<Value>> {
        match fnc_chain {
            FncChain::FncChain(left, right) => {
                let left = self.exec_fnc_chain(left, env);
                let right = self.exec_fnc_chain(right, env);
                if let (Some(left), Some(right)) = (left, right) {
                    self.call_like_fnc_with_value(&right, left, env)
                } else {
                    None
                }
            }
            FncChain::FncDef(fnc_def) => self.exec_fnc_def(fnc_def, env),
        }
    }

    fn exec_fnc_def(&mut self, fnc_def: &mut FncDef, env: &mut Env) -> Option<Rc<Value>> {
        match fnc_def {
            FncDef::FncDef(arg, right) => Some(Rc::new(Value::Fnc(
                Rc::clone(arg),
                Rc::clone(right),
                env.clone(),
            ))),
            FncDef::Expr0(expr_0) => self.exec_expr_0(expr_0, env),
        }
    }

    fn exec_expr_0(&mut self, expr_0: &mut Expr0, env: &mut Env) -> Option<Rc<Value>> {
        match expr_0 {
            Expr0::Expr0(left, right, op_code) => {
                let right = self.exec_expr_0(right, env);
                if let Some(right) = right {
                    match op_code {
                        OpCode0::At => match right.as_ref() {
                            Value::Fnc(a, i, e) => {
                                let mut value = vec![];
                                loop {
                                    let left = self.exec_expr_0(left, env);
                                    let f = left
                                        .and_then(|left| {
                                            value.push(Rc::clone(&left));
                                            self.call_fnc(
                                                (Rc::clone(&a), &i, &mut e.clone()),
                                                Rc::clone(&left),
                                            )
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
                                Some(Rc::new(Value::List(Rc::new(value))))
                            }
                            Value::Num(n) => {
                                let mut value = vec![];
                                loop {
                                    let left = self.exec_expr_0(left, env);
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
                                Some(Rc::new(Value::List(Rc::new(value))))
                            }
                            _ => None,
                        },
                    }
                } else {
                    None
                }
            }
            Expr0::Expr1(expr_1) => self.exec_expr_1(expr_1, env),
        }
    }

    fn exec_expr_1(&mut self, expr_1: &mut Expr1, env: &mut Env) -> Option<Rc<Value>> {
        match expr_1 {
            Expr1::Expr1(left, right, op_code) => {
                let left = self.exec_expr_1(left, env);
                let right = self.exec_expr_1(right, env);
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
            Expr1::Expr2(expr_2) => self.exec_expr_2(expr_2, env),
        }
    }

    fn exec_expr_2(&mut self, expr_2: &mut Expr2, env: &mut Env) -> Option<Rc<Value>> {
        match expr_2 {
            Expr2::Expr2(left, right, op_code) => {
                let left = self.exec_expr_2(left, env);
                let right = self.exec_expr_2(right, env);
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
            Expr2::Expr3(expr_3) => self.exec_expr_3(expr_3, env),
        }
    }

    fn exec_expr_3(&mut self, expr_3: &mut Expr3, env: &mut Env) -> Option<Rc<Value>> {
        match expr_3 {
            Expr3::Expr3(left, right, op_code) => {
                let left = self.exec_expr_3(left, env);
                let right = self.exec_expr_3(right, env);
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
            Expr3::Expr4(expr_4) => self.exec_expr_4(expr_4, env),
        }
    }

    fn exec_expr_4(&mut self, expr_4: &mut Expr4, env: &mut Env) -> Option<Rc<Value>> {
        match expr_4 {
            Expr4::Expr4(left, right, op_code) => {
                let left = self.exec_expr_4(left, env);
                let right = self.exec_expr_4(right, env);
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
            Expr4::Unary(unary) => self.exec_expr_unary(unary, env),
        }
    }

    fn exec_expr_unary(&mut self, unary: &mut Unary, env: &mut Env) -> Option<Rc<Value>> {
        match unary {
            Unary::Plus(fnc_call) => {
                if let Some(val) = self.exec_fnc_call(fnc_call, env) {
                    match val.as_ref() {
                        Value::Bool(val) => Self::exec_expr_2_bool(false, *val, &OpCode2::Add),
                        Value::List(val) => Self::exec_expr_2_list(&vec![], val, &OpCode2::Add),
                        Value::Num(val) => Self::exec_expr_2_num(0.0, *val, &OpCode2::Add),
                        Value::Str(val) => {
                            Self::exec_expr_2_str(&String::from(""), val, &OpCode2::Add)
                        }
                        Value::None => Some(Rc::new(Value::None)),
                        _ => None,
                    }
                } else {
                    None
                }
            }
            Unary::Minus(fnc_call) => {
                if let Some(val) = self.exec_fnc_call(fnc_call, env) {
                    match val.as_ref() {
                        Value::Bool(val) => Self::exec_expr_2_bool(false, *val, &OpCode2::Sub),
                        Value::List(val) => Self::exec_expr_2_list(&vec![], val, &OpCode2::Sub),
                        Value::Num(val) => Self::exec_expr_2_num(0.0, *val, &OpCode2::Sub),
                        Value::Str(val) => {
                            Self::exec_expr_2_str(&String::from(""), val, &OpCode2::Sub)
                        }
                        Value::None => Some(Rc::new(Value::None)),
                        _ => None,
                    }
                } else {
                    None
                }
            }
            Unary::FncCall(fnc_call) => self.exec_fnc_call(fnc_call, env),
        }
    }

    fn exec_fnc_call(&mut self, fnc_call: &mut FncCall, env: &mut Env) -> Option<Rc<Value>> {
        match fnc_call {
            FncCall::FncCall(fnc_call, arg) => {
                let fnc = self.exec_fnc_call(fnc_call, env);
                if let Some(fnc) = fnc {
                    let fnc = fnc.as_ref();
                    self.call_like_fnc_with_term(fnc, arg, env)
                } else {
                    None
                }
            }
            FncCall::Reducer(reducer) => self.exec_reducer(reducer, env),
        }
    }

    fn exec_reducer(&mut self, reducer: &mut Reducer, env: &mut Env) -> Option<Rc<Value>> {
        match reducer {
            Reducer::RLeft(i, lst) => {
                let i = self.exec_reducer(i, env);
                let lst = self.exec_reducer(lst, env);
                if let (Some(i), Some(lst)) = (i, lst) {
                    match lst.as_ref() {
                        Value::List(lst) => Some(Rc::new(Value::RLeft(i, Rc::clone(lst)))),
                        _ => Some(Rc::new(Value::RLeft(i, Rc::new(vec![lst])))),
                    }
                } else {
                    None
                }
            }
            Reducer::RRight(lst, i) => {
                let i = self.exec_reducer(i, env);
                let lst = self.exec_reducer(lst, env);
                if let (Some(i), Some(lst)) = (i, lst) {
                    match lst.as_ref() {
                        Value::List(lst) => Some(Rc::new(Value::RRight(i, Rc::clone(lst)))),
                        _ => Some(Rc::new(Value::RRight(i, Rc::new(vec![lst])))),
                    }
                } else {
                    None
                }
            }
            Reducer::Term(term) => self.exec_term(term, env),
        }
    }

    fn exec_term(&mut self, term: &mut Term, env: &mut Env) -> Option<Rc<Value>> {
        match term {
            Term::Literal(literal) => self.exec_literal(literal, env),
            Term::List(list) => {
                let mut values = vec![];
                for item in list {
                    if let Some(value) = self.exec_expr(item, env) {
                        values.push(value);
                    } else {
                        return None;
                    }
                }
                Some(Rc::new(Value::List(Rc::new(values))))
            }
            Term::Expr(exprs) => {
                let mut res = None;
                let mut env = env.clone();
                for expr in exprs {
                    res = self.exec_expr(expr, &mut env);
                }
                res
            }
        }
    }

    fn exec_literal(&mut self, literal: &Literal, env: &mut Env) -> Option<Rc<Value>> {
        match literal {
            Literal::Ident(ident) => {
                let ident = Rc::clone(&ident);
                if let Some(value) = env.get(&ident) {
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

    fn call_like_fnc_with_term(
        &mut self,
        fnc: &Value,
        arg: &mut FncCall,
        env: &mut Env,
    ) -> Option<Rc<Value>> {
        match fnc {
            Value::List(..)
            | Value::Str(..)
            | Value::RLeft(..)
            | Value::RRight(..)
            | Value::Fnc(..) => match self.exec_fnc_call(arg, env) {
                Some(arg) => self.call_like_fnc_with_value(&fnc, arg, env),
                None => None,
            },
            Value::Num(v) => {
                let n = v.floor() as usize;
                let mut res = vec![];
                for _ in 0..n {
                    if let Some(v) = self.exec_fnc_call(arg, env) {
                        res.push(v);
                    } else {
                        return None;
                    }
                }
                Some(Rc::new(Value::List(Rc::new(res))))
            }
            _ => None,
        }
    }

    fn call_like_fnc_with_value(
        &mut self,
        fnc: &Value,
        arg: Rc<Value>,
        env: &mut Env,
    ) -> Option<Rc<Value>> {
        match fnc {
            Value::Fnc(a, i, e) => self.call_fnc((Rc::clone(a), i, &mut e.clone()), arg),
            Value::List(vs) => match arg.as_ref() {
                Value::Num(n) => {
                    let n = n.floor();
                    let idx = if n >= 0.0 {
                        n as usize
                    } else {
                        vs.len() - (-n as usize)
                    };
                    vs.get(idx).map(|i| Rc::clone(i))
                }
                Value::Fnc(a, i, e) => {
                    let mut rs = vec![];
                    for v in vs.as_ref() {
                        if let Some(r) =
                            self.call_fnc((Rc::clone(a), i, &mut e.clone()), Rc::clone(v))
                        {
                            rs.push(r);
                        }
                    }
                    Some(Rc::new(Value::List(Rc::new(rs))))
                }
                _ => None,
            },
            Value::Str(v) => match arg.as_ref() {
                Value::Num(n) => {
                    let n = n.floor();
                    let idx = if n >= 0.0 {
                        n as usize
                    } else {
                        v.len() - (-n as usize)
                    };
                    v.as_str()
                        .chars()
                        .collect::<Vec<char>>()
                        .get(idx)
                        .map(|i| Rc::new(Value::Str(Rc::new(i.to_string()))))
                }
                _ => None,
            },
            Value::Num(v) => {
                let n = v.floor() as usize;
                let mut res = vec![];
                for _ in 0..n {
                    res.push(Rc::clone(&arg));
                }
                Some(Rc::new(Value::List(Rc::new(res))))
            }
            Value::RLeft(i, lst) => {
                let mut pre = Rc::clone(i);
                for x in lst.iter() {
                    if let Some(f) = self.call_like_fnc_with_value(&arg, Rc::clone(&pre), env) {
                        if let Some(p) = self.call_like_fnc_with_value(&f, Rc::clone(x), env) {
                            pre = p;
                        } else {
                            return None;
                        }
                    } else {
                        return None;
                    }
                }
                Some(pre)
            }
            Value::RRight(i, lst) => {
                let mut pre = Rc::clone(i);
                for x in lst.iter().rev() {
                    if let Some(f) = self.call_like_fnc_with_value(&arg, Rc::clone(&pre), env) {
                        if let Some(p) = self.call_like_fnc_with_value(&f, Rc::clone(x), env) {
                            pre = p;
                        } else {
                            return None;
                        }
                    } else {
                        return None;
                    }
                }
                Some(pre)
            }
            _ => None,
        }
    }

    fn call_fnc(
        &mut self,
        fnc: (Rc<String>, &FncDef, &mut Env),
        arg: Rc<Value>,
    ) -> Option<Rc<Value>> {
        fnc.2.insert(fnc.0, arg);
        let res = self.exec_fnc_def(&mut fnc.1.clone(), fnc.2);
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
                Rc::new(res)
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
            OpCode4::SDice => Some(Rc::new(Value::Num(self.exec_dice(left, right) as f64))),
            OpCode4::LDice => {
                let mut res = vec![];
                let num = left.floor() as usize;
                for _ in 0..num {
                    res.push(Rc::new(Value::Num(self.exec_dice(1.0, right))));
                }
                Some(Rc::new(Value::List(Rc::new(res))))
            }
        }
    }

    fn exec_dice(&mut self, num: f64, a: f64) -> f64 {
        let num = num.floor() as usize;
        let a = a.floor() as u32;
        let mut res = 0;
        for _ in 0..num {
            res += (self.rand)(a) + 1;
        }
        res as f64
    }
}

impl ExecResult {
    fn from(value: &Value) -> Self {
        match value {
            Value::Bool(x) => Self::Bool(*x),
            Value::Num(x) => Self::Num(*x),
            Value::Str(x) => Self::Str(Rc::clone(x)),
            Value::List(xs) => {
                Self::List(xs.iter().map(|x| ExecResult::from(x.as_ref())).collect())
            }
            _ => Self::None,
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
    fn num() {
        let mut rng = rand::thread_rng();
        let mut run_time = RunTime::new(move |x| rng.gen::<u32>() % x);
        let result = run_time.exec("2.0");
        assert_eq!(result, Some(ExecResult::Num(2.0)));
    }

    #[test]
    fn add_1_1() {
        let mut rng = rand::thread_rng();
        let mut run_time = RunTime::new(move |x| rng.gen::<u32>() % x);
        let result = run_time.exec("1+1");
        assert_eq!(result, Some(ExecResult::Num(2.0)));
    }

    #[test]
    fn sub_1_1() {
        let mut rng = rand::thread_rng();
        let mut run_time = RunTime::new(move |x| rng.gen::<u32>() % x);
        let result = run_time.exec("1-1");
        assert_eq!(result, Some(ExecResult::Num(0.0)));
    }

    #[test]
    fn multi_2_3() {
        let mut rng = rand::thread_rng();
        let mut run_time = RunTime::new(move |x| rng.gen::<u32>() % x);
        let result = run_time.exec("2*3");
        assert_eq!(result, Some(ExecResult::Num(6.0)));
    }

    #[test]
    fn div_3_2() {
        let mut rng = rand::thread_rng();
        let mut run_time = RunTime::new(move |x| rng.gen::<u32>() % x);
        let result = run_time.exec("3/2");
        assert_eq!(result, Some(ExecResult::Num(1.5)));
    }

    #[test]
    fn use_block() {
        let mut rng = rand::thread_rng();
        let mut run_time = RunTime::new(move |x| rng.gen::<u32>() % x);
        let result = run_time.exec("3*(4+5)");
        assert_eq!(result, Some(ExecResult::Num(27.0)));
    }

    #[test]
    fn use_sequence() {
        let mut rng = rand::thread_rng();
        let mut run_time = RunTime::new(move |x| rng.gen::<u32>() % x);
        let result = run_time.exec("3*(1+2; 2+3; 3+4)");
        assert_eq!(result, Some(ExecResult::Num(21.0)));
    }

    #[test]
    fn use_var() {
        let mut rng = rand::thread_rng();
        let mut run_time = RunTime::new(move |x| rng.gen::<u32>() % x);
        let result = run_time.exec("(x:=10;x)");
        assert_eq!(result, Some(ExecResult::Num(10.0)));
    }

    #[test]
    fn use_function_from_ident() {
        let mut rng = rand::thread_rng();
        let mut run_time = RunTime::new(move |x| rng.gen::<u32>() % x);
        let result = run_time.exec(r"(f:=\\x.x+1; f.2)");
        assert_eq!(result, Some(ExecResult::Num(3.0)));
    }

    #[test]
    fn use_function_direct() {
        let mut rng = rand::thread_rng();
        let mut run_time = RunTime::new(move |x| rng.gen::<u32>() % x);
        let result = run_time.exec(r"(\\x.x+1).2");
        assert_eq!(result, Some(ExecResult::Num(3.0)));
    }

    #[test]
    fn access_list_head() {
        let mut rng = rand::thread_rng();
        let mut run_time = RunTime::new(move |x| rng.gen::<u32>() % x);
        let result = run_time.exec(r"[1,2,3].2");
        assert_eq!(result, Some(ExecResult::Num(3.0)));
    }

    #[test]
    fn access_list_tail() {
        let mut rng = rand::thread_rng();
        let mut run_time = RunTime::new(move |x| rng.gen::<u32>() % x);
        let result = run_time.exec(r"[1,2,3].(0-1)");
        assert_eq!(result, Some(ExecResult::Num(3.0)));
    }

    #[test]
    fn unary_minus() {
        let mut rng = rand::thread_rng();
        let mut run_time = RunTime::new(move |x| rng.gen::<u32>() % x);
        let result = run_time.exec(r"[1,2,3].(-1)");
        assert_eq!(result, Some(ExecResult::Num(3.0)));
    }

    #[test]
    fn counted_loop() {
        let mut rng = rand::thread_rng();
        let mut run_time = RunTime::new(move |x| rng.gen::<u32>() % x);
        let x = run_time.exec(r"5.(5)");
        let y = run_time.exec(r"[5,5,5,5,5]");
        assert_eq!(x, y);
    }

    #[test]
    fn fnc_chain_direct() {
        let mut rng = rand::thread_rng();
        let mut run_time = RunTime::new(move |x| rng.gen::<u32>() % x);
        let x = run_time.exec(r"2>>\\x.x+1");
        assert_eq!(x, Some(ExecResult::Num(3.0)));
    }

    #[test]
    fn fnc_chain_with_ident() {
        let mut rng = rand::thread_rng();
        let mut run_time = RunTime::new(move |x| rng.gen::<u32>() % x);
        let x = run_time.exec(r"(f:=\\x.x+1; 2>>f)");
        assert_eq!(x, Some(ExecResult::Num(3.0)));
    }

    #[test]
    fn map_list() {
        let mut rng = rand::thread_rng();
        let mut run_time = RunTime::new(move |x| rng.gen::<u32>() % x);
        let x = run_time.exec(r"[1,1,1,1,1].(\\x.x+4)");
        let y = run_time.exec(r"[5,5,5,5,5]");
        assert_eq!(x, y);
    }

    #[test]
    fn call_fnc_by_two_arg() {
        let mut rng = rand::thread_rng();
        let mut run_time = RunTime::new(move |x| rng.gen::<u32>() % x);
        let x = run_time.exec(r"((\\x.\\y.x+y).(1).(2))");
        assert_eq!(x, Some(ExecResult::Num(3.0)));
    }

    #[test]
    fn capture_env() {
        let mut rng = rand::thread_rng();
        let mut run_time = RunTime::new(move |x| rng.gen::<u32>() % x);
        let x = run_time.exec(r"(a:=2; f:=\\x.a+x; a:=3; f.1)");
        assert_eq!(x, Some(ExecResult::Num(3.0)));
    }

    #[test]
    fn if_else_true() {
        let mut rng = rand::thread_rng();
        let mut run_time = RunTime::new(move |x| rng.gen::<u32>() % x);
        let x = run_time.exec(r"if(1+1==2)=>(3)else(4)");
        assert_eq!(x, Some(ExecResult::Num(3.0)));
    }

    #[test]
    fn if_else_false() {
        let mut rng = rand::thread_rng();
        let mut run_time = RunTime::new(move |x| rng.gen::<u32>() % x);
        let x = run_time.exec(r"if(1+1!=2)=>(3)else(4)");
        assert_eq!(x, Some(ExecResult::Num(4.0)));
    }

    #[test]
    fn if_if_else_else_true_true() {
        let mut rng = rand::thread_rng();
        let mut run_time = RunTime::new(move |x| rng.gen::<u32>() % x);
        let x = run_time.exec(r"if(1+1==2)=>if(1+2==3)=>(1)else(2)else(3)");
        assert_eq!(x, Some(ExecResult::Num(1.0)));
    }

    #[test]
    fn reduce_left() {
        let mut rng = rand::thread_rng();
        let mut run_time = RunTime::new(move |x| rng.gen::<u32>() % x);
        let x = run_time.exec(r"0#>[1,2,3,4,5].(\\p.\\c.p+c)");
        assert_eq!(x, Some(ExecResult::Num(15.0)));
    }

    #[test]
    fn reduce_right() {
        let mut rng = rand::thread_rng();
        let mut run_time = RunTime::new(move |x| rng.gen::<u32>() % x);
        let x = run_time.exec(r"[1,2,3,4,5]<#1.(\\p.\\c.p*c)");
        assert_eq!(x, Some(ExecResult::Num(120.0)));
    }

    #[test]
    fn reverse() {
        let mut rng = rand::thread_rng();
        let mut run_time = RunTime::new(move |x| rng.gen::<u32>() % x);
        let x = run_time.exec(r"[1,2,3,4,5]<#[].(\\p.\\c.p+[c])");
        let y = run_time.exec(r"[5,4,3,2,1]");
        assert_eq!(x, y);
    }

    #[test]
    fn shadowing() {
        let mut rng = rand::thread_rng();
        let mut run_time = RunTime::new(move |x| rng.gen::<u32>() % x);
        let x = run_time.exec(r"(x:=0; x:=x+1; x:=x+1; x)");
        assert_eq!(x, Some(ExecResult::Num(2.0)));
    }
}
