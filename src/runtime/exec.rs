use super::ast::*;
use super::parser;
use super::Ref;
use super::RunTime;
use crate::ExecResult;
use crate::Value;
use std::collections::VecDeque;
use std::rc::Rc;

pub fn exec<'a>(code: &str, run_time: &RunTime<'a>) -> (Option<ExecResult>, Option<String>) {
    exec_mut(code, &mut run_time.clone())
}

pub fn exec_mut<'a>(
    code: &str,
    run_time: &mut RunTime<'a>,
) -> (Option<ExecResult>, Option<String>) {
    let (ast, msg) = parser::parse::exprs(code).unwrap_or((None, None));
    let value = ast
        .and_then(|ast| exec_exprs(&ast, run_time))
        .map(|x| ExecResult::from(x.as_ref()));
    (value, msg)
}

fn exec_only_value<'a>(code: &str, run_time: &mut RunTime<'a>) -> Option<Rc<Value<'a>>> {
    let (ast, _) = parser::parse::exprs(code).unwrap_or((None, None));
    let value = ast.and_then(|ast| exec_exprs(&ast, run_time));
    value
}

fn exec_exprs<'a>(exprs: &Vec<Expr>, run_time: &mut RunTime<'a>) -> Option<Rc<Value<'a>>> {
    let mut res = None;
    for expr in exprs {
        res = exec_expr(expr, run_time);
    }
    res
}

fn exec_expr<'a>(expr: &Expr, run_time: &mut RunTime<'a>) -> Option<Rc<Value<'a>>> {
    match expr {
        Expr::Assign(ident, fnc_chain) => {
            let value = exec_branch(fnc_chain, run_time);
            if let Some(value) = value {
                run_time.env.insert(Rc::clone(ident), Rc::clone(&value));
                Some(value)
            } else {
                None
            }
        }
        Expr::Branch(branch) => exec_branch(branch, run_time),
    }
}

fn exec_branch<'a>(branch: &Branch, run_time: &mut RunTime<'a>) -> Option<Rc<Value<'a>>> {
    match branch {
        Branch::Branch(c, left, right) => {
            if let Some(c) = exec_fnc_chain(c, run_time) {
                if let Value::Bool(c) = c.as_ref() {
                    if *c {
                        exec_branch(left, run_time)
                    } else {
                        exec_branch(right, run_time)
                    }
                } else {
                    None
                }
            } else {
                None
            }
        }
        Branch::FncChain(fnc_chain) => exec_fnc_chain(fnc_chain, run_time),
    }
}

fn exec_fnc_chain<'b>(fnc_chain: &FncChain, run_time: &mut RunTime<'b>) -> Option<Rc<Value<'b>>> {
    match fnc_chain {
        FncChain::Concat(left, right) => {
            let left = exec_fnc_chain(left, run_time);
            let right = exec_fnc_chain(right, run_time);
            let run_time = run_time.capture();
            if let (Some(left), Some(right)) = (left, right) {
                Some(Rc::new(Value::Fnc(Box::new(
                    move |argv| -> Option<Rc<Value<'b>>> {
                        let mut run_time = run_time.capture();
                        call_like_fnc_with_value(&left, argv, &mut run_time)
                            .and_then(|x| call_like_fnc_with_value(&right, x, &mut run_time))
                    },
                ))))
            } else {
                None
            }
        }
        FncChain::Pipeline(left, right) => {
            let left = exec_fnc_chain(left, run_time);
            let right = exec_fnc_chain(right, run_time);
            if let (Some(left), Some(right)) = (left, right) {
                call_like_fnc_with_value(&right, left, run_time)
            } else {
                None
            }
        }
        FncChain::FncDef(fnc_def) => exec_fnc_def(fnc_def, run_time),
    }
}

fn exec_fnc_def<'b>(fnc_def: &FncDef, run_time: &mut RunTime<'b>) -> Option<Rc<Value<'b>>> {
    match fnc_def {
        FncDef::FncDef(arg, right) => {
            let arg = Rc::clone(arg);
            let right = Rc::clone(right);
            let run_time = run_time.capture();
            Some(Rc::new(Value::Fnc(Box::new(
                move |argv| -> Option<Rc<Value<'b>>> {
                    let mut run_time = run_time.capture();
                    run_time.env.insert(Rc::clone(&arg), argv);
                    exec_fnc_def(&Rc::clone(&right), &mut run_time)
                },
            ))))
        }
        FncDef::Expr0(expr_0) => exec_expr_0(expr_0, run_time),
    }
}

fn exec_expr_0<'b>(expr_0: &Expr0, run_time: &mut RunTime<'b>) -> Option<Rc<Value<'b>>> {
    match expr_0 {
        Expr0::Expr0(left, right, op_code) => {
            let right = exec_expr_0(right, run_time);
            if let Some(right) = right {
                match op_code {
                    OpCode0::At => match right.as_ref() {
                        Value::Fnc(fnc) => {
                            let mut value = vec![];
                            loop {
                                let left = exec_expr_0(left, run_time);
                                let f = left
                                    .and_then(|left| {
                                        value.push(Rc::clone(&left));
                                        fnc(Rc::clone(&left))
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
                                let left = exec_expr_0(left, run_time);
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
        Expr0::Expr1(expr_1) => exec_expr_1(expr_1, run_time),
    }
}

fn exec_expr_1<'b>(expr_1: &Expr1, run_time: &mut RunTime<'b>) -> Option<Rc<Value<'b>>> {
    match expr_1 {
        Expr1::Expr1(left, right, op_code) => {
            let left = exec_expr_1(left, run_time);
            let right = exec_expr_1(right, run_time);
            if let (Some(left), Some(right)) = (left, right) {
                let (left, right) = (left.as_ref(), right.as_ref());
                if let (Value::Bool(left), Value::Bool(right)) = (left, right) {
                    exec_expr_1_bool(*left, *right, op_code)
                } else if let (Value::Str(left), Value::Str(right)) = (left, right) {
                    exec_expr_1_str(&left, &right, op_code)
                } else if let (Value::Num(left), Value::Num(right)) = (left, right) {
                    exec_expr_1_num(*left, *right, op_code)
                } else {
                    None
                }
            } else {
                None
            }
        }
        Expr1::Expr2(expr_2) => exec_expr_2(expr_2, run_time),
    }
}

fn exec_expr_2<'b>(expr_2: &Expr2, run_time: &mut RunTime<'b>) -> Option<Rc<Value<'b>>> {
    match expr_2 {
        Expr2::Expr2(left, right, op_code) => {
            let left = exec_expr_2(left, run_time);
            let right = exec_expr_2(right, run_time);
            if let (Some(left), Some(right)) = (left, right) {
                let (left, right) = (left.as_ref(), right.as_ref());
                if let (Value::Bool(left), Value::Bool(right)) = (left, right) {
                    exec_expr_2_bool(*left, *right, op_code)
                } else if let (Value::Str(left), Value::Str(right)) = (left, right) {
                    exec_expr_2_str(&left, &right, op_code)
                } else if let (Value::Num(left), Value::Num(right)) = (left, right) {
                    exec_expr_2_num(*left, *right, op_code)
                } else if let (Value::List(left), Value::List(right)) = (left, right) {
                    exec_expr_2_list(&left, &right, op_code)
                } else {
                    None
                }
            } else {
                None
            }
        }
        Expr2::Expr3(expr_3) => exec_expr_3(expr_3, run_time),
    }
}

fn exec_expr_3<'b>(expr_3: &Expr3, run_time: &mut RunTime<'b>) -> Option<Rc<Value<'b>>> {
    match expr_3 {
        Expr3::Expr3(left, right, op_code) => {
            let left = exec_expr_3(left, run_time);
            let right = exec_expr_3(right, run_time);
            if let (Some(left), Some(right)) = (&left, &right) {
                let (left, right) = (left.as_ref(), right.as_ref());
                if let (Value::Bool(left), Value::Bool(right)) = (left, right) {
                    exec_expr_3_bool(*left, *right, op_code)
                } else if let (Value::Num(left), Value::Num(right)) = (left, right) {
                    exec_expr_3_num(*left, *right, op_code)
                } else {
                    None
                }
            } else {
                None
            }
        }
        Expr3::Expr4(expr_4) => exec_expr_4(expr_4, run_time),
    }
}

fn exec_expr_4<'b>(expr_4: &Expr4, run_time: &mut RunTime<'b>) -> Option<Rc<Value<'b>>> {
    match expr_4 {
        Expr4::Expr4(left, right, op_code) => {
            let left = exec_expr_4(left, run_time);
            let right = exec_expr_4(right, run_time);
            if let (Some(left), Some(right)) = (left, right) {
                let (left, right) = (left.as_ref(), right.as_ref());
                if let (Value::Num(left), Value::Num(right)) = (left, right) {
                    exec_expr_4_num(*left, *right, op_code, run_time)
                } else {
                    None
                }
            } else {
                None
            }
        }
        Expr4::Unary(unary) => exec_expr_unary(unary, run_time),
    }
}

fn exec_expr_unary<'b>(unary: &Unary, run_time: &mut RunTime<'b>) -> Option<Rc<Value<'b>>> {
    match unary {
        Unary::Plus(fnc_call) => {
            if let Some(val) = exec_fnc_call(fnc_call, run_time) {
                match val.as_ref() {
                    Value::Bool(val) => exec_expr_2_bool(false, *val, &OpCode2::Add),
                    Value::List(val) => exec_expr_2_list(&vec![], val, &OpCode2::Add),
                    Value::Num(val) => exec_expr_2_num(0.0, *val, &OpCode2::Add),
                    Value::Str(val) => exec_expr_2_str(&String::from(""), val, &OpCode2::Add),
                    _ => None,
                }
            } else {
                None
            }
        }
        Unary::Minus(fnc_call) => {
            if let Some(val) = exec_fnc_call(fnc_call, run_time) {
                match val.as_ref() {
                    Value::Bool(val) => exec_expr_2_bool(false, *val, &OpCode2::Sub),
                    Value::List(val) => exec_expr_2_list(&vec![], val, &OpCode2::Sub),
                    Value::Num(val) => exec_expr_2_num(0.0, *val, &OpCode2::Sub),
                    Value::Str(val) => exec_expr_2_str(&String::from(""), val, &OpCode2::Sub),
                    _ => None,
                }
            } else {
                None
            }
        }
        Unary::FncCall(fnc_call) => exec_fnc_call(fnc_call, run_time),
    }
}

fn exec_fnc_call<'b>(fnc_call: &FncCall, run_time: &mut RunTime<'b>) -> Option<Rc<Value<'b>>> {
    match fnc_call {
        FncCall::FncCall(fnc_call, arg) => {
            let fnc = exec_fnc_call(fnc_call, run_time);
            if let Some(fnc) = fnc {
                let fnc = fnc.as_ref();
                call_like_fnc_with_term(fnc, arg, run_time)
            } else {
                None
            }
        }
        FncCall::Reducer(reducer) => exec_reducer(reducer, run_time),
    }
}

fn exec_reducer<'b>(reducer: &Reducer, run_time: &mut RunTime<'b>) -> Option<Rc<Value<'b>>> {
    match reducer {
        Reducer::RLeft(i, lst) => {
            let i = exec_reducer(i, run_time);
            let lst = exec_reducer(lst, run_time);
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
            let i = exec_reducer(i, run_time);
            let lst = exec_reducer(lst, run_time);
            if let (Some(i), Some(lst)) = (i, lst) {
                match lst.as_ref() {
                    Value::List(lst) => Some(Rc::new(Value::RRight(i, Rc::clone(lst)))),
                    _ => Some(Rc::new(Value::RRight(i, Rc::new(vec![lst])))),
                }
            } else {
                None
            }
        }
        Reducer::Term(term) => exec_term(term, run_time),
    }
}

fn exec_term<'b>(term: &Term, run_time: &mut RunTime<'b>) -> Option<Rc<Value<'b>>> {
    match term {
        Term::Literal(literal) => exec_literal(literal, run_time),
        Term::List(list) => {
            let mut values = vec![];
            for item in list {
                if let Some(value) = exec_expr(item, run_time) {
                    values.push(value);
                } else {
                    return None;
                }
            }
            Some(Rc::new(Value::List(Rc::new(values))))
        }
        Term::Expr(exprs) => {
            let mut run_time = run_time.capture();
            exec_exprs(exprs, &mut run_time)
        }
    }
}

fn exec_literal<'b>(literal: &Literal, run_time: &mut RunTime<'b>) -> Option<Rc<Value<'b>>> {
    match literal {
        Literal::Ident(ident) => {
            let ident = Rc::clone(&ident);
            if let Some(value) = run_time.env.get(&ident) {
                Some(Rc::clone(value))
            } else {
                None
            }
        }
        Literal::Num(num) => Some(Rc::new(Value::Num(*num))),
        Literal::Str(str) => Some(Rc::new(Value::Str(Rc::clone(str)))),
        Literal::Ref(address) => {
            let src = value_from_ref(&run_time.reference, address.iter().collect());
            if let Some(src) = src {
                exec_only_value(src, &mut run_time.capture())
            } else {
                None
            }
        }
    }
}

fn value_from_ref<'a, 'b>(r: &'b Ref, mut a: VecDeque<&String>) -> Option<&'b String> {
    if let Some(child) = a.pop_front() {
        if let Some(child) = r.get(child) {
            value_from_ref(child, a)
        } else {
            None
        }
    } else {
        r.value.as_ref()
    }
}

fn call_like_fnc_with_term<'b>(
    fnc: &Value<'b>,
    arg: &FncCall,
    run_time: &mut RunTime<'b>,
) -> Option<Rc<Value<'b>>> {
    match fnc {
        Value::List(..)
        | Value::Str(..)
        | Value::RLeft(..)
        | Value::RRight(..)
        | Value::Fnc(..) => match exec_fnc_call(arg, run_time) {
            Some(arg) => call_like_fnc_with_value(&fnc, arg, run_time),
            None => None,
        },
        Value::Num(v) => {
            let n = v.floor() as usize;
            let mut res = vec![];
            for _ in 0..n {
                if let Some(v) = exec_fnc_call(arg, run_time) {
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

fn call_like_fnc_with_value<'b>(
    fnc: &Value<'b>,
    arg: Rc<Value<'b>>,
    run_time: &mut RunTime<'b>,
) -> Option<Rc<Value<'b>>> {
    match fnc {
        Value::Fnc(fnc) => fnc(arg),
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
            Value::Fnc(fnc) => {
                let mut rs = vec![];
                for v in vs.as_ref() {
                    if let Some(r) = fnc(Rc::clone(v)) {
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
                if let Some(f) = call_like_fnc_with_value(&arg, Rc::clone(&pre), run_time) {
                    if let Some(p) = call_like_fnc_with_value(&f, Rc::clone(x), run_time) {
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
                if let Some(f) = call_like_fnc_with_value(&arg, Rc::clone(&pre), run_time) {
                    if let Some(p) = call_like_fnc_with_value(&f, Rc::clone(x), run_time) {
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

fn exec_expr_1_bool<'b>(left: bool, right: bool, op_code: &OpCode1) -> Option<Rc<Value<'b>>> {
    match op_code {
        OpCode1::Equal => Some(Rc::new(Value::Bool(left == right))),
        OpCode1::NotEq => Some(Rc::new(Value::Bool(left != right))),
        OpCode1::EqGreaterThan => Some(Rc::new(Value::Bool(left >= right))),
        OpCode1::EqLessThan => Some(Rc::new(Value::Bool(left <= right))),
        OpCode1::GreaterThan => Some(Rc::new(Value::Bool(left > right))),
        OpCode1::LessThan => Some(Rc::new(Value::Bool(left < right))),
    }
}

fn exec_expr_1_str<'b>(left: &String, right: &String, op_code: &OpCode1) -> Option<Rc<Value<'b>>> {
    match op_code {
        OpCode1::Equal => Some(Rc::new(Value::Bool(left == right))),
        OpCode1::NotEq => Some(Rc::new(Value::Bool(left != right))),
        OpCode1::EqGreaterThan => Some(Rc::new(Value::Bool(left >= right))),
        OpCode1::EqLessThan => Some(Rc::new(Value::Bool(left <= right))),
        OpCode1::GreaterThan => Some(Rc::new(Value::Bool(left > right))),
        OpCode1::LessThan => Some(Rc::new(Value::Bool(left < right))),
    }
}

fn exec_expr_1_num<'b>(left: f64, right: f64, op_code: &OpCode1) -> Option<Rc<Value<'b>>> {
    match op_code {
        OpCode1::Equal => Some(Rc::new(Value::Bool(left == right))),
        OpCode1::NotEq => Some(Rc::new(Value::Bool(left != right))),
        OpCode1::EqGreaterThan => Some(Rc::new(Value::Bool(left >= right))),
        OpCode1::EqLessThan => Some(Rc::new(Value::Bool(left <= right))),
        OpCode1::GreaterThan => Some(Rc::new(Value::Bool(left > right))),
        OpCode1::LessThan => Some(Rc::new(Value::Bool(left < right))),
    }
}

fn exec_expr_2_bool<'b>(left: bool, right: bool, op_code: &OpCode2) -> Option<Rc<Value<'b>>> {
    match op_code {
        OpCode2::Add => Some(Rc::new(Value::Bool(left || right))),
        _ => None,
    }
}

fn exec_expr_2_str<'b>(left: &String, right: &String, op_code: &OpCode2) -> Option<Rc<Value<'b>>> {
    match op_code {
        OpCode2::Add => Some(Rc::new(Value::Str(Rc::new(format!("{}{}", left, right))))),
        _ => None,
    }
}

fn exec_expr_2_num<'b>(left: f64, right: f64, op_code: &OpCode2) -> Option<Rc<Value<'b>>> {
    match op_code {
        OpCode2::Add => Some(Rc::new(Value::Num(left + right))),
        OpCode2::Sub => Some(Rc::new(Value::Num(left - right))),
    }
}

fn exec_expr_2_list<'b>(
    left: &Vec<Rc<Value<'b>>>,
    right: &Vec<Rc<Value<'b>>>,
    op_code: &OpCode2,
) -> Option<Rc<Value<'b>>> {
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

fn exec_expr_3_bool<'b>(left: bool, right: bool, op_code: &OpCode3) -> Option<Rc<Value<'b>>> {
    match op_code {
        OpCode3::Multi => Some(Rc::new(Value::Bool(left && right))),
        _ => None,
    }
}

fn exec_expr_3_num<'b>(left: f64, right: f64, op_code: &OpCode3) -> Option<Rc<Value<'b>>> {
    match op_code {
        OpCode3::Multi => Some(Rc::new(Value::Num(left * right))),
        OpCode3::Div => Some(Rc::new(Value::Num(left / right))),
        OpCode3::Mod => Some(Rc::new(Value::Num(left % right))),
    }
}

fn exec_expr_4_num<'b>(
    left: f64,
    right: f64,
    op_code: &OpCode4,
    run_time: &mut RunTime,
) -> Option<Rc<Value<'b>>> {
    match op_code {
        OpCode4::SDice => Some(Rc::new(Value::Num(exec_dice(left, right, run_time) as f64))),
        OpCode4::LDice => {
            let mut res = vec![];
            let num = left.floor() as usize;
            for _ in 0..num {
                res.push(Rc::new(Value::Num(exec_dice(1.0, right, run_time))));
            }
            Some(Rc::new(Value::List(Rc::new(res))))
        }
    }
}

fn exec_dice(num: f64, a: f64, run_time: &mut RunTime) -> f64 {
    let num = num.floor() as usize;
    let a = a.floor() as u32;
    let mut res = 0;
    for _ in 0..num {
        res += (&mut *run_time.rand.borrow_mut())(a) + 1;
    }
    res as f64
}
