mod exec;

use crate::Env;
use crate::ExecResult;
use crate::Value;
use std::cell::{Ref, RefCell};
use std::rc::Rc;

pub use exec::exec;

pub struct RunTime<'a> {
    env: Env<'a>,
    log: Rc<RefCell<Vec<String>>>,
    rand: Rc<RefCell<dyn FnMut(u32) -> u32 + 'a>>,
}

impl<'a> Clone for RunTime<'a> {
    fn clone(&self) -> Self {
        Self {
            env: self.env.clone(),
            rand: Rc::clone(&self.rand),
            log: Rc::new(RefCell::new(self.log.borrow().clone())),
        }
    }
}

impl<'a> RunTime<'a> {
    pub fn new(rand: impl FnMut(u32) -> u32 + 'a) -> Self {
        let mut me = Self {
            env: Env::new(),
            log: Rc::new(RefCell::new(vec![])),
            rand: Rc::new(RefCell::new(rand)),
        };

        me.set_defaults();

        me
    }

    pub fn set_function(
        &mut self,
        name: impl Into<String>,
        fnc: impl FnMut(Rc<Value<'a>>) -> Option<Rc<Value<'a>>> + 'a,
    ) {
        let fnc = RefCell::new(Box::new(fnc));
        let fnc = Box::new(move |v| (&mut *fnc.borrow_mut())(v));
        let fnc = Rc::new(Value::Fnc(fnc));
        self.env.insert(Rc::new(name.into()), fnc);
    }

    pub fn log(&self) -> Ref<Vec<String>> {
        self.log.borrow()
    }

    pub fn clear_log(&self) {
        self.log.borrow_mut().clear();
    }

    fn set_defaults(&mut self) {
        self.set_function("log", {
            let log = Rc::clone(&self.log);
            move |val| {
                log.borrow_mut().push(format!("{}", ExecResult::from(&val)));
                Some(val)
            }
        });

        self.set_function("len", move |val| {
            let len = match val.as_ref() {
                Value::List(xs) => Some(xs.len() as f64),
                Value::Str(xs) => Some(xs.chars().collect::<Vec<char>>().len() as f64),
                _ => None,
            };
            len.map(|len| Rc::new(Value::Num(len)))
        });

        self.set_function("pack", move |val| {
            if let Value::List(lst) = val.as_ref() {
                let mut vs = vec![];
                for v in lst.as_ref() {
                    if let Value::List(v) = v.as_ref() {
                        vs.push(v);
                    } else {
                        return None;
                    }
                }
                let acc: Vec<Vec<Rc<Value>>> = vec![];
                let vs = vs.into_iter().fold(acc, |mut acc, x| {
                    let mut idx = 0;
                    for i in x.as_ref() {
                        if idx < acc.len() {
                            acc[idx].push(Rc::clone(i));
                        } else {
                            acc.push(vec![Rc::clone(i)])
                        }
                        idx += 1;
                    }
                    acc
                });
                let mut lst = vec![];
                for v in vs {
                    lst.push(Rc::new(Value::List(Rc::new(v))));
                }
                Some(Rc::new(Value::List(Rc::new(lst))))
            } else {
                None
            }
        });

        self.set_function("str", |val| {
            Some(Rc::new(Value::Str(Rc::new(format!(
                "{}",
                ExecResult::from(&val)
            )))))
        });

        self.set_function("max", |val| match val.as_ref() {
            Value::List(list) => {
                if list.len() > 0 {
                    if let Some(list) = bool_list(&list) {
                        let i = list[0];
                        let max = list.into_iter().fold(i, |m, v| if m < v { v } else { m });
                        Some(Rc::new(Value::Bool(max)))
                    } else if let Some(list) = num_list(&list) {
                        let i = list[0];
                        let max: f64 = list.into_iter().fold(i, |m, v| m.max(v));
                        Some(Rc::new(Value::Num(max)))
                    } else if let Some(list) = str_list(&list) {
                        let i = Rc::clone(&list[0]);
                        let max = list.into_iter().fold(i, |m, v| m.max(v));
                        Some(Rc::new(Value::Str(max)))
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            _ => None,
        });

        self.set_function("min", |val| match val.as_ref() {
            Value::List(list) => {
                if list.len() > 0 {
                    if let Some(list) = bool_list(&list) {
                        let i = list[0];
                        let max = list.into_iter().fold(i, |m, v| if m > v { v } else { m });
                        Some(Rc::new(Value::Bool(max)))
                    } else if let Some(list) = num_list(&list) {
                        let i = list[0];
                        let max: f64 = list.into_iter().fold(i, |m, v| m.min(v));
                        Some(Rc::new(Value::Num(max)))
                    } else if let Some(list) = str_list(&list) {
                        let i = Rc::clone(&list[0]);
                        let max = list.into_iter().fold(i, |m, v| m.min(v));
                        Some(Rc::new(Value::Str(max)))
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            _ => None,
        });

        self.set_function("sort", |val| match val.as_ref() {
            Value::List(list) => {
                if list.len() > 0 {
                    if let Some(mut list) = bool_list(&list) {
                        list.sort();
                        let list = list.into_iter().map(|x| Rc::new(Value::Bool(x))).collect();
                        Some(Rc::new(Value::List(Rc::new(list))))
                    } else if let Some(mut list) = num_list(&list) {
                        list.sort_by(|a, b| a.partial_cmp(b).unwrap());
                        let list = list.into_iter().map(|x| Rc::new(Value::Num(x))).collect();
                        Some(Rc::new(Value::List(Rc::new(list))))
                    } else if let Some(mut list) = str_list(&list) {
                        list.sort();
                        let list = list.into_iter().map(|x| Rc::new(Value::Str(x))).collect();
                        Some(Rc::new(Value::List(Rc::new(list))))
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            _ => None,
        });
    }
}

fn bool_list(list: &Vec<Rc<Value>>) -> Option<Vec<bool>> {
    let mut res = vec![];
    for item in list {
        if let Value::Bool(item) = item.as_ref() {
            res.push(*item);
        } else {
            return None;
        }
    }
    Some(res)
}

fn num_list(list: &Vec<Rc<Value>>) -> Option<Vec<f64>> {
    let mut res = vec![];
    for item in list {
        if let Value::Num(item) = item.as_ref() {
            res.push(*item);
        } else {
            return None;
        }
    }
    Some(res)
}

fn str_list(list: &Vec<Rc<Value>>) -> Option<Vec<Rc<String>>> {
    let mut res = vec![];
    for item in list {
        if let Value::Str(item) = item.as_ref() {
            res.push(Rc::clone(item));
        } else {
            return None;
        }
    }
    Some(res)
}
