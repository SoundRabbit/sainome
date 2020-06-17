mod exec;

use crate::Env;
use crate::ExecResult;
use crate::Value;
use std::cell::RefCell;
use std::rc::Rc;

pub use exec::exec;

pub struct RunTime<'a> {
    env: Env<'a>,
    log: Rc<RefCell<Vec<String>>>,
    rand: Rc<RefCell<dyn FnMut(u32) -> u32 + 'a>>,
}

impl<'a> RunTime<'a> {
    pub fn new(rand: impl FnMut(u32) -> u32 + 'a) -> Self {
        let mut me = Self {
            env: Env::new(),
            log: Rc::new(RefCell::new(vec![])),
            rand: Rc::new(RefCell::new(rand)),
        };

        me.set_log();
        me.set_len();
        me.set_pack();

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

    pub fn log(&self) -> Vec<String> {
        self.log.borrow().clone()
    }

    fn set_log(&mut self) {
        let log = Rc::clone(&self.log);
        self.set_function("log", move |val| {
            log.borrow_mut().push(format!("{}", ExecResult::from(&val)));
            Some(val)
        });
    }

    fn set_len(&mut self) {
        self.set_function("len", move |val| {
            let len = match val.as_ref() {
                Value::List(xs) => Some(xs.len() as f64),
                Value::Str(xs) => Some(xs.chars().collect::<Vec<char>>().len() as f64),
                _ => None,
            };
            len.map(|len| Rc::new(Value::Num(len)))
        })
    }

    fn set_pack(&mut self) {
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
    }
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
