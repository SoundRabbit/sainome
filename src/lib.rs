extern crate peg;

mod ast;
mod parser;
mod run_time;

use std::collections::HashMap;
use std::fmt::Display;
use std::rc::Rc;

pub use run_time::exec;
pub use run_time::exec_mut;
pub use run_time::RunTime;

pub enum Value<'a> {
    Bool(bool),
    Str(Rc<String>),
    Num(f64),
    List(Rc<Vec<Rc<Value<'a>>>>),
    Fnc(Box<dyn Fn(Rc<Value<'a>>) -> Option<Rc<Value<'a>>> + 'a>),
    RLeft(Rc<Value<'a>>, Rc<Vec<Rc<Value<'a>>>>),
    RRight(Rc<Value<'a>>, Rc<Vec<Rc<Value<'a>>>>),
}

type Env<'a> = HashMap<Rc<String>, Rc<Value<'a>>>;

#[derive(Debug, PartialEq)]
pub enum ExecResult {
    Bool(bool),
    Str(Rc<String>),
    Num(f64),
    List(Vec<ExecResult>),
    Fnc,
    Err(String),
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
            Value::Fnc(..) | Value::RLeft(..) | Value::RRight(..) => Self::Fnc,
        }
    }
}

impl Display for ExecResult {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExecResult::Bool(x) => write!(f, "{}", x),
            ExecResult::Err(x) => write!(f, "Err:{}", x),
            ExecResult::Fnc => write!(f, "Fnction"),
            ExecResult::List(xs) => {
                let mut fmt = vec![];
                for x in xs {
                    fmt.push(format!("{}", x));
                }
                write!(f, "[{}]", fmt.join(", "))
            }
            ExecResult::Num(x) => write!(f, "{}", x),
            ExecResult::Str(x) => write!(f, "{}", x),
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
        let run_time = RunTime::new(move |x| rng.gen::<u32>() % x);
        let result = exec("2.0", &run_time);
        assert_eq!(result, Some(ExecResult::Num(2.0)));
    }

    #[test]
    fn add_1_1() {
        let mut rng = rand::thread_rng();
        let run_time = RunTime::new(move |x| rng.gen::<u32>() % x);
        let result = exec("1 + 1", &run_time);
        assert_eq!(result, Some(ExecResult::Num(2.0)));
    }

    #[test]
    fn sub_1_1() {
        let mut rng = rand::thread_rng();
        let run_time = RunTime::new(move |x| rng.gen::<u32>() % x);
        let result = exec("1 - 1", &run_time);
        assert_eq!(result, Some(ExecResult::Num(0.0)));
    }

    #[test]
    fn multi_2_3() {
        let mut rng = rand::thread_rng();
        let run_time = RunTime::new(move |x| rng.gen::<u32>() % x);
        let result = exec("2 * 3", &run_time);
        assert_eq!(result, Some(ExecResult::Num(6.0)));
    }

    #[test]
    fn div_3_2() {
        let mut rng = rand::thread_rng();
        let run_time = RunTime::new(move |x| rng.gen::<u32>() % x);
        let result = exec("3 / 2", &run_time);
        assert_eq!(result, Some(ExecResult::Num(1.5)));
    }

    #[test]
    fn use_block() {
        let mut rng = rand::thread_rng();
        let run_time = RunTime::new(move |x| rng.gen::<u32>() % x);
        let result = exec("3 * (4 + 5)", &run_time);
        assert_eq!(result, Some(ExecResult::Num(27.0)));
    }

    #[test]
    fn use_sequence() {
        let mut rng = rand::thread_rng();
        let run_time = RunTime::new(move |x| rng.gen::<u32>() % x);
        let result = exec("3 * (1 + 2; 2 + 3; 3 + 4)", &run_time);
        assert_eq!(result, Some(ExecResult::Num(21.0)));
    }

    #[test]
    fn use_var() {
        let mut rng = rand::thread_rng();
        let run_time = RunTime::new(move |x| rng.gen::<u32>() % x);
        let result = exec("(x := 10; x)", &run_time);
        assert_eq!(result, Some(ExecResult::Num(10.0)));
    }

    #[test]
    fn use_function_from_ident() {
        let mut rng = rand::thread_rng();
        let run_time = RunTime::new(move |x| rng.gen::<u32>() % x);
        let result = exec(r"(f := \x. x + 1; f.2)", &run_time);
        assert_eq!(result, Some(ExecResult::Num(3.0)));
    }

    #[test]
    fn use_function_direct() {
        let mut rng = rand::thread_rng();
        let run_time = RunTime::new(move |x| rng.gen::<u32>() % x);
        let result = exec(r"(\x. x + 1).2", &run_time);
        assert_eq!(result, Some(ExecResult::Num(3.0)));
    }

    #[test]
    fn access_list_head() {
        let mut rng = rand::thread_rng();
        let run_time = RunTime::new(move |x| rng.gen::<u32>() % x);
        let result = exec(r"[1, 2, 3].2", &run_time);
        assert_eq!(result, Some(ExecResult::Num(3.0)));
    }

    #[test]
    fn access_list_tail() {
        let mut rng = rand::thread_rng();
        let run_time = RunTime::new(move |x| rng.gen::<u32>() % x);
        let result = exec(r"[1, 2, 3].(0-1)", &run_time);
        assert_eq!(result, Some(ExecResult::Num(3.0)));
    }

    #[test]
    fn unary_minus() {
        let mut rng = rand::thread_rng();
        let run_time = RunTime::new(move |x| rng.gen::<u32>() % x);
        let result = exec(r"[1, 2, 3].(-1)", &run_time);
        assert_eq!(result, Some(ExecResult::Num(3.0)));
    }

    #[test]
    fn counted_loop() {
        let mut rng = rand::thread_rng();
        let run_time = RunTime::new(move |x| rng.gen::<u32>() % x);
        let x = exec(r"5.(5)", &run_time);
        let y = Some(ExecResult::List(vec![
            ExecResult::Num(5.0),
            ExecResult::Num(5.0),
            ExecResult::Num(5.0),
            ExecResult::Num(5.0),
            ExecResult::Num(5.0),
        ]));
        assert_eq!(x, y);
    }

    #[test]
    fn fnc_pipeline_direct() {
        let mut rng = rand::thread_rng();
        let run_time = RunTime::new(move |x| rng.gen::<u32>() % x);
        let x = exec(r"2 |> \x. x + 1", &run_time);
        assert_eq!(x, Some(ExecResult::Num(3.0)));
    }

    #[test]
    fn fnc_pipeline_with_ident() {
        let mut rng = rand::thread_rng();
        let run_time = RunTime::new(move |x| rng.gen::<u32>() % x);
        let x = exec(r"(f := \x. x + 1; 2 |> f)", &run_time);
        assert_eq!(x, Some(ExecResult::Num(3.0)));
    }

    #[test]

    fn fnc_concat_direct() {
        let mut rng = rand::thread_rng();
        let run_time = RunTime::new(move |x| rng.gen::<u32>() % x);
        let x = exec(r"(\x. x + 1 >> \x. x + 1).1", &run_time);
        assert_eq!(x, Some(ExecResult::Num(3.0)));
    }

    #[test]
    fn map_list() {
        let mut rng = rand::thread_rng();
        let run_time = RunTime::new(move |x| rng.gen::<u32>() % x);
        let x = exec(r"[1, 1, 1, 1, 1].(\x. x + 4)", &run_time);
        let y = exec(r"[5, 5, 5, 5, 5]", &run_time);
        assert_eq!(x, y);
    }

    #[test]
    fn call_fnc_by_two_arg() {
        let mut rng = rand::thread_rng();
        let run_time = RunTime::new(move |x| rng.gen::<u32>() % x);
        let x = exec(r"((\x. \y. x + y).(1).(2))", &run_time);
        assert_eq!(x, Some(ExecResult::Num(3.0)));
    }

    #[test]
    fn capture_env() {
        let mut rng = rand::thread_rng();
        let run_time = RunTime::new(move |x| rng.gen::<u32>() % x);
        let x = exec(r"(a := 2; f := \x. a + x; a := 3; f.1)", &run_time);
        assert_eq!(x, Some(ExecResult::Num(3.0)));
    }

    #[test]
    fn if_else_true() {
        let mut rng = rand::thread_rng();
        let run_time = RunTime::new(move |x| rng.gen::<u32>() % x);
        let x = exec(r"if 1+1==2 => 3 else 4", &run_time);
        assert_eq!(x, Some(ExecResult::Num(3.0)));
    }

    #[test]
    fn if_else_false() {
        let mut rng = rand::thread_rng();
        let run_time = RunTime::new(move |x| rng.gen::<u32>() % x);
        let x = exec(r"if(1 + 1 != 2)=>(3)else(4)", &run_time);
        assert_eq!(x, Some(ExecResult::Num(4.0)));
    }

    #[test]
    fn if_if_else_else_true_true() {
        let mut rng = rand::thread_rng();
        let run_time = RunTime::new(move |x| rng.gen::<u32>() % x);
        let x = exec(
            r"if 1 + 1 == 2 => if 1 + 2 ==3 => 1 else 2 else 3",
            &run_time,
        );
        assert_eq!(x, Some(ExecResult::Num(1.0)));
    }

    #[test]
    fn reduce_left() {
        let mut rng = rand::thread_rng();
        let run_time = RunTime::new(move |x| rng.gen::<u32>() % x);
        let x = exec(r"0#>[1, 2, 3, 4, 5].(\p. \c. p + c)", &run_time);
        assert_eq!(x, Some(ExecResult::Num(15.0)));
    }

    #[test]
    fn reduce_right() {
        let mut rng = rand::thread_rng();
        let run_time = RunTime::new(move |x| rng.gen::<u32>() % x);
        let x = exec(r"[1, 2, 3, 4, 5]<#1.(\p. \c. p * c)", &run_time);
        assert_eq!(x, Some(ExecResult::Num(120.0)));
    }

    #[test]
    fn reverse() {
        let mut rng = rand::thread_rng();
        let run_time = RunTime::new(move |x| rng.gen::<u32>() % x);
        let x = exec(r"[1, 2, 3, 4, 5]<#[].(\p. \c. p + [c])", &run_time);
        let y = exec(r"[5, 4, 3, 2, 1]", &run_time);
        assert_eq!(x, y);
    }

    #[test]
    fn shadowing() {
        let mut rng = rand::thread_rng();
        let run_time = RunTime::new(move |x| rng.gen::<u32>() % x);
        let x = exec(r"(x := 0; x := x + 1; x := x + 1; x)", &run_time);
        assert_eq!(x, Some(ExecResult::Num(2.0)));
    }

    #[test]
    fn outside_function() {
        let mut x = 0.0;
        {
            let mut rng = rand::thread_rng();
            let mut run_time = RunTime::new(move |x| rng.gen::<u32>() % x);
            run_time.set_function("addx", |_| {
                x = x + 1.0;
                Some(Rc::new(Value::Num(x)))
            });
            exec(r"addx.1", &run_time);
        }
        assert_eq!(x, 1.0);
    }

    #[test]
    fn logging() {
        let mut rng = rand::thread_rng();
        let run_time = RunTime::new(move |x| rng.gen::<u32>() % x);
        let x = exec(r"[0.0, 1.0, 2.0] |> log |> log", &run_time);
        if let Some(x) = x {
            assert_eq!(
                run_time.log().clone(),
                vec![format!("{}", x), format!("{}", x)]
            );
        } else {
            unreachable!();
        }
    }

    #[test]
    fn list_len() {
        let mut rng = rand::thread_rng();
        let run_time = RunTime::new(move |x| rng.gen::<u32>() % x);
        let x = exec(r"[0.0, 1.0, 2.0] |> len", &run_time);
        assert_eq!(x, Some(ExecResult::Num(3.0)));
    }

    #[test]
    fn str_len() {
        let mut rng = rand::thread_rng();
        let run_time = RunTime::new(move |x| rng.gen::<u32>() % x);
        let x = exec("\"あいうえおabcde\" |> len", &run_time);
        assert_eq!(x, Some(ExecResult::Num(10.0)));
    }

    #[test]
    fn pack_array() {
        let mut rng = rand::thread_rng();
        let run_time = RunTime::new(move |x| rng.gen::<u32>() % x);
        let x = exec("[[1, 2, 3], [4, 5], [6, 7, 8, 9]] |> pack", &run_time);
        let y = exec("[[1, 4, 6], [2, 5, 7], [3, 8], [9]]", &run_time);
        assert_eq!(x, y);
    }

    #[test]
    fn min_num() {
        let mut rng = rand::thread_rng();
        let run_time = RunTime::new(move |x| rng.gen::<u32>() % x);
        let x = exec("[1, 2, 3, 4, 5] |> min", &run_time);
        assert_eq!(x, Some(ExecResult::Num(1.0)));
    }

    #[test]
    fn min_str() {
        let mut rng = rand::thread_rng();
        let run_time = RunTime::new(move |x| rng.gen::<u32>() % x);
        let x = exec("[\"a\", \"abc\", \"dc\", \"c\", \"f\"] |> min", &run_time);
        assert_eq!(x, Some(ExecResult::Str(Rc::new(String::from("a")))));
    }

    #[test]
    fn max_num() {
        let mut rng = rand::thread_rng();
        let run_time = RunTime::new(move |x| rng.gen::<u32>() % x);
        let x = exec("[1, 2, 3, 4, 5] |> max", &run_time);
        assert_eq!(x, Some(ExecResult::Num(5.0)));
    }

    #[test]
    fn max_str() {
        let mut rng = rand::thread_rng();
        let run_time = RunTime::new(move |x| rng.gen::<u32>() % x);
        let x = exec("[\"a\", \"abc\", \"dc\", \"c\", \"f\"] |> max", &run_time);
        assert_eq!(x, Some(ExecResult::Str(Rc::new(String::from("f")))));
    }

    #[test]
    fn sort_num() {
        let mut rng = rand::thread_rng();
        let run_time = RunTime::new(move |x| rng.gen::<u32>() % x);
        let x = exec("[1, 6, 2, 3, 4, 7, 8, 5, 9] |> sort", &run_time);
        let y = exec("[1, 2, 3, 4, 5, 6, 7, 8, 9]", &run_time);
        assert_eq!(x, y);
    }

    #[test]
    fn sum_of_num() {
        let mut rng = rand::thread_rng();
        let run_time = RunTime::new(move |x| rng.gen::<u32>() % x);
        let x = exec("[1, 2, 3, 4, 5, 6, 7, 8, 9] |> sum", &run_time);
        assert_eq!(x, Some(ExecResult::Num(45.0)));
    }
}
