use std::cell::RefCell;

use crate::{
    error::{Error, Result},
    Builtin, Ident, MaybeWeak, Value,
};
use rustc_hash::FxHashMap as HashMap;

macro_rules! make_env {
    ( $($name:literal => $proc:expr),* $(,)? ) => {
        [
            $( ($name.into(), $proc as Builtin), )*
        ].into_iter().collect()
    };
}

fn v(it: Value) -> Result<MaybeWeak<RefCell<Value>>> {
    Ok(MaybeWeak::new(RefCell::new(it)))
}

pub fn base() -> HashMap<Ident, Builtin> {
    make_env! {
        "+" => |mut args| match args.len() {
            0 => Err(Error::NotEnoughArguments { expected: 1, got: 0 }),
            1 => v(Value::Number(args.pop().unwrap().get().borrow().number()?)),
            _ => v(Value::Number(args.iter()
                .map(|it| it.get().borrow().number())
                .reduce(|a, b| Ok(a? + b?))
                .unwrap()?
            )),
        },
        "-" => |mut args| match args.len() {
            0 => Err(Error::NotEnoughArguments { expected: 1, got: 0 }),
            1 => v(Value::Number(-args.pop().unwrap().get().borrow().number()?)),
            _ => v(Value::Number(args.iter()
                .map(|it| it.get().borrow().number())
                .reduce(|a, b| Ok(a? - b?))
                .unwrap()?
            )),
        },
        "*" => |args| match args.len() {
            0 => Err(Error::NotEnoughArguments { expected: 2, got: 0 }),
            1 => Err(Error::NotEnoughArguments { expected: 2, got: 1 }),
            _ => v(Value::Number(args.iter()
                .map(|it| it.get().borrow().number())
                .reduce(|a, b| Ok(a? * b?))
                .unwrap()?
            )),
        },
        "/" => |args| match args.len() {
            0 => Err(Error::NotEnoughArguments { expected: 2, got: 0 }),
            1 => Err(Error::NotEnoughArguments { expected: 2, got: 1 }),
            _ => v(Value::Number(args.iter()
                .map(|it| it.get().borrow().number())
                .reduce(|a, b| Ok(a? / b?))
                .unwrap()?
            )),
        },
        "=" => |args| v(Value::Boolean(args[0].get().borrow().number()? == args[1].get().borrow().number()?)),
        // TODO
        "eq?" => |args| v(match (&*args[0].get().borrow(), &*args[1].get().borrow()) {
            (Value::Nil, Value::Nil) => Value::TRUE,
            (Value::Symbol(a), Value::Symbol(b)) => Value::Boolean(a == b),
            (Value::Number(a), Value::Number(b)) => Value::Boolean(a == b),
            (Value::Char(a), Value::Char(b)) => Value::Boolean(a == b),
            (Value::Boolean(a), Value::Boolean(b)) => Value::Boolean(a == b),
            _ => Value::FALSE,
        }),
        "equal?" => |args| v(Value::Boolean(args[0] == args[1])),
        "or" => |args| Ok(args.into_iter()
            .find(|x| *x.get().borrow() != Value::FALSE)
            .unwrap_or_else(|| MaybeWeak::new(RefCell::new(Value::FALSE)))
        ),
        "and" => |mut args| match args.len() {
            0 => v(Value::TRUE),
            _ => Ok(args.iter()
                .find(|x| *x.get().borrow() == Value::FALSE)
                .cloned()
                .unwrap_or_else(|| args.pop().unwrap())
            ),
        },
        "display" => |args| {
            for val in args {
                print!("{}", val.get().borrow());
            }
            v(Value::Nil)
        },
        "newline" => |args| match args.len() {
            0 => {
                println!();
                v(Value::Nil)
            },
            n => Err(Error::TooManyArguments { expected: 0, got: n })
        },
        "exit" => |args| {
            let Some(ok) = args.get(0) else {
                std::process::exit(0);
            };
            if ok.get().borrow().boolean()? {
                std::process::exit(0);
            } else {
                std::process::exit(1);
            }
        },
    }
}
