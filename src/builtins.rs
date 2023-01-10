use crate::{error::Error, Builtin, Ident, Value};
use rustc_hash::FxHashMap as HashMap;

macro_rules! make_env {
    ($( $vis:vis $env:ident { $($name:literal => $proc:expr),* $(,)? } )*) => {
        $(
            $vis fn $env() -> HashMap<Ident, Builtin> {
                [
                    $( ($name.into(), $proc as Builtin), )*
                ].into_iter().collect()
            }
        )*
    };
}

make_env! {
    pub base {
        "+" => |mut args| match args.len() {
            0 => Err(Error::NotEnoughArguments { expected: 1, got: 0 }),
            1 => Ok(Value::Number(args.pop().unwrap().number()?)),
            _ => Ok(Value::Number(args.iter()
                .map(Value::number)
                .reduce(|a, b| Ok(a? + b?))
                .unwrap()?
            )),
        },
        "-" => |mut args| match args.len() {
            0 => Err(Error::NotEnoughArguments { expected: 1, got: 0 }),
            1 => Ok(Value::Number(-args.pop().unwrap().number()?)),
            _ => Ok(Value::Number(args.iter()
                .map(Value::number)
                .reduce(|a, b| Ok(a? - b?))
                .unwrap()?
            )),
        },
        "*" => |args| match args.len() {
            0 => Err(Error::NotEnoughArguments { expected: 2, got: 0 }),
            1 => Err(Error::NotEnoughArguments { expected: 2, got: 1 }),
            _ => Ok(Value::Number(args.iter()
                .map(Value::number)
                .reduce(|a, b| Ok(a? * b?))
                .unwrap()?
            )),
        },
        "/" => |args| match args.len() {
            0 => Err(Error::NotEnoughArguments { expected: 2, got: 0 }),
            1 => Err(Error::NotEnoughArguments { expected: 2, got: 1 }),
            _ => Ok(Value::Number(args.iter()
                .map(Value::number)
                .reduce(|a, b| Ok(a? / b?))
                .unwrap()?
            )),
        },
        "=" => |args| Ok(Value::Boolean(args[0].number()? == args[1].number()?)),
        // TODO
        "eq?" => |args| Ok(match (args[0].clone(), args[1].clone()) {
            (Value::Nil, Value::Nil) => Value::TRUE,
            (Value::Symbol(a), Value::Symbol(b)) => Value::Boolean(a == b),
            (Value::Number(a), Value::Number(b)) => Value::Boolean(a == b),
            (Value::Char(a), Value::Char(b)) => Value::Boolean(a == b),
            (Value::Boolean(a), Value::Boolean(b)) => Value::Boolean(a == b),
            _ => Value::FALSE,
        }),
        "equal?" => |args| Ok(Value::Boolean(args[0] == args[1])),
        "or" => |args| Ok(args.into_iter()
            .find(|x| *x != Value::FALSE)
            .unwrap_or(Value::FALSE)
        ),
        "and" => |mut args| match args.len() {
            0 => Ok(Value::TRUE),
            _ => Ok(args.iter()
                .find(|x| **x == Value::FALSE)
                .cloned()
                .unwrap_or_else(|| args.pop().unwrap())
            ),
        },
        "display" => |args| {
            for val in args {
                print!("{}", val);
            }
            Ok(Value::Nil)
        },
        "newline" => |_| {
            println!();
            Ok(Value::Nil)
        },
        "exit" => |args| {
            let Some(ok) = args.get(0) else {
                std::process::exit(0);
            };
            if ok.boolean()? {
                std::process::exit(0);
            } else {
                std::process::exit(1);
            }
        },
    }
}
