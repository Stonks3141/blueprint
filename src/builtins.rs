use crate::{error::Error, Builtin, Ident, Number, Value};
use fxhash::FxHashMap as HashMap;

macro_rules! make_env {
    ($( $vis:vis $env:ident { $($name:literal => $proc:expr),* $(,)? } )*) => {
        $(
            $vis fn $env() -> HashMap<Ident, Builtin> {
                [
                    $( ($name.to_string(), $proc as Builtin), )*
                ].into_iter().collect()
            }
        )*
    };
}

make_env! {
    pub base {
        "+" => |args| Ok(Value::Number(
             args.iter()
                 .map(|val| val.number())
                 .try_fold(Number::ZERO, |a, b| b.map(|b| a + b))?,
         )),
        "-" => |args| Ok(Value::Number(
            args.iter()
                .map(|val| val.number())
                .try_fold(Number::ZERO, |a, b| b.map(|b| a - b))?
                + Number::from(2) * args[0].number()?,
        )),
        "*" => |args| Ok(Value::Number(
            args.iter()
                .map(|val| val.number())
                .try_fold(Number::ONE, |a, b| b.map(|b| a * b))?,
        )),
        "/" => |args| {
            let (lhs, rhs) = (args[0].number()?, args[1].number()?);
            Ok(Value::Number(lhs / rhs))
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
        // TODO
        "or" => |args| Ok(Value::Boolean(args[0] == Value::TRUE || args[1] == Value::TRUE)),
        "display" => |args| {
            for val in args.iter() {
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
