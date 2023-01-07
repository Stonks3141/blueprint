use crate::{error::Result, Number, Value};

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum Builtin {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Equal,
    NumEq,
    Or,
    Display,
    Newline,
    Exit,
}

impl Builtin {
    pub fn eval(self, args: &[Value]) -> Result<Value> {
        Ok(match self {
            Self::Add => Value::Number(
                args.iter()
                    .map(|val| val.number())
                    .try_fold(Number::ZERO, |a, b| b.map(|b| a + b))?,
            ),
            Self::Sub => Value::Number(
                args.iter()
                    .map(|val| val.number())
                    .try_fold(Number::ZERO, |a, b| b.map(|b| a - b))?
                    + Number::from(2) * args[0].number()?,
            ),
            Self::Mul => Value::Number(
                args.iter()
                    .map(|val| val.number())
                    .try_fold(Number::ONE, |a, b| b.map(|b| a * b))?,
            ),
            Self::Div => {
                let (lhs, rhs) = (args[0].number()?, args[1].number()?);
                Value::Number(lhs / rhs)
            }
            Self::NumEq => Value::Boolean(args[0].number()? == args[1].number()?),
            // TODO
            Self::Eq => match (args[0].clone(), args[1].clone()) {
                (Value::Nil, Value::Nil) => Value::TRUE,
                (Value::Symbol(a), Value::Symbol(b)) => Value::Boolean(a == b),
                (Value::Number(a), Value::Number(b)) => Value::Boolean(a == b),
                (Value::Char(a), Value::Char(b)) => Value::Boolean(a == b),
                (Value::Boolean(a), Value::Boolean(b)) => Value::Boolean(a == b),
                _ => Value::FALSE,
            },
            Self::Equal => Value::Boolean(args[0] == args[1]),
            Self::Or => Value::Boolean(args[0] == Value::TRUE || args[1] == Value::TRUE),
            Self::Display => {
                for val in args.iter() {
                    print!("{}", val);
                }
                Value::Nil
            }
            Self::Newline => {
                println!();
                Value::Nil
            }
            Self::Exit => {
                let Some(ok) = args.get(0) else {
                    std::process::exit(0);
                };
                if ok.boolean()? {
                    std::process::exit(0);
                } else {
                    std::process::exit(1);
                }
            }
        })
    }
}
