use crate::{error::Result, Value};

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum Builtin {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
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
                    .try_fold(0.0, |a, b| b.map(|b| a + b))?,
            ),
            Self::Sub => Value::Number(
                args.iter()
                    .map(|val| val.number())
                    .try_fold(0.0, |a, b| b.map(|b| a - b))?
                    + 2.0 * args[0].number()?,
            ),
            Self::Mul => Value::Number(
                args.iter()
                    .map(|val| val.number())
                    .try_fold(1.0, |a, b| b.map(|b| a * b))?,
            ),
            Self::Div => {
                let (lhs, rhs) = (args[0].number()?, args[1].number()?);
                Value::Number(lhs / rhs)
            }
            Self::Eq => Value::Boolean(args[0] == args[1]),
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
