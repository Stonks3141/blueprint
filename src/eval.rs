#[cfg(test)]
mod tests;

use crate::{
    error::{Error, Result},
    Env, Expr, MaybeWeak, Value,
};
use std::{borrow::Cow, cell::RefCell, cmp::Ordering};

pub fn eval(mut expr: Expr, mut env: Cow<'_, Env>) -> Result<MaybeWeak<RefCell<Value>>> {
    Ok(loop {
        match expr {
            Expr::Application { procedure, args } => {
                let arg_vals = args
                    .into_iter()
                    .map(|arg| eval(arg, Cow::Borrowed(&env)))
                    .collect::<Result<Vec<_>>>()?;

                let proc = eval(*procedure, Cow::Borrowed(&env))?.get();
                let procedure = proc.borrow();

                let (closure_env, args, val) = match *procedure {
                    Value::Closure {
                        ref env,
                        ref args,
                        ref val,
                    } => (env, args, val),
                    Value::Builtin(builtin) => break builtin(arg_vals)?,
                    _ => {
                        return Err(Error::Value {
                            expected: "{closure}".to_string(),
                            found: format!("{procedure}"),
                        });
                    }
                };
                let mut closure_env = closure_env.clone();
                let args = args.clone();

                let expected = args.len();
                let got = arg_vals.len();
                match got.cmp(&expected) {
                    Ordering::Less => return Err(Error::NotEnoughArguments { expected, got }),
                    Ordering::Greater => return Err(Error::TooManyArguments { expected, got }),
                    Ordering::Equal => (),
                }

                closure_env.reserve(args.len());
                closure_env.extend(args.into_iter().zip(arg_vals.into_iter()));

                env = Cow::Owned(Env {
                    outer: None,
                    this: closure_env,
                });
                expr = *val.clone();
                continue;
            }
            Expr::Lambda { args, val } => {
                break MaybeWeak::new(RefCell::new(Value::Closure {
                    env: env.flatten(),
                    args,
                    val,
                }));
            }
            Expr::Let { bindings, val } => {
                let new_env = env.layer_with(
                    bindings
                        .into_iter()
                        .map(|(k, v)| match eval(v, Cow::Borrowed(&env)) {
                            Ok(v) => Ok((k, v)),
                            Err(e) => Err(e),
                        })
                        .collect::<Result<_>>()?,
                );
                break eval(*val, Cow::Borrowed(&new_env))?;
            }
            Expr::LetS { bindings, val } => {
                let mut new_env = Env::from_outer(&env);
                for (k, v) in bindings {
                    let v = eval(v, Cow::Borrowed(&new_env))?;
                    new_env.this.insert(k, v);
                }
                break eval(*val, Cow::Borrowed(&new_env))?;
            }
            Expr::Letrec { bindings, val } => {
                let new_env = Env {
                    outer: Some(&env),
                    this: bindings
                        .keys()
                        .map(|k| (k.clone(), MaybeWeak::new(RefCell::new(Value::Nil))))
                        .collect(),
                };
                let mut inner_env = new_env.clone();
                inner_env.this.values_mut().for_each(|v| *v = v.weak());

                for (k, v) in bindings
                    .into_iter()
                    .map(|(k, v)| (k, eval(v, Cow::Borrowed(&inner_env))))
                    .collect::<Vec<_>>()
                {
                    *new_env.this[&k].get().borrow_mut() = v?.inner();
                }
                break eval(*val, Cow::Borrowed(&new_env))?;
            }
            Expr::LetrecS { bindings, val } => {
                let new_env = Env {
                    outer: Some(&env),
                    this: bindings
                        .iter()
                        .map(|(k, _)| (k.clone(), MaybeWeak::new(RefCell::new(Value::Nil))))
                        .collect(),
                };
                let mut inner_env = new_env.clone();
                inner_env.this.values_mut().for_each(|v| *v = v.weak());

                for (k, v) in bindings {
                    let v = eval(v, Cow::Borrowed(&inner_env))?;
                    *new_env.this[&k].get().borrow_mut() = v.inner();
                }
                break eval(*val, Cow::Borrowed(&new_env))?;
            }
            Expr::Define { .. } => {
                panic!("`define`s should have been removed by the `unify` function and the parser")
            }
            Expr::If { predicate, t, f } => {
                let predicate = eval(*predicate, Cow::Borrowed(&env))?.get();
                if *predicate.borrow() != Value::FALSE {
                    expr = *t;
                } else {
                    expr = *f;
                }
                continue;
            }
            Expr::Begin { ops, val } => {
                for op in ops {
                    eval(op, Cow::Borrowed(&env))?;
                }
                break eval(*val, env)?;
            }
            Expr::Set { ident, val } => {
                *env.get(&ident)
                    .ok_or(Error::Unbound(ident))?
                    .get()
                    .borrow_mut() = eval(*val, Cow::Borrowed(&env))?.inner();
                break MaybeWeak::new(RefCell::new(Value::Nil));
            }
            Expr::Value(val) => break MaybeWeak::new(RefCell::new(val)),
            Expr::Ident(ident) => {
                break env.get(&ident).ok_or(Error::Unbound(ident))?;
            }
        }
    })
}
