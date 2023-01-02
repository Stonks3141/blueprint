use builtin::Builtin;
use fxhash::FxHashMap as HashMap;
use std::{
    borrow::Cow,
    cell::RefCell,
    fmt,
    rc::{Rc, Weak},
    time::Instant,
};

mod builtin;
mod parse;
mod parse_old;
#[cfg(test)]
mod tests;

pub type Ident = String;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(f64),
    Boolean(bool),
    Pair(Box<Value>, Box<Value>),
    Vector(Vec<Value>),
    Bytevector(Vec<u8>),
    Symbol(Ident),
    Char(char),
    String(String),
    Builtin(Builtin),
    Closure {
        env: HashMap<Ident, MaybeWeak<RefCell<Value>>>,
        args: Vec<Ident>,
        val: Box<Expr>,
    },
    Nil,
}

impl Value {
    const FALSE: Self = Self::Boolean(false);
    const TRUE: Self = Self::Boolean(true);

    fn number(self) -> Option<f64> {
        match self {
            Self::Number(x) => Some(x),
            _ => None,
        }
    }
}

fn make_list(mut vals: impl Iterator<Item = Value>) -> Value {
    match vals.next() {
        Some(val) => Value::Pair(Box::new(val), Box::new(make_list(vals))),
        None => Value::Nil,
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Number(x) => write!(f, "{}", x),
            Self::Boolean(x) => write!(f, "{}", x),
            Self::Pair(car, cdr) => write!(f, "({} . {})", car, cdr),
            Self::Vector(vec) => {
                write!(f, "#(")?;
                let mut iter = vec.iter().peekable();
                while let Some(val) = iter.next() {
                    if iter.peek().is_some() {
                        write!(f, "{} ", val)?;
                    } else {
                        write!(f, "{}", val)?;
                    }
                }
                write!(f, ")")
            }
            Self::Bytevector(vec) => {
                write!(f, "#u8(")?;
                let mut iter = vec.iter().peekable();
                while let Some(val) = iter.next() {
                    if iter.peek().is_some() {
                        write!(f, "{} ", val)?;
                    } else {
                        write!(f, "{}", val)?;
                    }
                }
                write!(f, ")")
            }
            Self::Symbol(x) => write!(f, "{}", x),
            Self::Char(x) => write!(f, "{}", x),
            Self::String(x) => write!(f, "{}", x),
            Self::Closure { .. } => write!(f, "{{closure}}"),
            Self::Builtin(_) => write!(f, "{{closure}}"),
            Self::Nil => write!(f, "()"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Application {
        procedure: Box<Expr>,
        args: Vec<Expr>,
    },
    Lambda {
        args: Vec<Ident>,
        val: Box<Expr>,
    },
    Let {
        bindings: HashMap<Ident, Expr>,
        val: Box<Expr>,
    },
    LetS {
        bindings: Vec<(Ident, Expr)>,
        val: Box<Expr>,
    },
    Letrec {
        bindings: HashMap<Ident, Expr>,
        val: Box<Expr>,
    },
    LetrecS {
        bindings: Vec<(Ident, Expr)>,
        val: Box<Expr>,
    },
    Define {
        name: Ident,
        val: Box<Expr>,
    },
    If {
        predicate: Box<Expr>,
        t: Box<Expr>,
        f: Box<Expr>,
    },
    Begin {
        ops: Vec<Expr>,
        val: Box<Expr>,
    },
    Set {
        ident: Ident,
        val: Box<Expr>,
    },
    Value(Value),
    Ident(Ident),
    Builtin(Builtin),
}

fn unify(exprs: impl Iterator<Item = Expr>) -> Expr {
    let mut bindings = Vec::new();
    let mut ops = Vec::new();
    for expr in exprs {
        match expr {
            Expr::Define { name, val } => {
                bindings.push((name, *val));
            }
            expr => ops.push(expr),
        }
    }
    Expr::LetrecS {
        bindings,
        val: Box::new(Expr::Begin {
            ops,
            val: Box::new(Expr::Value(Value::Nil)),
        }),
    }
}

// This struct is necessary so that closures bound in `letrec` expressions will
// only hold `Weak` references to themselves, preventing a reference cycle.
#[derive(Debug, Clone)]
pub enum MaybeWeak<T> {
    Strong(Rc<T>),
    Weak(Weak<T>),
}

impl<T: PartialEq<T>> PartialEq<MaybeWeak<T>> for MaybeWeak<T> {
    fn eq(&self, other: &MaybeWeak<T>) -> bool {
        self.get() == other.get()
    }
}

impl<T> MaybeWeak<T> {
    fn new(inner: T) -> Self {
        Self::Strong(Rc::new(inner))
    }

    fn strong(&self) -> Option<Self> {
        match self {
            Self::Strong(rc) => Some(Self::Strong(rc.clone())),
            Self::Weak(weak) => weak.upgrade().map(Self::Strong),
        }
    }

    fn weak(&self) -> Self {
        match self {
            Self::Strong(rc) => Self::Weak(Rc::downgrade(&rc)),
            Self::Weak(weak) => Self::Weak(weak.clone()),
        }
    }

    fn get(&self) -> Rc<T> {
        let Self::Strong(rc) = self.strong().unwrap() else {
            unreachable!();
        };
        rc
    }
}

#[derive(Debug, Default, Clone, PartialEq)]
pub struct Env<'a> {
    outer: Option<&'a Env<'a>>,
    this: HashMap<Ident, MaybeWeak<RefCell<Value>>>,
}

impl<'a> Env<'a> {
    pub fn new() -> Self {
        Self::default()
    }

    fn from_outer(outer: &'a Env) -> Self {
        Self {
            outer: Some(&outer),
            ..Default::default()
        }
    }

    fn layer_with(&'a self, bindings: HashMap<Ident, MaybeWeak<RefCell<Value>>>) -> Self {
        Self {
            outer: Some(&self),
            this: bindings,
        }
    }

    fn get(&self, ident: &str) -> Option<Rc<RefCell<Value>>> {
        self.this
            .get(ident)
            .map(|cell| cell.get())
            .or_else(|| self.outer.as_ref().and_then(|outer| outer.get(ident)))
    }

    // Flatten the environment for closures since the outer env is one scope, improving lookup time.
    fn flatten(&self) -> HashMap<Ident, MaybeWeak<RefCell<Value>>> {
        let mut env = self
            .outer
            .as_ref()
            .map(|env| env.flatten())
            .unwrap_or_default();

        env.extend(self.this.clone().into_iter());
        env
    }
}

impl<'a> fmt::Display for Env<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (k, v) in self.this.iter() {
            write!(f, "{}: {}\n\n", k, v.get().borrow())?;
        }
        self.outer
            .as_ref()
            .map(|outer| write!(f, "{}", outer))
            .unwrap_or(Ok(()))
    }
}

pub fn eval(mut expr: Expr, mut env: Cow<'_, Env>) -> Value {
    loop {
        match expr {
            Expr::Application { procedure, args } => {
                let arg_vals = args.into_iter().map(|arg| eval(arg, Cow::Borrowed(&env)));

                let procedure = eval(*procedure, Cow::Borrowed(&env));

                let (mut closure_env, args, val) = match procedure {
                    Value::Closure { env, args, val } => (env, args, val),
                    Value::Builtin(builtin) => break builtin.eval(&arg_vals.collect::<Vec<_>>()),
                    _ => panic!("Expected closure or builtin, found `{}`", procedure),
                };
                let expected = args.len();
                let got = arg_vals.len();
                if got > expected {
                    panic!("Too many arguments: expected {}, got {}", expected, got);
                } else if got < expected {
                    panic!("Not enough arguments: expected {}, got {}", expected, got);
                }

                closure_env.extend(
                    args.into_iter()
                        .zip(arg_vals.map(|x| MaybeWeak::new(RefCell::new(x)))),
                );

                env = Cow::Owned(Env {
                    outer: None,
                    this: closure_env,
                });
                expr = *val;
                continue;
            }
            Expr::Lambda { args, val } => {
                break Value::Closure {
                    env: env.flatten(),
                    args,
                    val,
                };
            }
            Expr::Let { bindings, val } => {
                let new_env = env.layer_with(
                    bindings
                        .into_iter()
                        .map(|(k, v)| {
                            let v = eval(v, Cow::Borrowed(&env));
                            (k, MaybeWeak::new(RefCell::new(v)))
                        })
                        .collect(),
                );
                break eval(*val, Cow::Borrowed(&new_env));
            }
            Expr::LetS { bindings, val } => {
                let mut new_env = Env::from_outer(&env);
                for (k, v) in bindings.into_iter() {
                    let v = eval(v, Cow::Borrowed(&new_env));
                    new_env.this.insert(k, MaybeWeak::new(RefCell::new(v)));
                }
                break eval(*val, Cow::Borrowed(&new_env));
            }
            Expr::Letrec { bindings, val } => {
                let new_env = Env {
                    outer: Some(&env),
                    this: bindings
                        .iter()
                        .map(|(k, _)| (k.clone(), MaybeWeak::new(RefCell::new(Value::Nil))))
                        .collect(),
                };
                let mut inner_env = new_env.clone();
                inner_env.this.values_mut().for_each(|v| *v = v.weak());

                bindings
                    .into_iter()
                    .map(|(k, v)| (k, eval(v, Cow::Borrowed(&inner_env))))
                    .collect::<Vec<_>>()
                    .into_iter() // ensure that evaluation always occurs before binding
                    .for_each(|(k, v)| *new_env.this[&k].get().borrow_mut() = v);
                break eval(*val, Cow::Borrowed(&new_env));
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

                for (k, v) in bindings.into_iter() {
                    let v = eval(v, Cow::Borrowed(&inner_env));
                    *new_env.this[&k].get().borrow_mut() = v;
                }
                break eval(*val, Cow::Borrowed(&new_env));
                // env = Cow::Owned(new_env);
                // expr = *val;
                // continue;
            }
            Expr::Define { .. } => {
                panic!("`define`s should have been removed by the `unify` function")
            }
            Expr::If { predicate, t, f } => {
                let predicate = eval(*predicate, Cow::Borrowed(&env));
                if predicate == Value::TRUE {
                    expr = *t;
                } else if predicate == Value::FALSE {
                    expr = *f;
                } else {
                    panic!("If conditon must return a boolean")
                }
                continue;
            }
            Expr::Begin { ops, val } => {
                ops.into_iter().for_each(|op| {
                    eval(op, Cow::Borrowed(&env));
                });
                break eval(*val, env);
            }
            Expr::Set { ident, val } => {
                *env.get(&ident).unwrap().borrow_mut() = eval(*val, Cow::Borrowed(&env));
                break Value::Nil;
            }
            Expr::Value(val) => break val,
            Expr::Ident(ident) => {
                break (*env
                    .get(&ident)
                    .expect(&format!("Unbound variable: '{}'", ident)))
                .clone()
                .into_inner();
            }
            Expr::Builtin(builtin) => break Value::Builtin(builtin),
        }
    }
}

pub fn exec(prgm: &str) {
    let time = Instant::now();
    let exprs = parse::parse_prgm(prgm).unwrap().1;
    let ast = unify(exprs.into_iter());
    println!("Parsing time: {:?}", time.elapsed());
    println!("Program output:\n");

    let time = Instant::now();
    eval(ast, Cow::Owned(Env::new()));
    println!("\nEvaluation time: {:?}", time.elapsed());
}
