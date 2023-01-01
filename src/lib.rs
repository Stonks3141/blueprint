use builtin::Builtin;
use fxhash::FxHashMap as HashMap;
use std::{
    cell::RefCell,
    fmt,
    rc::{Rc, Weak},
    time::Instant,
};

mod builtin;
mod parse;
mod parse_old;

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
    Closure {
        env: Env,
        args: Vec<Ident>,
        val: Box<Expr>,
    },
    Builtin(Builtin),
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
    let mut bindings = HashMap::default();
    let mut ops = Vec::new();
    for expr in exprs {
        match expr {
            Expr::Define { name, val } => {
                bindings.insert(name, *val);
            }
            expr => ops.push(expr),
        }
    }
    Expr::Letrec {
        bindings,
        val: Box::new(Expr::Begin {
            ops,
            val: Box::new(Expr::Value(Value::Nil)),
        }),
    }
}

#[derive(Debug, Clone)]
pub enum MaybeWeak<T> {
    Strong(Rc<T>),
    Weak(Weak<T>),
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

#[derive(Debug, Default, Clone)]
pub struct Env {
    outer: Option<Box<Env>>,
    this: HashMap<Ident, MaybeWeak<RefCell<Value>>>,
}

impl PartialEq<Env> for Env {
    fn eq(&self, other: &Env) -> bool {
        self.this
            .iter()
            .zip(other.this.iter())
            .fold(true, |acc, (a, b)| {
                a.0 == b.0 && *a.1.get() == *b.1.get() && acc
            })
            && self.outer == other.outer
    }
}

impl Env {
    pub fn new() -> Self {
        Self::default()
    }

    fn from_outer(outer: Env) -> Self {
        Self {
            outer: Some(Box::new(outer)),
            this: HashMap::default(),
        }
    }

    fn get(&self, ident: &str) -> Option<Rc<RefCell<Value>>> {
        self.this
            .get(ident)
            .map(|cell| cell.get())
            .or_else(|| self.outer.as_ref().and_then(|outer| outer.get(ident)))
    }

    fn flatten(&self) -> HashMap<Ident, MaybeWeak<RefCell<Value>>> {
        let mut map = self
            .outer
            .as_ref()
            .map(|env| env.flatten())
            .unwrap_or_default();

        map.extend(self.this.clone().into_iter());
        map
    }
}

impl fmt::Display for Env {
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

pub fn eval(mut expr: Expr, mut env: Env) -> Value {
    loop {
        match expr {
            Expr::Application { procedure, args } => {
                let args = args
                    .into_iter()
                    .map(|arg| eval(arg, Env::from_outer(env.clone())))
                    .collect::<Vec<_>>();

                let procedure = eval(*procedure, Env::from_outer(env.clone()));

                let (closure_env, arg_names, val) = match procedure {
                    Value::Closure { env, args, val } => (env, args, *val),
                    Value::Builtin(builtin) => break builtin.eval(&args),
                    _ => panic!("Expected closure or builtin, found `{}`", procedure),
                };
                env = closure_env;

                let expected = arg_names.len();
                let got = args.len();
                if got > expected {
                    panic!("Too many arguments: expected {}, got {}", expected, got);
                } else if got < expected {
                    panic!("Not enough arguments: expected {}, got {}", expected, got);
                }

                env.this.extend(
                    arg_names
                        .into_iter()
                        .zip(args.into_iter().map(|x| MaybeWeak::new(RefCell::new(x)))),
                );

                expr = val;
                continue;
            }
            Expr::Lambda { args, val } => {
                break Value::Closure {
                    env: Env {
                        outer: None,
                        this: env.flatten(),
                    },
                    args,
                    val,
                };
            }
            Expr::Let { bindings, val } => {
                env = Env {
                    this: bindings
                        .into_iter()
                        .map(|(k, v)| {
                            let v = eval(v, Env::from_outer(env.clone()));
                            (k, MaybeWeak::new(RefCell::new(v)))
                        })
                        .collect(),
                    outer: Some(Box::new(env)),
                };
                break eval(*val, Env::from_outer(env.clone()));
            }
            Expr::LetS { bindings, val } => {
                env = Env::from_outer(env);
                for (k, v) in bindings.into_iter() {
                    let v = eval(v, Env::from_outer(env.clone()));
                    env.this.insert(k, MaybeWeak::new(RefCell::new(v)));
                }
                break eval(*val, env.clone());
            }
            Expr::Letrec { bindings, val } => {
                env = Env::from_outer(env);
                env.this.extend(
                    bindings
                        .keys()
                        .map(|k| (k.clone(), MaybeWeak::new(RefCell::new(Value::Nil)))),
                );
                let mut inner_env = env.clone();
                inner_env.this.values_mut().for_each(|v| *v = v.weak());

                for (k, v) in bindings.into_iter() {
                    let v = eval(v, Env::from_outer(inner_env.clone()));
                    *env.this[&k].get().borrow_mut() = v;
                }
                break eval(*val, env.clone());
            }
            Expr::Define { .. } => {
                panic!("`define`s should have been removed by the `unify` function")
            }
            Expr::If { predicate, t, f } => {
                let predicate = eval(*predicate, Env::from_outer(env.clone()));
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
                    eval(op, Env::from_outer(env.clone()));
                });
                break eval(*val, Env::from_outer(env.clone()));
            }
            Expr::Set { ident, val } => {
                *env.get(&ident).unwrap().borrow_mut() = eval(*val, Env::from_outer(env.clone()));
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
    eval(ast, Env::new());
    println!("\nEvaluation time: {:?}", time.elapsed());
}
