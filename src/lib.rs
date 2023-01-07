mod builtin;
pub mod error;
mod eval;
mod number;
pub mod parse;

pub use eval::eval;
pub use number::Number;

use builtin::Builtin;
use error::{Error, Result};
use fxhash::FxHashMap as HashMap;
use once_cell::sync::OnceCell;
use std::{
    borrow::Cow,
    cell::RefCell,
    fmt,
    rc::{Rc, Weak},
};

pub static REPL: OnceCell<bool> = OnceCell::new();

pub type Ident = String;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(Number),
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

    fn number(&self) -> Result<Number> {
        match self {
            Self::Number(x) => Ok(*x),
            val => Err(Error::Value {
                expected: "number".to_string(),
                found: format!("{val}"),
            }),
        }
    }

    fn boolean(&self) -> Result<bool> {
        match self {
            Self::Boolean(x) => Ok(*x),
            val => Err(Error::Value {
                expected: "boolean".to_string(),
                found: format!("{val}"),
            }),
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
            Self::Char(x) => write!(f, "'{}'", x),
            Self::String(x) => write!(f, "\"{}\"", x),
            Self::Closure { .. } | Self::Builtin(_) => write!(f, "{{closure}}"),
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
    pub fn new(inner: T) -> Self {
        Self::Strong(Rc::new(inner))
    }

    pub fn strong(&self) -> Option<Self> {
        match self {
            Self::Strong(rc) => Some(Self::Strong(rc.clone())),
            Self::Weak(weak) => weak.upgrade().map(Self::Strong),
        }
    }

    pub fn weak(&self) -> Self {
        match self {
            Self::Strong(rc) => Self::Weak(Rc::downgrade(rc)),
            Self::Weak(weak) => Self::Weak(weak.clone()),
        }
    }

    pub fn get(&self) -> Rc<T> {
        let Self::Strong(rc) = self.strong().unwrap() else {
            unreachable!();
        };
        rc
    }
}

#[derive(Debug, Default, Clone, PartialEq)]
pub struct Env<'a> {
    pub outer: Option<&'a Env<'a>>,
    pub this: HashMap<Ident, MaybeWeak<RefCell<Value>>>,
}

impl<'a> Env<'a> {
    pub fn new() -> Self {
        Self::default()
    }

    fn from_outer(outer: &'a Env) -> Self {
        Self {
            outer: Some(outer),
            ..Default::default()
        }
    }

    fn layer_with(&'a self, bindings: HashMap<Ident, MaybeWeak<RefCell<Value>>>) -> Self {
        Self {
            outer: Some(self),
            this: bindings,
        }
    }

    fn get(&self, ident: &str) -> Option<Rc<RefCell<Value>>> {
        self.this
            .get(ident)
            .map(MaybeWeak::get)
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
            .map_or(Ok(()), |outer| write!(f, "{}", outer))
    }
}

pub fn exec(prgm: &str) -> Result<()> {
    let prgm: String = prgm.lines().filter(|line| !line.starts_with(';')).collect();
    let exprs = parse::parse_prgm(&prgm).map_err(|_| Error::Syntax)?.1;
    let ast = unify(exprs.into_iter());
    eval(ast, Cow::Owned(Env::new()))?;
    Ok(())
}
