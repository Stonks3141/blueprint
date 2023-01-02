#![allow(dead_code)]

use super::{Builtin, Expr, Ident, Value};
use fxhash::FxHashMap as HashMap;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Opening,
    Closing,
    Ident(Ident),
    Literal(Value),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Sexp {
    Tuple(Vec<Sexp>),
    Ident(Ident),
    Literal(Value),
}

impl Sexp {
    fn ident(self) -> Option<Ident> {
        match self {
            Self::Ident(x) => Some(x),
            _ => None,
        }
    }

    fn tuple(self) -> Option<Vec<Self>> {
        match self {
            Self::Tuple(x) => Some(x),
            _ => None,
        }
    }
}

fn parse_literal(ident: &str) -> Token {
    if let Ok(x) = ident.parse() {
        Token::Literal(Value::Number(x))
    } else if ident.starts_with('"') && ident.ends_with('"') && ident.len() > 1 {
        Token::Literal(Value::String(
            ident[1..(ident.len() - 1)]
                .replace(r#"\""#, "\"")
                .replace(r"\n", "\n")
                .replace(r"\r", "\r"),
        ))
    } else if ident.starts_with(r"#\") {
        Token::Literal(Value::Char(ident.chars().nth(2).unwrap()))
    } else {
        match ident {
            "nil" => Token::Literal(Value::Nil),
            "#t" => Token::Literal(Value::Boolean(true)),
            "#f" => Token::Literal(Value::Boolean(false)),
            _ => Token::Ident(ident.to_owned()),
        }
    }
}

pub fn tokenize<I>(mut src: I, tokens: &mut Vec<Token>) -> I
where
    I: Iterator<Item = char>,
{
    let mut token = String::new();
    let mut quoted = false;
    let mut escaped = false;
    while let Some(c) = src.next() {
        match c {
            '(' => {
                if escaped {
                    escaped = false;
                    token.push('(');
                } else {
                    tokens.push(Token::Opening);
                    src = tokenize(src, tokens);
                }
            }
            ')' => {
                if escaped {
                    escaped = false;
                    token.push(')');
                } else {
                    if !token.is_empty() {
                        tokens.push(parse_literal(&token));
                        token.clear();
                    }
                    tokens.push(Token::Closing);
                    return src;
                }
            }
            '"' => {
                token.push('"');
                if escaped {
                    escaped = false;
                } else {
                    quoted = !quoted;
                }
            }
            '\\' => {
                token.push('\\');
                if quoted {
                    escaped = true;
                }
            }
            c if c.is_whitespace() => {
                if quoted {
                    token.push(c);
                } else if !token.is_empty() {
                    tokens.push(parse_literal(&token));
                    token.clear();
                }
            }
            c => token.push(c),
        }
    }
    src
}

pub fn parse_sexp<I>(mut tokens: I) -> (Sexp, I)
where
    I: Iterator<Item = Token>,
{
    let mut sexp = Vec::new();
    while let Some(token) = tokens.next() {
        match token {
            Token::Opening => {
                let result = parse_sexp(tokens);
                sexp.push(result.0);
                tokens = result.1;
            }
            Token::Closing => return (Sexp::Tuple(sexp), tokens),
            Token::Ident(ident) => sexp.push(Sexp::Ident(ident)),
            Token::Literal(lit) => sexp.push(Sexp::Literal(lit)),
        }
    }
    (Sexp::Tuple(sexp), tokens)
}

// reserved keywords:
// => do or and else quasiquote begin if quote case lambda set!
// cond let unquote define let* unquote-splicing delay letrec

pub fn parse(sexp: Sexp) -> Expr {
    match sexp {
        Sexp::Tuple(a) => match a.get(0) {
            Some(Sexp::Ident(first)) => match first.as_str() {
                "lambda" => Expr::Lambda {
                    args: a[1]
                        .clone()
                        .tuple()
                        .unwrap()
                        .into_iter()
                        .map(|x| x.ident().unwrap())
                        .collect(),
                    val: Box::new(parse(a[2].clone())),
                },
                "let" => Expr::Let {
                    bindings: HashMap::from_iter(a[1].clone().tuple().unwrap().into_iter().map(
                        |x| {
                            let x = x.tuple().unwrap();
                            (x[0].clone().ident().unwrap(), parse(x[1].clone()))
                        },
                    )),
                    val: Box::new(parse(a[2].clone())),
                },
                "define" => match a[1].clone() {
                    Sexp::Ident(name) => Expr::Define {
                        name,
                        val: Box::new(parse(a[2].clone())),
                    },
                    Sexp::Tuple(val) => Expr::Define {
                        name: val[0].clone().ident().unwrap(),
                        val: Box::new(Expr::Lambda {
                            args: val[1..]
                                .iter()
                                .map(|x| x.clone().ident().unwrap())
                                .collect(),
                            val: Box::new(parse(a[2].clone())),
                        }),
                    },
                    _ => panic!(),
                },
                "if" => Expr::If {
                    predicate: Box::new(parse(a[1].clone())),
                    t: Box::new(parse(a[2].clone())),
                    f: Box::new(match a.get(3) {
                        Some(val) => parse(val.clone()),
                        None => Expr::Value(Value::Nil),
                    }),
                },
                "begin" => Expr::Begin {
                    ops: a[1..(a.len() - 1)]
                        .iter()
                        .map(|x| parse(x.clone()))
                        .collect(),
                    val: Box::new(parse(a.last().unwrap().clone())),
                },
                "set!" => Expr::Set {
                    ident: a[1].clone().ident().unwrap(),
                    val: Box::new(parse(a[2].clone())),
                },
                "+" => Expr::Builtin(Builtin::Add),
                "-" => Expr::Builtin(Builtin::Sub),
                "*" => Expr::Builtin(Builtin::Mul),
                "/" => Expr::Builtin(Builtin::Div),
                "=" => Expr::Builtin(Builtin::Eq),
                "or" => Expr::Builtin(Builtin::Or),
                "display" => Expr::Builtin(Builtin::Display),
                "newline" => Expr::Builtin(Builtin::Newline),
                x => Expr::Application {
                    procedure: Box::new(Expr::Ident(x.to_string())),
                    args: a.into_iter().skip(1).map(parse).collect(),
                },
            },
            Some(x) => Expr::Application {
                procedure: Box::new(parse(x.clone())),
                args: a.into_iter().skip(1).map(parse).collect(),
            },
            None => panic!("??"),
        },
        Sexp::Ident(ident) => Expr::Ident(ident),
        Sexp::Literal(val) => Expr::Value(val),
    }
}

pub fn parse_prgm(prgm: &str) -> Vec<Expr> {
    let mut tokens = Vec::new();
    let prgm: String = prgm.lines().filter(|line| !line.starts_with(';')).collect();
    let mut chars = prgm.chars();
    while chars.size_hint().1 != Some(0) {
        chars = tokenize(chars, &mut tokens);
    }

    match parse_sexp(tokens.into_iter()).0 {
        Sexp::Tuple(x) => x.into_iter().map(parse).collect::<Vec<_>>(),
        _ => panic!(),
    }
}
