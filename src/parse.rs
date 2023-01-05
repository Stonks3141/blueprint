#[cfg(test)]
mod tests;

use super::{Builtin, Expr, Ident, Value};
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{char, multispace0, none_of, one_of, u8},
    character::streaming,
    combinator::{cut, map, value},
    error::VerboseError,
    multi::{many0, many1},
    number::complete::double,
    sequence::{delimited, pair, preceded, separated_pair, tuple},
    IResult, Parser,
};

fn discard0(i: &str) -> IResult<&'_ str, &'_ str, VerboseError<&'_ str>> {
    // many0(alt((multispace0, preceded(char(';'), not_line_ending))))(i)
    if *crate::REPL.get_or_init(|| false) {
        streaming::multispace0(i) // must be streaming for multiline REPL input
    } else {
        multispace0(i)
    }
}

fn sexp<'a, O, F>(f: F) -> impl FnMut(&'a str) -> IResult<&'a str, O, VerboseError<&'a str>>
where
    F: Parser<&'a str, O, VerboseError<&'a str>>,
{
    preceded(
        discard0,
        delimited(char('('), delimited(discard0, f, discard0), cut(char(')'))),
    )
}

fn parse_boolean(i: &str) -> IResult<&'_ str, Value, VerboseError<&'_ str>> {
    alt((
        value(Value::TRUE, alt((tag("#t"), tag("#true")))),
        value(Value::FALSE, alt((tag("#f"), tag("#false")))),
    ))(i)
}

fn parse_nil(i: &str) -> IResult<&'_ str, Value, VerboseError<&'_ str>> {
    value(Value::Nil, tag("()"))(i)
}

fn parse_char(i: &str) -> IResult<&'_ str, Value, VerboseError<&'_ str>> {
    map(
        preceded(
            tag(r"#\"),
            cut(alt((
                value('\x07', tag("alarm")),
                value('\x08', tag("backspace")),
                value('\x7f', tag("delete")),
                value('\x1b', tag("escape")),
                value('\n', tag("newline")),
                value('\0', tag("null")),
                value('\r', tag("return")),
                value(' ', tag("space")),
                value('\t', tag("tab")),
                none_of("\x07\x08\x7f\x1b\n\0\r\t"),
            ))),
        ),
        Value::Char,
    )(i)
}

// TODO: esaping and escaped quotes
fn parse_string(i: &str) -> IResult<&'_ str, Value, VerboseError<&'_ str>> {
    map(delimited(char('"'), many0(none_of("\"")), char('"')), |x| {
        Value::String(x.into_iter().collect())
    })(i)
}

// TODO: hexadecimal/octal/binary numbers
fn parse_number(i: &str) -> IResult<&'_ str, Value, VerboseError<&'_ str>> {
    alt((
        // map(preceded(tag("0x"), hex_u32), |x| Value::Number(x as f64)),
        // map(preceded(tag("-0x"), hex_u32), |x| Value::Number(-x as f64)),
        map(double, Value::Number),
    ))(i)
}

fn parse_symbol(i: &str) -> IResult<&'_ str, Value, VerboseError<&'_ str>> {
    map(
        many1(one_of(
            "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ+-.*/<=>!?:$%_&~^",
        )),
        |x| Value::Symbol(x.into_iter().collect()),
    )(i)
}

fn parse_vector(i: &str) -> IResult<&'_ str, Value, VerboseError<&'_ str>> {
    map(
        preceded(
            char('#'),
            sexp(cut(many0(delimited(discard0, parse_value_full, discard0)))),
        ),
        Value::Vector,
    )(i)
}

fn parse_bytevector(i: &str) -> IResult<&'_ str, Value, VerboseError<&'_ str>> {
    map(
        preceded(
            tag("#u8"),
            sexp(cut(many0(delimited(discard0, u8, discard0)))),
        ),
        Value::Bytevector,
    )(i)
}

// TODO: parse improper lists like (a b c . d) or (a b . c d)
fn parse_list(i: &str) -> IResult<&'_ str, Value, VerboseError<&'_ str>> {
    map(
        sexp(many0(delimited(discard0, parse_value_full, discard0))),
        |v| crate::make_list(v.into_iter()),
    )(i)
}

fn parse_pair(i: &str) -> IResult<&'_ str, Value, VerboseError<&'_ str>> {
    map(
        sexp(separated_pair(
            parse_value_full,
            delimited(discard0, char('.'), discard0),
            parse_value_full,
        )),
        |(car, cdr)| Value::Pair(Box::new(car), Box::new(cdr)),
    )(i)
}

fn parse_quoted(i: &str) -> IResult<&'_ str, Value, VerboseError<&'_ str>> {
    preceded(
        discard0,
        alt((
            preceded(char('\''), parse_value_full),
            sexp(preceded(tag("quote"), preceded(discard0, parse_value_full))),
        )),
    )(i)
}

fn parse_value(i: &str) -> IResult<&'_ str, Value, VerboseError<&'_ str>> {
    alt((parse_number, parse_boolean, parse_char, parse_string))(i)
}

fn parse_value_full(i: &str) -> IResult<&'_ str, Value, VerboseError<&'_ str>> {
    alt((
        parse_number,
        parse_boolean,
        parse_char,
        parse_string,
        parse_symbol,
        parse_vector,
        parse_bytevector,
        parse_pair,
        parse_list,
        parse_nil,
        parse_quoted,
    ))(i)
}

fn parse_builtin(i: &str) -> IResult<&'_ str, Builtin, VerboseError<&'_ str>> {
    alt((
        value(Builtin::Add, tag("+")),
        value(Builtin::Sub, tag("-")),
        value(Builtin::Mul, tag("*")),
        value(Builtin::Div, tag("/")),
        value(Builtin::Eq, tag("eq?")),
        value(Builtin::Equal, tag("equal?")),
        value(Builtin::NumEq, tag("=")),
        value(Builtin::Or, tag("or")),
        value(Builtin::Display, tag("display")),
        value(Builtin::Newline, tag("newline")),
        value(Builtin::Exit, tag("exit")),
    ))(i)
}

fn parse_ident(i: &str) -> IResult<&'_ str, Ident, VerboseError<&'_ str>> {
    map(
        preceded(
            discard0,
            many1(one_of(
                "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ+-.*/<=>!?:$%_&~^",
            )),
        ),
        |x| x.into_iter().collect(),
    )(i)
}

fn parse_body(i: &str) -> IResult<&'_ str, Expr, VerboseError<&'_ str>> {
    map(
        pair(
            many0(map(parse_define, |expr| match expr {
                Expr::Define { name, val } => (name, *val),
                _ => unreachable!(),
            })),
            parse_expr,
        ),
        |(bindings, val)| {
            if bindings.is_empty() {
                val
            } else {
                Expr::LetrecS {
                    bindings,
                    val: Box::new(val),
                }
            }
        },
    )(i)
}

fn parse_application(i: &str) -> IResult<&'_ str, Expr, VerboseError<&'_ str>> {
    sexp(map(
        pair(parse_expr, many0(parse_expr)),
        |(procedure, args)| Expr::Application {
            procedure: Box::new(procedure),
            args,
        },
    ))(i)
}

fn parse_lambda(i: &str) -> IResult<&'_ str, Expr, VerboseError<&'_ str>> {
    sexp(map(
        preceded(
            tag("lambda"),
            cut(pair(sexp(many0(parse_ident)), parse_body)),
        ),
        |(args, val)| Expr::Lambda {
            args,
            val: Box::new(val),
        },
    ))(i)
}

fn parse_let(i: &str) -> IResult<&'_ str, Expr, VerboseError<&'_ str>> {
    sexp(map(
        preceded(
            tag("let"),
            cut(pair(
                sexp(many1(sexp(pair(parse_ident, parse_expr)))),
                parse_body,
            )),
        ),
        |(bindings, val)| Expr::Let {
            bindings: bindings.into_iter().collect(),
            val: Box::new(val),
        },
    ))(i)
}

fn parse_lets(i: &str) -> IResult<&'_ str, Expr, VerboseError<&'_ str>> {
    sexp(map(
        preceded(
            tag("let*"),
            cut(pair(
                sexp(many1(sexp(pair(parse_ident, parse_expr)))),
                parse_body,
            )),
        ),
        |(bindings, val)| Expr::LetS {
            bindings: bindings.into_iter().collect(),
            val: Box::new(val),
        },
    ))(i)
}

fn parse_letrec(i: &str) -> IResult<&'_ str, Expr, VerboseError<&'_ str>> {
    sexp(map(
        preceded(
            tag("letrec"),
            cut(pair(
                sexp(many1(sexp(pair(parse_ident, parse_expr)))),
                parse_body,
            )),
        ),
        |(bindings, val)| Expr::Letrec {
            bindings: bindings.into_iter().collect(),
            val: Box::new(val),
        },
    ))(i)
}

fn parse_letrecs(i: &str) -> IResult<&'_ str, Expr, VerboseError<&'_ str>> {
    sexp(map(
        preceded(
            tag("letrec*"),
            cut(pair(
                sexp(many1(sexp(pair(parse_ident, parse_expr)))),
                parse_body,
            )),
        ),
        |(bindings, val)| Expr::LetrecS {
            bindings: bindings.into_iter().collect(),
            val: Box::new(val),
        },
    ))(i)
}

fn parse_define(i: &str) -> IResult<&'_ str, Expr, VerboseError<&'_ str>> {
    sexp(preceded(
        tag("define"),
        alt((
            map(pair(parse_ident, parse_expr), |(name, val)| Expr::Define {
                name,
                val: Box::new(val),
            }),
            map(
                pair(sexp(pair(parse_ident, many0(parse_ident))), parse_body),
                |((name, args), val)| Expr::Define {
                    name,
                    val: Box::new(Expr::Lambda {
                        args,
                        val: Box::new(val),
                    }),
                },
            ),
        )),
    ))(i)
}

fn parse_if(i: &str) -> IResult<&'_ str, Expr, VerboseError<&'_ str>> {
    sexp(map(
        preceded(tag("if"), cut(tuple((parse_expr, parse_expr, parse_expr)))),
        |(predicate, t, f)| Expr::If {
            predicate: Box::new(predicate),
            t: Box::new(t),
            f: Box::new(f),
        },
    ))(i)
}

fn parse_begin(i: &str) -> IResult<&'_ str, Expr, VerboseError<&'_ str>> {
    sexp(map(
        preceded(tag("begin"), cut(many1(parse_expr))),
        |mut ops| {
            // many1 guarantees at least one element in ops
            let val = Box::new(ops.pop().unwrap());
            Expr::Begin { ops, val }
        },
    ))(i)
}

fn parse_set(i: &str) -> IResult<&'_ str, Expr, VerboseError<&'_ str>> {
    sexp(map(
        preceded(tag("set!"), cut(pair(parse_ident, parse_expr))),
        |(ident, val)| Expr::Set {
            ident,
            val: Box::new(val),
        },
    ))(i)
}

pub fn parse_expr(i: &str) -> IResult<&'_ str, Expr, VerboseError<&'_ str>> {
    preceded(
        discard0,
        alt((
            parse_lambda,
            parse_letrecs,
            parse_letrec,
            parse_lets,
            parse_let,
            parse_define,
            parse_if,
            parse_begin,
            parse_set,
            map(parse_quoted, Expr::Value),
            // order is important here, `parse_ident` could parse builtins or values as idents,
            // `parse_application` could parse any of the above as applications, and `parse_builtin`
            // could parse a negative integer as subtraction.
            map(parse_value, Expr::Value),
            map(parse_builtin, Expr::Builtin),
            map(parse_ident, Expr::Ident),
            parse_application,
        )),
    )(i)
}

pub fn parse_prgm(i: &str) -> IResult<&'_ str, Vec<Expr>, VerboseError<&'_ str>> {
    cut(many0(parse_expr))(i)
}
