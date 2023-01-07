use super::*;
use crate::{make_list, Expr, Value};

#[test]
fn bool() -> anyhow::Result<()> {
    let prgm = "#f";
    assert_eq!(parse_expr(prgm)?.1, Expr::Value(Value::Boolean(false)));
    let prgm = "#false";
    assert_eq!(parse_expr(prgm)?.1, Expr::Value(Value::Boolean(false)));
    let prgm = "#t";
    assert_eq!(parse_expr(prgm)?.1, Expr::Value(Value::Boolean(true)));
    let prgm = "#true";
    assert_eq!(parse_expr(prgm)?.1, Expr::Value(Value::Boolean(true)));
    Ok(())
}

#[test]
fn nil() -> anyhow::Result<()> {
    let prgm = "'()";
    assert_eq!(parse_expr(prgm)?.1, Expr::Value(Value::Nil));
    Ok(())
}

#[test]
fn char() -> anyhow::Result<()> {
    let prgm = r"#\alarm";
    assert_eq!(parse_expr(prgm)?.1, Expr::Value(Value::Char('\x07')));
    let prgm = r"#\backspace";
    assert_eq!(parse_expr(prgm)?.1, Expr::Value(Value::Char('\x08')));
    let prgm = r"#\delete";
    assert_eq!(parse_expr(prgm)?.1, Expr::Value(Value::Char('\x7f')));
    let prgm = r"#\escape";
    assert_eq!(parse_expr(prgm)?.1, Expr::Value(Value::Char('\x1b')));
    let prgm = r"#\newline";
    assert_eq!(parse_expr(prgm)?.1, Expr::Value(Value::Char('\n')));
    let prgm = r"#\null";
    assert_eq!(parse_expr(prgm)?.1, Expr::Value(Value::Char('\0')));
    let prgm = r"#\return";
    assert_eq!(parse_expr(prgm)?.1, Expr::Value(Value::Char('\r')));
    let prgm = r"#\space";
    assert_eq!(parse_expr(prgm)?.1, Expr::Value(Value::Char(' ')));
    let prgm = r"#\tab";
    assert_eq!(parse_expr(prgm)?.1, Expr::Value(Value::Char('\t')));
    let prgm = r"#\ ";
    assert_eq!(parse_expr(prgm)?.1, Expr::Value(Value::Char(' ')));
    let prgm = r"#\a";
    assert_eq!(parse_expr(prgm)?.1, Expr::Value(Value::Char('a')));
    Ok(())
}

#[test]
fn string() -> anyhow::Result<()> {
    let prgm = r#""hi""#;
    assert_eq!(
        parse_expr(prgm)?.1,
        Expr::Value(Value::String("hi".to_string()))
    );
    Ok(())
}

#[ignore = "string escapes are not implemented"]
#[test]
fn string_escapes() -> anyhow::Result<()> {
    let prgm = r#""h\"i\r\n""#;
    assert_eq!(
        parse_expr(prgm)?.1,
        Expr::Value(Value::String("h\"i\r\n".to_string()))
    );
    Ok(())
}

#[test]
fn number() -> anyhow::Result<()> {
    let prgm = "0";
    assert_eq!(
        parse_expr(prgm)?.1,
        Expr::Value(Value::Number(Number::from(0.0)))
    );
    let prgm = "0.0";
    assert_eq!(
        parse_expr(prgm)?.1,
        Expr::Value(Value::Number(Number::from(0.0)))
    );
    let prgm = "-1";
    assert_eq!(
        parse_expr(prgm)?.1,
        Expr::Value(Value::Number(Number::from(-1.0)))
    );
    let prgm = "1e-1";
    assert_eq!(
        parse_expr(prgm)?.1,
        Expr::Value(Value::Number(Number::from(0.1)))
    );
    Ok(())
}

#[test]
fn vector() -> anyhow::Result<()> {
    let prgm = "'#(1 2 3)";
    assert_eq!(
        parse_expr(prgm)?.1,
        Expr::Value(Value::Vector(vec![
            Value::Number(Number::from(1.0)),
            Value::Number(Number::from(2.0)),
            Value::Number(Number::from(3.0)),
        ])),
    );
    let prgm = "'#()";
    assert_eq!(parse_expr(prgm)?.1, Expr::Value(Value::Vector(vec![])));
    let prgm = "'#(a #(()))";
    assert_eq!(
        parse_expr(prgm)?.1,
        Expr::Value(Value::Vector(vec![
            Value::Symbol("a".to_string()),
            Value::Vector(vec![Value::Nil]),
        ])),
    );
    Ok(())
}

#[test]
fn bytevector() -> anyhow::Result<()> {
    let prgm = "'#u8(1 2 3)";
    assert_eq!(
        parse_expr(prgm)?.1,
        Expr::Value(Value::Bytevector(vec![1, 2, 3])),
    );
    let prgm = "'#u8()";
    assert_eq!(parse_expr(prgm)?.1, Expr::Value(Value::Bytevector(vec![])));
    Ok(())
}

#[test]
fn list() -> anyhow::Result<()> {
    let prgm = "'(1 2 3)";
    assert_eq!(
        parse_expr(prgm)?.1,
        Expr::Value(make_list(
            vec![
                Value::Number(Number::from(1.0)),
                Value::Number(Number::from(2.0)),
                Value::Number(Number::from(3.0)),
            ]
            .into_iter()
        )),
    );
    let prgm = "'(a (()))";
    assert_eq!(
        parse_expr(prgm)?.1,
        Expr::Value(make_list(
            vec![
                Value::Symbol("a".to_string()),
                make_list(vec![Value::Nil].into_iter()),
            ]
            .into_iter()
        )),
    );
    Ok(())
}

#[test]
fn pair() -> anyhow::Result<()> {
    let prgm = "'(1 . (2 . (3 . ())))";
    assert_eq!(
        parse_expr(prgm)?.1,
        Expr::Value(make_list(
            vec![
                Value::Number(Number::from(1.0)),
                Value::Number(Number::from(2.0)),
                Value::Number(Number::from(3.0)),
            ]
            .into_iter()
        )),
    );
    let prgm = "'(a . ((() . ()) . ()))";
    assert_eq!(
        parse_expr(prgm)?.1,
        Expr::Value(make_list(
            vec![
                Value::Symbol("a".to_string()),
                make_list(vec![Value::Nil].into_iter()),
            ]
            .into_iter()
        )),
    );
    Ok(())
}
