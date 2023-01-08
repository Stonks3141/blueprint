use super::*;
use crate::{error::Result, parse::parse_expr, Number};

fn run(prgm: &str) -> Result<Value> {
    let expr = parse_expr(prgm)?.1;
    eval(expr, Cow::Owned(Env::new()))
}

#[test]
fn fib() -> Result<()> {
    let prgm = r#"
(letrec ((fib (lambda (n)
  (if (or (= n 1) (= n 2))
    1
    (+ (fib (- n 1)) (fib (- n 2)))))))
  (fib 10))
"#;
    assert_eq!(run(prgm)?, Value::Number(Number::from(55)));
    Ok(())
}

// test for tail call optimization
#[test]
fn sum() -> Result<()> {
    let prgm = r#"
(letrec ((sum (lambda (i acc max)
  (if (= i max)
    (+ acc i)
    (sum (+ i 1) (+ acc i) max)))))
  (sum 1 0 1000))
"#;
    assert_eq!(run(prgm)?, Value::Number(Number::from(500500)));
    Ok(())
}

#[test]
fn closure() -> Result<()> {
    let prgm = r#"
(let* ((foo (lambda (n) (lambda (x) (+ n x))))
       (my-foo (foo 42)))
         (my-foo 5)))
"#;
    assert_eq!(run(prgm)?, Value::Number(Number::from(47)));
    Ok(())
}

#[test]
fn nested_let() -> Result<()> {
    let prgm = r#"
(let ((foo 4))
  (let ((bar (+ foo 1)))
    bar))
"#;
    assert_eq!(run(prgm)?, Value::Number(Number::from(5)));
    Ok(())
}

#[test]
fn num_eq() -> Result<()> {
    let prgm = "(= 1 1)";
    assert_eq!(run(prgm)?, Value::TRUE);
    let prgm = "(= 1 -1)";
    assert_eq!(run(prgm)?, Value::FALSE);
    let prgm = "(= 1 'a)";
    assert!(run(prgm).is_err());
    Ok(())
}

#[test]
fn or() -> Result<()> {
    let prgm = "(or)";
    assert_eq!(run(prgm)?, Value::FALSE);
    let prgm = "(or #t)";
    assert_eq!(run(prgm)?, Value::TRUE);
    let prgm = "(or '())";
    assert_eq!(run(prgm)?, Value::Nil);
    let prgm = "(or #f)";
    assert_eq!(run(prgm)?, Value::FALSE);
    let prgm = "(or #f #t)";
    assert_eq!(run(prgm)?, Value::TRUE);
    let prgm = "(or #f 'a)";
    assert_eq!(run(prgm)?, Value::Symbol("a".to_string()));
    let prgm = "(or 'a #f 'b)";
    assert_eq!(run(prgm)?, Value::Symbol("a".to_string()));
    Ok(())
}

#[test]
fn and() -> Result<()> {
    let prgm = "(and)";
    assert_eq!(run(prgm)?, Value::TRUE);
    let prgm = "(and #f)";
    assert_eq!(run(prgm)?, Value::FALSE);
    let prgm = "(and #t)";
    assert_eq!(run(prgm)?, Value::TRUE);
    let prgm = "(and #f)";
    assert_eq!(run(prgm)?, Value::FALSE);
    let prgm = "(and #t 'a)";
    assert_eq!(run(prgm)?, Value::Symbol("a".to_string()));
    Ok(())
}
