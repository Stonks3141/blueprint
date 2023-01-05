use super::*;
use crate::parse::parse_expr;

#[test]
fn fib() -> anyhow::Result<()> {
    let prgm = r#"
(letrec ((fib (lambda (n)
  (if (or (= n 1) (= n 2))
    1
    (+ (fib (- n 1)) (fib (- n 2)))))))
  (fib 10))
"#;
    let expr = parse_expr(prgm)?.1;
    assert_eq!(eval(expr, Cow::Owned(Env::new()))?, Value::Number(55.0));
    Ok(())
}

// test for tail call optimization
#[test]
fn sum() -> anyhow::Result<()> {
    let prgm = r#"
(letrec ((sum (lambda (i acc max)
  (if (= i max)
    (+acc i)
    (sum (+ i 1) (+ acc i) max)))))
  (sum 1 0 1000))
"#;
    let expr = parse_expr(prgm)?.1;
    assert_eq!(eval(expr, Cow::Owned(Env::new()))?, Value::Number(500500.0));
    Ok(())
}

#[test]
fn closure() -> anyhow::Result<()> {
    let prgm = r#"
(let* ((foo (lambda (n) (lambda (x) (+ n x))))
       (my-foo (foo 42)))
         (my-foo 5)))
"#;
    let expr = parse_expr(prgm)?.1;
    assert_eq!(eval(expr, Cow::Owned(Env::new()))?, Value::Number(47.0));
    Ok(())
}

#[test]
fn nested_let() -> anyhow::Result<()> {
    let prgm = r#"
(let ((foo 4))
  (let ((bar (+ foo 1)))
    bar))
"#;
    let expr = parse_expr(prgm)?.1;
    assert_eq!(eval(expr, Cow::Owned(Env::new()))?, Value::Number(5.0));
    Ok(())
}

#[test]
fn num_eq() -> anyhow::Result<()> {
    let prgm = "(= 1 1)";
    let expr = parse_expr(prgm)?.1;
    assert_eq!(eval(expr, Cow::Owned(Env::new()))?, Value::TRUE);
    let prgm = "(= 1 -1)";
    let expr = parse_expr(prgm)?.1;
    assert_eq!(eval(expr, Cow::Owned(Env::new()))?, Value::FALSE);
    let prgm = "(= 1 'a)";
    let expr = parse_expr(prgm)?.1;
    assert!(eval(expr, Cow::Owned(Env::new())).is_err());
    Ok(())
}