use super::*;
use parse::parse_expr;

#[test]
fn fib() {
    let prgm = r#"
(letrec ((fib (lambda (n)
  (if (or (= n 1) (= n 2))
    1
    (+ (fib (- n 1)) (fib (- n 2)))))))
  (fib 10))
"#;
    let expr = parse_expr(prgm).unwrap().1;
    assert_eq!(
        eval(expr, Cow::Owned(Env::new())).unwrap(),
        Value::Number(55.0)
    );
}

// test for tail call optimization
#[test]
fn sum() {
    let prgm = r#"
(letrec ((sum (lambda (i acc max)
  (if (= i max)
    (+acc i)
    (sum (+ i 1) (+ acc i) max)))))
  (sum 1 0 1000))
"#;
    let expr = parse_expr(prgm).unwrap().1;
    assert_eq!(
        eval(expr, Cow::Owned(Env::new())).unwrap(),
        Value::Number(500500.0)
    );
}

#[test]
fn closure() {
    let prgm = r#"
(let* ((foo (lambda (n) (lambda (x) (+ n x))))
       (my-foo (foo 42)))
         (my-foo 5)))
"#;
    let expr = parse_expr(prgm).unwrap().1;
    assert_eq!(
        eval(expr, Cow::Owned(Env::new())).unwrap(),
        Value::Number(47.0)
    );
}

#[test]
fn nested_let() {
    let prgm = r#"
(let ((foo 4))
  (let ((bar (+ foo 1)))
    bar))
"#;
    let expr = parse_expr(prgm).unwrap().1;
    assert_eq!(
        eval(expr, Cow::Owned(Env::new())).unwrap(),
        Value::Number(5.0)
    );
}
