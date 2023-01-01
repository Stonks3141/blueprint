const PRGM: &str = r#"

(define (fib n)
  (if (or (= n 1) (= n 2))
    1
    (+ (fib (- n 1)) (fib (- n 2)))))

(display (fib 20))
(newline)

(define (sum i acc max)
  (if (= i max)
    (+acc i)
    (sum (+ i 1) (+ acc i) max)))

(display (sum 1 0 10000))
(newline)

(define (foo n) (lambda (x) (+ n x)))

(define my-foo (foo 42))

(display (my-foo 1))
(newline)

"#;

fn main() {
    let prgm: String = PRGM.lines().filter(|line| !line.starts_with(';')).collect();
    schemers::exec(&prgm);
}
