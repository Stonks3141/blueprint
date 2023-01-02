const PRGM: &str = r#"
;(define (sum n)
;  (letrec ((sum-inner (lambda (i acc)
;    (if (= i n)
;      (+ acc i)
;      (sum-inner (+ i 1) (+ acc i))))))
;  (sum-inner 1 0)))
;
;(display (sum 100000))

(if '() '() '()
"#;

fn main() {
    let prgm: String = PRGM.lines().filter(|line| !line.starts_with(';')).collect();
    if let Err(e) = schemers::exec(&prgm) {
        println!("{}", e);
        std::process::exit(1);
    }
}
