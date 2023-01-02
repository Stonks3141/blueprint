const PRGM: &str = r#"
(define (sum i acc max)
  (if (= i max)
    (+ acc i)
    (sum (+ i 1) (+ acc i) max)))

(display (sum 1 0 100000))
"#;

fn main() {
    let prgm: String = PRGM.lines().filter(|line| !line.starts_with(';')).collect();
    schemers::exec(&prgm);
}
