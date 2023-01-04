use quick_error::quick_error;

pub type Result<T> = std::result::Result<T, Error>;

quick_error! {
    #[derive(Debug)]
    pub enum Error {
        Syntax {
            display("invalid syntax")
        }
        Unbound(name: String) {
            display("unbound variable: `{name}`")
        }
        Value { expected: String, found: String } {
            display("unexpected value: expected `{expected}`, found `{found}`")
        }
        TooManyArguments { expected: usize, got: usize } {
            display("too many arguments: expected {expected}, got {got}")
        }
        NotEnoughArguments { expected: usize, got: usize } {
            display("not enough arguments: expected {expected}, got {got}")
        }
    }
}
