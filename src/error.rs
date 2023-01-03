use thiserror::Error;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Error, Debug)]
pub enum Error {
    #[error("invalid syntax")]
    Syntax,
    #[error("unbound variable: `{0}`")]
    Unbound(String),
    #[error("unexpected value: expected `{expected}`, found `{found}`")]
    Value { expected: String, found: String },
    #[error("too many arguments: expected {expected}, got {got}")]
    TooManyArguments { expected: usize, got: usize },
    #[error("not enough arguments: expected {expected}, got {got}")]
    NotEnoughArguments { expected: usize, got: usize },
}
