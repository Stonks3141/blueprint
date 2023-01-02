use thiserror::Error;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Error, Debug)]
pub enum Error {
    #[error("Syntax error")]
    Syntax,
    #[error("Error: unbound variable `{0}`")]
    Unbound(String),
    #[error("Error: expected `{expected}`, found `{found}`")]
    Value { expected: String, found: String },
    #[error("Error: too many arguments: expected {expected}, got {got}")]
    TooManyArguments { expected: usize, got: usize },
    #[error("Error: not enough arguments: expected {expected}, got {got}")]
    NotEnoughArguments { expected: usize, got: usize },
}
