use blueprint_scheme::{eval, exec, parse::parse_expr, Env, Expr, MaybeWeak, Value, REPL};
use quick_error::quick_error;
use std::{
    borrow::Cow,
    cell::RefCell,
    fs,
    io::{self, Write},
    path::PathBuf,
};

quick_error! {
    #[derive(Debug)]
    enum Error {
        Io(err: std::io::Error) {
            display("{err}")
            source(err)
            from()
        }
        Interpreter(err: blueprint_scheme::error::Error) {
            display("{err}")
            source(err)
            from()
        }
    }
}

fn main() -> Result<(), Error> {
    let flags = xflags::parse_or_exit! {
        /// Print the version and exit.
        optional -V,--version
        /// Path to the Scheme file to execute
        optional path: PathBuf
    };

    if flags.version {
        println!("{}", env!("CARGO_PKG_VERSION"));
        return Ok(());
    }

    if let Some(path) = flags.path {
        REPL.set(false).unwrap();
        let prgm = fs::read_to_string(path)?;
        exec(&prgm)?;
    } else {
        REPL.set(true).unwrap();
        println!("blueprint v{}", env!("CARGO_PKG_VERSION"));
        println!("Type (exit) to leave the REPL.");

        let mut stdout = io::stdout();
        let stdin = io::stdin();

        let mut env = Env::new();

        'main: loop {
            write!(stdout, "> ")?;
            stdout.flush()?;
            let mut line = String::new();
            let expr = loop {
                stdin.read_line(&mut line)?;

                break match parse_expr(&line) {
                    Ok((_, expr)) => expr,
                    Err(e) => match e {
                        nom::Err::Incomplete(_) => continue,
                        nom::Err::Error(_) | nom::Err::Failure(_) => {
                            println!("error: invalid syntax");
                            continue 'main;
                        }
                    },
                };
            };

            if let Expr::Define { name, val } = expr {
                env.this
                    .insert(name.clone(), MaybeWeak::new(RefCell::new(Value::Nil)));
                let mut weak_env = env.clone();
                let v = weak_env.this.get_mut(&name).unwrap();
                *v = v.weak();

                let val = match eval(*val, Cow::Borrowed(&weak_env)) {
                    Ok(val) => val,
                    Err(e) => {
                        println!("error: {e}");
                        continue;
                    }
                };
                println!("{val}");
                *env.this.get_mut(&name).unwrap().get().borrow_mut() = val;
                continue;
            }

            let val = match eval(expr, Cow::Borrowed(&env)) {
                Ok(val) => val,
                Err(e) => {
                    println!("error: {e}");
                    continue;
                }
            };
            println!("{val}");
        }
    }
    Ok(())
}
