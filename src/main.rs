use blueprint_scm::{eval, exec, parse::parse_expr, Env, Expr, MaybeWeak, Value};
use clap::{arg, command, crate_version, value_parser, Command};
use std::{
    borrow::Cow,
    cell::RefCell,
    fs,
    io::{self, Write},
    path::PathBuf,
};

fn cmd() -> Command {
    command!()
        .after_help("Run with no arguments to get a REPL.")
        .arg(arg!(path: [PATH] "The Scheme file to interpret").value_parser(value_parser!(PathBuf)))
}

fn main() -> anyhow::Result<()> {
    if let Some(path) = cmd().get_matches().get_one::<PathBuf>("path") {
        let prgm = fs::read_to_string(path)?;
        exec(&prgm)?;
    } else {
        println!("blueprint v{}", crate_version!());
        println!("Type (exit) to leave the REPL.");

        let mut stdout = io::stdout();
        let stdin = io::stdin();

        let mut env = Env::new();

        loop {
            write!(stdout, "> ")?;
            stdout.flush()?;
            let mut line = String::new();
            stdin.read_line(&mut line)?;

            let expr = match parse_expr(&line) {
                Ok((_, expr)) => expr,
                Err(e) => {
                    println!("{e}");
                    continue;
                }
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
                        println!("Error: {e}");
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
                    println!("Error: {e}");
                    continue;
                }
            };
            println!("{val}");
        }
    }
    Ok(())
}
