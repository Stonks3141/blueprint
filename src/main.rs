use clap::{arg, command, value_parser, Command};
use std::{fs, path::PathBuf};

fn cmd() -> Command {
    command!()
        .after_help("Run with no arguments to get a REPL.")
        .arg(arg!(path: [PATH] "The Scheme file to interpret").value_parser(value_parser!(PathBuf)))
}

fn main() -> anyhow::Result<()> {
    if let Some(path) = cmd().get_matches().get_one::<PathBuf>("path") {
        let prgm = fs::read_to_string(path)?;
        if let Err(e) = schemers::exec(&prgm) {
            println!("{}", e);
            std::process::exit(1);
        }
    } else {
        // TODO: repl
    }
    Ok(())
}
