use anyhow::Context;
use clap::Parser;
use interpreter::Interpreter;

use std::{io::Read, path::PathBuf};

mod interpreter;

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Gilligan {
    /// Source file to execute. If not provided, the program will be read from stdin.
    source: Option<PathBuf>,
}

fn run(args: &Gilligan) -> anyhow::Result<()> {
    let program_source = match &args.source {
        Some(path) => {
            // Assert the file exists
            anyhow::ensure!(path.exists(), "File not found: {:?}", path);
            std::fs::read_to_string(path).context("Failed to read source file")?
        }
        None => {
            let mut buffer = String::new();
            std::io::stdin()
                .read_to_string(&mut buffer)
                .context("Failed to read from stdin")?;

            buffer
        }
    };

    let mut interpreter = Interpreter::new();
    interpreter.eval(&program_source)?;

    for value in interpreter.exit_values() {
        println!("{}", value);
    }

    Ok(())
}

fn main() {
    let args = Gilligan::parse();
    if let Err(e) = run(&args) {
        eprintln!("{}", e);
        std::process::exit(1);
    }
}
