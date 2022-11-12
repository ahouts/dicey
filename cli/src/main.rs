#![deny(unsafe_code)]
#![deny(
    clippy::all,
    clippy::pedantic,
    clippy::nursery,
    clippy::unwrap_used,
    clippy::expect_used
)]

use anyhow::{Context, Result};
use clap::Parser;
use mimalloc::MiMalloc;
use std::path::PathBuf;

#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

#[derive(Debug, Parser)]
#[command(author, version, about)]
struct Args {
    /// path to prelude file
    #[arg()]
    prelude: Option<PathBuf>,
}

fn main() -> Result<()> {
    let args = Args::try_parse()?;

    let code = if let Some(script) = args.prelude {
        std::fs::read_to_string(script).context("error reading script file")?
    } else {
        String::new()
    };

    loop {
        match run_command(code.as_str()) {
            Ok(value) => {
                eprintln!("{value}");
            }
            Err(err) => {
                eprintln!("{:?}", err);
            }
        }
    }
}

fn run_command(code: &str) -> Result<dicey::Value> {
    let input: String = dialoguer::Input::new()
        .allow_empty(false)
        .with_prompt("dicey")
        .with_initial_text("")
        .interact_text()
        .context("error getting script")?;

    let mut vm = dicey::Vm::default();
    let chunk = dicey::Compiler::default()
        .compile(format!("{code}{input}").as_str())
        .context("error compiling script")?;

    vm.execute(&chunk).context("error executing script")
}
