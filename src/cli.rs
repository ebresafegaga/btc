use std::{error, fmt, fs::File, io::Read, str::FromStr};

// A simple cli
use clap::Parser;

use crate::{parser::grammar, typing};

// btc parse <file>
// btc typecheck <file>
// btc exec <file>
// btc repl

#[derive(Debug, Default)]
struct InvalidInput;

impl fmt::Display for InvalidInput {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "invalid command line input")
    }
}

impl error::Error for InvalidInput {}

#[derive(Debug, Default)]
enum Mode {
    Parse,
    #[default]
    Typecheck,
}

impl FromStr for Mode {
    type Err = InvalidInput;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "parse" => Ok(Mode::Parse),
            "typecheck" => Ok(Mode::Typecheck),
            _ => Err(InvalidInput),
        }
    }
}

impl fmt::Display for Mode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Mode::Parse => write!(f, "parse"),
            Mode::Typecheck => write!(f, "typecheck"),
        }
    }
}

#[derive(Parser, Default, Debug)]
pub struct Arguments {
    mode: Mode,
    path: String,
}

pub fn parse(_source: &str) {}

pub fn typecheck(source: &str) {
    match grammar::ProgramParser::new().parse(source) {
        Err(e) => println!("{:?}", e),
        Ok(program) => match typing::typecheck_program(&program) {
            Err(e) => println!("A type error occured: \n {:?}", e),
            Ok(ctx) => {
                for (name, ty) in ctx.iter().rev() {
                    match ty {
                        typing::Binding::TyStruct(..) => {
                            println!("{} defined", name)
                        }
                        typing::Binding::VarExpr(ty) => {
                            println!("{} : {}", name, ty)
                        }
                    }
                }
            }
        },
    }
}

pub fn run() {
    let args = Arguments::parse();

    let mut file = File::open(args.path).unwrap();
    let mut source = String::new();
    file.read_to_string(&mut source).unwrap();
    match args.mode {
        Mode::Parse => parse(&source),
        Mode::Typecheck => typecheck(&source),
    }
}
