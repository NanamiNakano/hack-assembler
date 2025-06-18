mod parser;

use crate::parser::{Label, LexingError, ParsingError, Token, parse};
use clap::Parser;
use logos::{Logos, Span};
use snafu::{ResultExt, Snafu, whatever};
use std::collections::HashMap;
use std::fs;
use std::fs::{File, OpenOptions};
use std::io::{BufRead, BufReader, Write};
use std::path::{Path, PathBuf};

#[derive(Parser, Debug)]
struct Args {
    #[arg(short, long)]
    input: String,
    #[arg(short, long)]
    output: Option<String>,
    #[arg(long, action)]
    overrides: bool,
}

#[derive(Snafu, Debug)]
enum Error {
    #[snafu(display("error while i/o operation: {path}: {message}"))]
    IOError {
        source: std::io::Error,
        path: String,
        message: String,
    },

    #[snafu(display("error while lexing at {line}:{span:?}: {source}"))]
    LexingError {
        source: LexingError,
        span: Span,
        line: usize,
    },

    #[snafu(display("error while parsing line {line}: {source}"))]
    ParsingError { source: ParsingError, line: usize },

    #[snafu(whatever, display("{message}"))]
    Whatever {
        message: String,
        #[snafu(source(from(Box<dyn std::error::Error>, Some)))]
        source: Option<Box<dyn std::error::Error>>,
    },
}

#[snafu::report]
fn main() -> Result<(), Error> {
    let args = Args::parse();

    let input_file_path = Path::new(&args.input);
    let input = File::open(input_file_path).context(IOSnafu {
        path: &args.input,
        message: "can not open file".to_owned(),
    })?;
    let input = BufReader::new(input).lines();

    let mut symbol_table = SymbolTable::new();
    let mut program = vec![];
    'line: for (line_number, line) in input.enumerate() {
        let line = line.context(IOSnafu {
            path: &args.input,
            message: "can not read line".to_owned(),
        })?;
        let lex = Token::lexer(&line);
        let mut tokens = vec![];
        for (token, span) in lex.spanned() {
            let token = token.context(LexingSnafu {
                span,
                line: line_number + 1,
            })?;
            match &token {
                Token::LParen(Label::Label(label)) => {
                    if symbol_table.get(label).is_some() {
                        whatever!("{} is defined multiple times", label);
                    }
                    symbol_table.insert(label.to_owned(), program.len() as u16);
                    continue 'line;
                }
                _ => {}
            }
            tokens.push(token)
        }
        if !tokens.is_empty() {
            program.push((line_number + 1, tokens));
        }
    }

    let output_file_path = match args.output {
        None => input_file_path.with_extension("hack"),
        Some(path) => PathBuf::from(path),
    };
    let mut output = if args.overrides {
        OpenOptions::new()
            .write(true)
            .create(true)
            .open(&output_file_path)
            .context(IOSnafu {
                path: output_file_path.to_string_lossy(),
                message: "can not open file".to_owned(),
            })?
    } else {
        if fs::exists(&output_file_path).context(IOSnafu {
            path: output_file_path.to_string_lossy(),
            message: "can not open file".to_owned(),
        })? {
            whatever!("Output file already exist! Override using `--overrides` flag.")
        };
        OpenOptions::new()
            .write(true)
            .create(true)
            .open(&output_file_path)
            .context(IOSnafu {
                path: output_file_path.to_string_lossy(),
                message: "can not open file".to_owned(),
            })?
    };
    let mut binary = vec![];
    for (line_number, line) in program {
        let instruction =
            parse(&line, &mut symbol_table).context(ParsingSnafu { line: line_number })?;
        let encoded = instruction.encode();
        binary.push(encoded)
    }
    for code in binary {
        output
            .write_fmt(format_args!("{code:016b}\n"))
            .context(IOSnafu {
                path: output_file_path.to_string_lossy(),
                message: "can not write to file".to_owned(),
            })?
    }
    Ok(())
}

pub struct SymbolTable {
    pub table: HashMap<String, u16>,
    pointer: u16,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            table: HashMap::from([
                ("R0".to_owned(), 0),
                ("R1".to_owned(), 1),
                ("R2".to_owned(), 2),
                ("R3".to_owned(), 3),
                ("R4".to_owned(), 4),
                ("R5".to_owned(), 5),
                ("R6".to_owned(), 6),
                ("R7".to_owned(), 7),
                ("R8".to_owned(), 8),
                ("R9".to_owned(), 9),
                ("R10".to_owned(), 10),
                ("R11".to_owned(), 11),
                ("R12".to_owned(), 12),
                ("R13".to_owned(), 13),
                ("R14".to_owned(), 14),
                ("R15".to_owned(), 15),
                ("SCREEN".to_owned(), 16384),
                ("KBD".to_owned(), 24576),
                ("SP".to_owned(), 0),
                ("LCL".to_owned(), 1),
                ("ARG".to_owned(), 2),
                ("THIS".to_owned(), 3),
                ("THAT".to_owned(), 4),
            ]),
            pointer: 15,
        }
    }
    pub fn insert(&mut self, k: String, v: u16) {
        self.table.insert(k, v);
    }
    pub fn get(&self, k: &String) -> Option<&u16> {
        self.table.get(k)
    }
    pub fn len(&self) -> u16 {
        self.table.len() as u16
    }
    pub fn new_pointer(&mut self, label: String) -> u16 {
        self.pointer = self.pointer + 1;
        self.table.insert(label, self.pointer);
        self.pointer
    }
}
