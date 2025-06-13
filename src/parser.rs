use crate::SymbolTable;
use logos::{Lexer, Logos};
use snafu::Snafu;
use std::num::ParseIntError;

#[derive(Snafu, Default, Debug, Clone, PartialEq)]
pub(crate) enum LexingError {
    #[snafu(display("invalid integer: {message}"))]
    InvalidInteger { message: String },
    #[snafu(display("{message}"))]
    NotFoundToken { message: String },
    #[default]
    NonAsciiCharacter,
}

impl From<ParseIntError> for LexingError {
    fn from(err: ParseIntError) -> Self {
        use std::num::IntErrorKind::*;
        match err.kind() {
            PosOverflow | NegOverflow => LexingError::InvalidInteger {
                message: "overflow error".to_owned(),
            },
            _ => LexingError::InvalidInteger {
                message: "other error".to_owned(),
            },
        }
    }
}

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(skip r"([ \t\n\f]+)|//[^\n]*")]
#[logos(error = LexingError)]
pub(crate) enum Token {
    #[token("@", label_callback)]
    At(Label),
    #[token("(", label_callback)]
    LParen(Label),
    #[token(")")]
    RParen,
    #[regex("[ADM]+=", |lex| lex.slice().to_owned())]
    Dest(String),
    #[regex("[ADM]?[-+!&|]?[01ADM]", |lex| lex.slice().to_owned())]
    Comp(String),
    #[regex(";J(GT|EQ|GE|LT|NE|LE|MP)", |lex| lex.slice().to_owned())]
    Jump(String),
}

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(skip r"[ \t\n\f]+")]
#[logos(error = LexingError)]
pub(crate) enum Label {
    #[regex("[0-9]+", |lex| lex.slice().parse())]
    Literal(u16),
    #[regex("R([0-9]|1[0-5])", |lex| lex.slice().to_owned())]
    Predefined(String),
    #[regex("[a-zA-Z][a-zA-Z0-9_.$]*", |lex| lex.slice().to_owned())]
    Label(String),
}

fn label_callback(lex: &mut Lexer<Token>) -> Result<Label, LexingError> {
    let mut label_lexer = lex.clone().morph::<Label>();
    let Some(token) = label_lexer.next() else {
        return Err(LexingError::NotFoundToken {
            message: "except a label".to_owned(),
        });
    };
    *lex = label_lexer.morph();

    let mut peekable = lex.peekable();
    if let Some(token) = peekable.peek() {
        match token {
            Ok(Token::RParen) => {
                lex.next();
            }
            _ => {}
        }
    }

    token
}

#[derive(Snafu, Debug)]
pub enum ParsingError {
    #[snafu(display("error while parsing: no `comp` expression"))]
    NoCompExprError,
    #[snafu(display("trying to reference overflowed address: {address:016b}"))]
    AddressOverflow { address: u16 },
    #[snafu(display("invalid syntax"))]
    SyntaxError,
}

pub enum Instruction {
    A {
        address: u16,
    },
    C {
        c_bit: ControlBit,
        comp: CInstrComp,
        dest: CInstrDest,
        jump: CInstrJump,
    },
}

#[repr(u16)]
pub enum ControlBit {
    Address = 0b111_0_000000000000,
    Memory = 0b111_1_000000000000,
}

#[repr(u16)]
pub enum CInstrComp {
    Zero = 0b101010_000000,
    One = 0b111111_000000,
    NeOne = 0b111010_000000,
    D = 0b001100_000000,
    AM = 0b110000_000000,
    NotD = 0b001101_000000,
    NotAM = 0b110001_000000,
    NeD = 0b001111_000000,
    NeAM = 0b110011_000000,
    DAddOne = 0b011111_000000,
    AMAddOne = 0b110111_000000,
    DSubOne = 0b001110_000000,
    AMSubOne = 0b110010_000000,
    DAddAM = 0b000010_000000,
    DSubAM = 0b010011_000000,
    AMSubD = 0b000111_000000,
    DAndAM = 0b000000_000000,
    DOrAM = 0b010101_000000,
}

impl From<&str> for CInstrComp {
    fn from(value: &str) -> Self {
        match value {
            "0" => Self::Zero,
            "1" => Self::One,
            "-1" => Self::NeOne,
            "D" => Self::D,
            "A" | "M" => Self::AM,
            "!D" => Self::NotD,
            "!A" | "!M" => Self::NotAM,
            "-D" => Self::NeD,
            "-A" | "-M" => Self::NeAM,
            "D+1" => Self::DAddOne,
            "A+1" | "M+1" => Self::AMAddOne,
            "D-1" => Self::DSubOne,
            "A-1" | "M-1" => Self::AMSubOne,
            "D+A" | "D+M" => Self::DAddAM,
            "D-A" | "D-M" => Self::DSubAM,
            "A-D" | "M-D" => Self::AMSubD,
            "D&A" | "D&M" => Self::DAndAM,
            "D|A" | "D|M" => Self::DOrAM,
            _ => unreachable!(),
        }
    }
}

#[repr(u16)]
pub enum CInstrDest {
    Null = 0b000_000,
    M = 0b001_000,
    D = 0b010_000,
    DM = 0b011_000,
    A = 0b100_000,
    AM = 0b101_000,
    AD = 0b110_000,
    ADM = 0b111_000,
}

impl From<Option<&str>> for CInstrDest {
    fn from(value: Option<&str>) -> Self {
        let Some(value) = value else {
            return Self::Null;
        };
        match value {
            "M=" => Self::M,
            "D=" => Self::D,
            "DM=" | "MD=" => Self::DM,
            "A=" => Self::A,
            "AM=" | "MA=" => Self::AM,
            "AD=" | "DA=" => Self::AD,
            "ADM=" | "AMD=" | "DAM=" | "DMA=" | "MAD=" | "MDA=" => Self::ADM,
            _ => unreachable!(),
        }
    }
}

#[repr(u16)]
pub enum CInstrJump {
    Null = 0b000,
    JGT = 0b001,
    JEQ = 0b010,
    JGE = 0b011,
    JLT = 0b100,
    JNE = 0b101,
    JLE = 0b110,
    JMP = 0b111,
}

impl From<Option<&str>> for CInstrJump {
    fn from(value: Option<&str>) -> Self {
        let Some(value) = value else {
            return Self::Null;
        };
        match value {
            ";JGT" => Self::JGT,
            ";JEQ" => Self::JEQ,
            ";JGE" => Self::JGE,
            ";JLT" => Self::JLT,
            ";JNE" => Self::JNE,
            ";JLE" => Self::JLE,
            ";JMP" => Self::JMP,
            _ => unreachable!(),
        }
    }
}

impl Instruction {
    fn a(address: u16) -> Self {
        Self::A { address }
    }

    fn c(comp: &str, dest: Option<&str>, jump: Option<&str>) -> Self {
        let c_bit = if comp.contains("M") {
            ControlBit::Memory
        } else {
            ControlBit::Address
        };
        let comp: CInstrComp = comp.into();
        let dest: CInstrDest = dest.into();
        let jump: CInstrJump = jump.into();
        Self::C {
            c_bit,
            comp,
            dest,
            jump,
        }
    }

    pub fn encode(self) -> u16 {
        match self {
            Instruction::A { address } => address.to_owned(),
            Instruction::C {
                c_bit,
                comp,
                dest,
                jump,
            } => (c_bit as u16) | (comp as u16) | (dest as u16) | (jump as u16),
        }
    }
}

fn parse_address(label: &Label, symbol_table: &mut SymbolTable) -> Result<u16, ParsingError> {
    match label {
        Label::Literal(address) => {
            if (address >> 15) == 1 {
                return Err(ParsingError::AddressOverflow {
                    address: address.clone(),
                });
            }
            Ok(address.clone())
        }
        Label::Predefined(label) => {
            let address = symbol_table.get(label).expect("expect predefined");
            Ok(address.clone())
        }
        Label::Label(label) => {
            let address = match symbol_table.get(label) {
                None => symbol_table.new_pointer(label.clone()),
                Some(address) => address.clone()
            };
            Ok(address)
        }
    }
}

pub fn parse(tokens: &Vec<Token>, symbol_table: &mut SymbolTable) -> Result<Instruction, ParsingError> {
    match tokens.len() {
        1 => match tokens.first().expect("expect one token") {
            Token::At(a_instr) => {
                let address = parse_address(a_instr, symbol_table)?;
                Ok(Instruction::a(address))
            }
            Token::Comp(comp) => Ok(Instruction::c(comp, None, None)),
            _ => unreachable!(),
        },
        2 => match tokens.first().expect("expect first token") {
            Token::Dest(dest) => {
                let Token::Comp(comp) = &tokens[1] else {
                    return Err(ParsingError::SyntaxError);
                };
                Ok(Instruction::c(comp, Some(dest), None))
            }
            Token::Comp(comp) => {
                let Token::Jump(jump) = &tokens[1] else {
                    return Err(ParsingError::SyntaxError);
                };
                Ok(Instruction::c(comp, None, Some(jump)))
            }
            _ => unreachable!(),
        },
        3 => {
            let (Token::Dest(dest), Token::Comp(comp), Token::Jump(jump)) =
                (&tokens[0], &tokens[1], &tokens[2])
            else {
                return Err(ParsingError::SyntaxError);
            };
            Ok(Instruction::c(comp, Some(dest), Some(jump)))
        }
        _ => unreachable!(),
    }
}
