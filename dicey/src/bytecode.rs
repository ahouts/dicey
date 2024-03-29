use std::mem::size_of;

use anyhow::{anyhow, Context, Result};
use enum_dispatch::enum_dispatch;
use smartstring::{LazyCompact, SmartString};

#[enum_dispatch(OpCodeImpl)]
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum OpCode {
    NumberLitOp,
    Add,
    Subtract,
    Multiply,
    Divide,
    Equal,
    NotEqual,
    GreaterEqual,
    Greater,
    LessEqual,
    Less,
    Pop,
    Or,
    And,
    Negate,
    Not,
    BooleanLitOp,
    PushLocalOp,
    LoadLocalOp,
    CallOp,
    FunctionOp,
    EndFunction,
    BeginIf,
    EndIf,
    If,
    BeginElse,
    EndElse,
    Mod,
    RollOp,
    BeginList,
    Push,
    Index,
    Strict,
    FieldAccessOp,
    FinalizeList,
    Repeat,
}

impl OpCode {
    fn from_byte(byte: u8) -> Result<Self> {
        Ok(match byte {
            1 => Self::from(NumberLitOp),
            2 => Self::from(Add),
            3 => Self::from(Subtract),
            4 => Self::from(Multiply),
            5 => Self::from(Divide),
            6 => Self::from(Equal),
            7 => Self::from(NotEqual),
            8 => Self::from(GreaterEqual),
            9 => Self::from(Greater),
            10 => Self::from(LessEqual),
            11 => Self::from(Less),
            12 => Self::from(Pop),
            13 => Self::from(Or),
            14 => Self::from(And),
            15 => Self::from(Negate),
            16 => Self::from(Not),
            17 => Self::from(BooleanLitOp),
            18 => Self::from(PushLocalOp),
            20 => Self::from(LoadLocalOp),
            22 => Self::from(CallOp),
            23 => Self::from(FunctionOp),
            24 => Self::from(EndFunction),
            25 => Self::from(BeginIf),
            26 => Self::from(EndIf),
            27 => Self::from(If),
            28 => Self::from(BeginElse),
            29 => Self::from(EndElse),
            30 => Self::from(Mod),
            31 => Self::from(RollOp),
            32 => Self::from(BeginList),
            33 => Self::from(Push),
            34 => Self::from(Index),
            35 => Self::from(Strict),
            36 => Self::from(FieldAccessOp),
            37 => Self::from(FinalizeList),
            38 => Self::from(Repeat),
            _ => return Err(anyhow!("unknown opcode {byte}")),
        })
    }
}

#[enum_dispatch]
pub trait OpCodeImpl {
    fn byte(self) -> u8;
    fn read(self, buffer: &[u8], chunk: &Chunk) -> Result<(Instruction, usize)>;
}

#[enum_dispatch(InstructionImpl)]
#[derive(Debug, Clone)]
pub enum Instruction {
    NumberLit,
    Add,
    Subtract,
    Multiply,
    Divide,
    Equal,
    NotEqual,
    GreaterEqual,
    Greater,
    LessEqual,
    Less,
    Pop,
    Or,
    And,
    Negate,
    Not,
    BooleanLit,
    PushLocal,
    LoadLocal,
    Call,
    Function,
    EndFunction,
    BeginIf,
    EndIf,
    If,
    BeginElse,
    EndElse,
    Mod,
    Roll,
    BeginList,
    Push,
    Index,
    Strict,
    FieldAccess,
    FinalizeList,
    Repeat,
}

#[enum_dispatch]
pub trait InstructionImpl {
    fn op_code(&self) -> OpCode;
    fn write(&self, chunk: &mut Chunk) -> Result<()>;
}

macro_rules! dataless_opcode {
    ($t:ident, $code:literal) => {
        #[derive(Debug, Default, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
        pub struct $t;

        impl OpCodeImpl for $t {
            fn byte(self) -> u8 {
                $code
            }

            fn read(self, _: &[u8], _: &Chunk) -> Result<(Instruction, usize)> {
                Ok((Instruction::from($t), 0))
            }
        }

        impl InstructionImpl for $t {
            fn op_code(&self) -> OpCode {
                OpCode::from($t)
            }

            fn write(&self, _: &mut Chunk) -> Result<()> {
                Ok(())
            }
        }
    };
}

#[derive(Debug, Default, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct NumberLitOp;

impl OpCodeImpl for NumberLitOp {
    fn byte(self) -> u8 {
        1
    }

    fn read(self, buffer: &[u8], _: &Chunk) -> Result<(Instruction, usize)> {
        const SIZE: usize = size_of::<f64>();
        if buffer.len() < SIZE {
            Err(anyhow!("incomplete number literal at end of bytecode"))
        } else {
            Ok((
                Instruction::from(NumberLit {
                    value: f64::from_be_bytes(
                        buffer[..SIZE].try_into().context("internal VM error")?,
                    ),
                }),
                SIZE,
            ))
        }
    }
}

#[derive(Debug, Clone)]
pub struct NumberLit {
    pub value: f64,
}

impl InstructionImpl for NumberLit {
    fn op_code(&self) -> OpCode {
        OpCode::from(NumberLitOp)
    }

    fn write(&self, chunk: &mut Chunk) -> Result<()> {
        for byte in f64::to_be_bytes(self.value) {
            chunk.data.push(byte);
        }
        Ok(())
    }
}

dataless_opcode!(Add, 2);
dataless_opcode!(Subtract, 3);
dataless_opcode!(Multiply, 4);
dataless_opcode!(Divide, 5);
dataless_opcode!(Equal, 6);
dataless_opcode!(NotEqual, 7);
dataless_opcode!(GreaterEqual, 8);
dataless_opcode!(Greater, 9);
dataless_opcode!(LessEqual, 10);
dataless_opcode!(Less, 11);
dataless_opcode!(Pop, 12);
dataless_opcode!(Or, 13);
dataless_opcode!(And, 14);
dataless_opcode!(Negate, 15);
dataless_opcode!(Not, 16);

#[derive(Debug, Default, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct BooleanLitOp;

impl OpCodeImpl for BooleanLitOp {
    fn byte(self) -> u8 {
        17
    }

    fn read(self, buffer: &[u8], _: &Chunk) -> Result<(Instruction, usize)> {
        if buffer.is_empty() {
            Err(anyhow!("incomplete boolean literal at end of bytecode"))
        } else {
            Ok((
                Instruction::from(BooleanLit {
                    value: buffer[0] != 0,
                }),
                1,
            ))
        }
    }
}

#[derive(Debug, Clone)]
pub struct BooleanLit {
    pub value: bool,
}

impl InstructionImpl for BooleanLit {
    fn op_code(&self) -> OpCode {
        OpCode::from(BooleanLitOp)
    }

    fn write(&self, chunk: &mut Chunk) -> Result<()> {
        chunk.data.push(u8::from(self.value));
        Ok(())
    }
}

#[derive(Debug, Default, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct PushLocalOp;

impl OpCodeImpl for PushLocalOp {
    fn byte(self) -> u8 {
        18
    }

    fn read(self, buffer: &[u8], _: &Chunk) -> Result<(Instruction, usize)> {
        if buffer.len() < 4 {
            Err(anyhow!("incomplete push local at end of bytecode"))
        } else {
            Ok((
                Instruction::from(PushLocal {
                    id: u32::from_be_bytes(buffer[..4].try_into().context("internal VM error")?),
                }),
                4,
            ))
        }
    }
}

#[derive(Debug, Clone)]
pub struct PushLocal {
    pub id: u32,
}

impl InstructionImpl for PushLocal {
    fn op_code(&self) -> OpCode {
        OpCode::from(PushLocalOp)
    }

    fn write(&self, chunk: &mut Chunk) -> Result<()> {
        for byte in u32::to_be_bytes(self.id) {
            chunk.data.push(byte);
        }
        Ok(())
    }
}

#[derive(Debug, Default, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct LoadLocalOp;

impl OpCodeImpl for LoadLocalOp {
    fn byte(self) -> u8 {
        20
    }

    fn read(self, buffer: &[u8], _: &Chunk) -> Result<(Instruction, usize)> {
        if buffer.len() < 4 {
            Err(anyhow!("incomplete load local at end of bytecode"))
        } else {
            Ok((
                Instruction::from(LoadLocal {
                    id: u32::from_be_bytes(buffer[..4].try_into().context("internal VM error")?),
                }),
                4,
            ))
        }
    }
}

#[derive(Debug, Clone)]
pub struct LoadLocal {
    pub id: u32,
}

impl InstructionImpl for LoadLocal {
    fn op_code(&self) -> OpCode {
        OpCode::from(LoadLocalOp)
    }

    fn write(&self, chunk: &mut Chunk) -> Result<()> {
        for byte in u32::to_be_bytes(self.id) {
            chunk.data.push(byte);
        }
        Ok(())
    }
}

#[derive(Debug, Default, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct CallOp;

impl OpCodeImpl for CallOp {
    fn byte(self) -> u8 {
        22
    }

    fn read(self, buffer: &[u8], _: &Chunk) -> Result<(Instruction, usize)> {
        if buffer.is_empty() {
            Err(anyhow!("incomplete call at end of bytecode"))
        } else {
            Ok((Instruction::from(Call { n_args: buffer[0] }), 1))
        }
    }
}

#[derive(Debug, Clone)]
pub struct Call {
    pub n_args: u8,
}

impl InstructionImpl for Call {
    fn op_code(&self) -> OpCode {
        OpCode::from(CallOp)
    }

    fn write(&self, chunk: &mut Chunk) -> Result<()> {
        chunk.data.push(self.n_args);
        Ok(())
    }
}

#[derive(Debug, Default, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct FunctionOp;

impl OpCodeImpl for FunctionOp {
    fn byte(self) -> u8 {
        23
    }

    fn read(self, buffer: &[u8], _: &Chunk) -> Result<(Instruction, usize)> {
        if buffer.is_empty() {
            Err(anyhow!("incomplete function at end of bytecode"))
        } else {
            Ok((Instruction::from(Function { n_args: buffer[0] }), 1))
        }
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub n_args: u8,
}

impl InstructionImpl for Function {
    fn op_code(&self) -> OpCode {
        OpCode::from(FunctionOp)
    }

    fn write(&self, chunk: &mut Chunk) -> Result<()> {
        chunk.data.push(self.n_args);
        Ok(())
    }
}

dataless_opcode!(EndFunction, 24);
dataless_opcode!(BeginIf, 25);
dataless_opcode!(EndIf, 26);
dataless_opcode!(If, 27);
dataless_opcode!(BeginElse, 28);
dataless_opcode!(EndElse, 29);
dataless_opcode!(Mod, 30);

#[derive(Debug, Default, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct RollOp;

impl OpCodeImpl for RollOp {
    fn byte(self) -> u8 {
        31
    }

    fn read(self, buffer: &[u8], _: &Chunk) -> Result<(Instruction, usize)> {
        if buffer.len() < 2 {
            Err(anyhow!("incomplete roll at end of bytecode"))
        } else {
            Ok((
                Instruction::from(Roll {
                    n: buffer[0],
                    d: buffer[1],
                }),
                2,
            ))
        }
    }
}

#[derive(Debug, Clone)]
pub struct Roll {
    pub n: u8,
    pub d: u8,
}

impl InstructionImpl for Roll {
    fn op_code(&self) -> OpCode {
        OpCode::from(RollOp)
    }

    fn write(&self, chunk: &mut Chunk) -> Result<()> {
        chunk.data.push(self.n);
        chunk.data.push(self.d);
        Ok(())
    }
}

dataless_opcode!(BeginList, 32);
dataless_opcode!(Push, 33);
dataless_opcode!(Index, 34);
dataless_opcode!(Strict, 35);

#[derive(Debug, Default, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct FieldAccessOp;

impl OpCodeImpl for FieldAccessOp {
    fn byte(self) -> u8 {
        36
    }

    fn read(self, buffer: &[u8], chunk: &Chunk) -> Result<(Instruction, usize)> {
        if buffer.is_empty() {
            Err(anyhow!("incomplete roll at end of bytecode"))
        } else {
            Ok((
                Instruction::from(FieldAccess {
                    field: chunk.fields[buffer[0] as usize].clone(),
                }),
                1,
            ))
        }
    }
}

#[derive(Debug, Clone)]
pub struct FieldAccess {
    pub field: SmartString<LazyCompact>,
}

impl InstructionImpl for FieldAccess {
    fn op_code(&self) -> OpCode {
        OpCode::from(FieldAccessOp)
    }

    fn write(&self, chunk: &mut Chunk) -> Result<()> {
        let idx = if let Some((idx, _)) = chunk
            .fields
            .iter()
            .enumerate()
            .find(|(_, name)| *name == &self.field)
        {
            idx
        } else {
            let idx = chunk.fields.len();
            chunk.fields.push(self.field.clone());
            idx
        };
        chunk.data.push(idx.try_into().context("too many fields")?);
        Ok(())
    }
}

dataless_opcode!(FinalizeList, 37);
dataless_opcode!(Repeat, 38);

#[derive(Debug, Default, Clone, Eq, PartialEq)]
pub struct Chunk {
    pub data: Vec<u8>,
    pub fields: Vec<SmartString<LazyCompact>>,
}

impl Chunk {
    pub fn push(&mut self, ins: impl Into<Instruction>) -> Result<()> {
        let ins = ins.into();
        let code = ins.op_code().byte();
        self.data.push(code);
        ins.write(self)?;
        Ok(())
    }

    pub fn read(&self, index: usize) -> Result<(Instruction, usize)> {
        let code = self.data.get(index).context("read past end of chunk")?;
        let op_code = OpCode::from_byte(*code)?;
        let (ins, off) = op_code.read(&self.data[(index + 1)..], self)?;
        Ok((ins, off + 1))
    }
}
