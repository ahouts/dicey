use crate::bytecode::{Chunk, InstructionImpl, OpCode};
use anyhow::{anyhow, Context, Result};
use fastrand::Rng;
use std::collections::BTreeMap;
use std::fmt;

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Value {
    Number(f64),
    Boolean(bool),
    Function { n_args: u8, loc: usize },
}

impl Value {
    fn to_number(self) -> Result<f64> {
        match self {
            Value::Number(n) => Ok(n),
            unexpected => Err(anyhow!("expected number, found {unexpected}")),
        }
    }

    fn to_bool(self) -> Result<bool> {
        match self {
            Value::Boolean(value) => Ok(value),
            unexpected => Err(anyhow!("expected boolean, found {unexpected}")),
        }
    }

    fn to_func(self) -> Result<(u8, usize)> {
        match self {
            Value::Function { n_args, loc } => Ok((n_args, loc)),
            unexpected => Err(anyhow!("expected function, found {unexpected}")),
        }
    }
}

impl From<f64> for Value {
    fn from(value: f64) -> Self {
        Value::Number(value)
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Value::Boolean(value)
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Number(value) => write!(f, "{value}"),
            Value::Boolean(value) => write!(f, "{value}"),
            Value::Function { .. } => write!(f, "<function>"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Ord, Eq, Hash)]
enum Reachability {
    Stack,
    Unknown,
}

#[derive(Debug, Clone)]
struct Local {
    value: Value,
    reachability: Reachability,
}

#[derive(Debug, Default)]
pub struct Vm {
    stack: Vec<Value>,
    locals: BTreeMap<u32, Local>,
    return_addrs: Vec<usize>,
    pc: usize,
    pub(crate) rng: Rng,
}

impl Vm {
    pub fn execute(&mut self, chunk: &Chunk) -> Result<Value> {
        self.pc = 0;
        loop {
            if self.pc >= chunk.data.len() {
                break;
            }
            let (ins, off) = chunk.read(self.pc)?;
            self.pc += off;
            log::trace!("{:10} {:?} {:?}", self.pc, self.stack, ins);
            match ins {
                crate::bytecode::Instruction::NumberLit(lit) => {
                    self.stack.push(Value::Number(lit.value));
                }
                crate::bytecode::Instruction::Add(_) => {
                    let b = self.pop()?.to_number()?;
                    let a = self.pop()?.to_number()?;
                    self.stack.push(Value::Number(a + b));
                }
                crate::bytecode::Instruction::Subtract(_) => {
                    let b = self.pop()?.to_number()?;
                    let a = self.pop()?.to_number()?;
                    self.stack.push(Value::Number(a - b));
                }
                crate::bytecode::Instruction::Multiply(_) => {
                    let b = self.pop()?.to_number()?;
                    let a = self.pop()?.to_number()?;
                    self.stack.push(Value::Number(a * b));
                }
                crate::bytecode::Instruction::Divide(_) => {
                    let b = self.pop()?.to_number()?;
                    let a = self.pop()?.to_number()?;
                    self.stack.push(Value::Number(a / b));
                }
                crate::bytecode::Instruction::Equal(_) => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    self.stack.push(Value::Boolean(a == b));
                }
                crate::bytecode::Instruction::NotEqual(_) => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    self.stack.push(Value::Boolean(a != b));
                }
                crate::bytecode::Instruction::GreaterEqual(_) => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    self.stack.push(Value::Boolean(a >= b));
                }
                crate::bytecode::Instruction::Greater(_) => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    self.stack.push(Value::Boolean(a > b));
                }
                crate::bytecode::Instruction::LessEqual(_) => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    self.stack.push(Value::Boolean(a <= b));
                }
                crate::bytecode::Instruction::Less(_) => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    self.stack.push(Value::Boolean(a < b));
                }
                crate::bytecode::Instruction::Pop(_) => {
                    self.pop()?;
                }
                crate::bytecode::Instruction::Or(_) => {
                    let b = self.pop()?.to_bool()?;
                    let a = self.pop()?.to_bool()?;
                    self.stack.push(Value::Boolean(a || b));
                }
                crate::bytecode::Instruction::And(_) => {
                    let b = self.pop()?.to_bool()?;
                    let a = self.pop()?.to_bool()?;
                    self.stack.push(Value::Boolean(a && b));
                }
                crate::bytecode::Instruction::Negate(_) => {
                    let a = self.pop()?.to_number()?;
                    self.stack.push(Value::Number(-a));
                }
                crate::bytecode::Instruction::Not(_) => {
                    let a = self.pop()?.to_bool()?;
                    self.stack.push(Value::Boolean(!a));
                }
                crate::bytecode::Instruction::BooleanLit(lit) => {
                    self.stack.push(Value::Boolean(lit.value));
                }
                crate::bytecode::Instruction::PushLocal(local) => {
                    let value = self.pop()?;
                    self.locals.insert(
                        local.id,
                        Local {
                            value,
                            reachability: Reachability::Stack,
                        },
                    );
                }
                crate::bytecode::Instruction::PopLocal(local) => {
                    let local = self
                        .locals
                        .get_mut(&local.id)
                        .with_context(|| anyhow!("tried to pop unknown local {}", local.id))?;
                    local.reachability = Reachability::Unknown;
                }
                crate::bytecode::Instruction::LoadLocal(local) => {
                    let local = self
                        .locals
                        .get(&local.id)
                        .with_context(|| anyhow!("tried to load unknown local {}", local.id))?;
                    self.stack.push(local.value.clone());
                }
                crate::bytecode::Instruction::Random(_) => {
                    let n = self.pop()?.to_number()? as u64;
                    self.stack.push(Value::Number((self.rng.u64(1..=n)) as f64));
                }
                crate::bytecode::Instruction::Call(call) => {
                    let mut args = Vec::new();
                    for _ in 0..call.n_args {
                        args.push(self.pop().context("not enough arguments for call")?);
                    }
                    let (n_args, offset) = self
                        .pop()
                        .context("tried to call with empty stack")?
                        .to_func()?;
                    if n_args != call.n_args {
                        return Err(anyhow!(
                            "called function with incorrect number of arguments"
                        ));
                    }
                    self.return_addrs.push(self.pc);
                    self.pc = offset;
                    args.into_iter().rev().for_each(|arg| self.stack.push(arg));
                }
                crate::bytecode::Instruction::Function(func) => {
                    self.stack.push(Value::Function {
                        n_args: func.n_args,
                        loc: self.pc,
                    });
                    let mut callstack_depth = 1;
                    loop {
                        let (ins, off) = chunk.read(self.pc)?;
                        self.pc += off;
                        callstack_depth += match ins.op_code() {
                            OpCode::FunctionOp(_) => 1,
                            OpCode::EndFunction(_) => -1,
                            _ => 0,
                        };
                        if callstack_depth == 0 {
                            break;
                        }
                    }
                }
                crate::bytecode::Instruction::EndFunction(_) => {
                    self.pc = self
                        .return_addrs
                        .pop()
                        .context("returned when not in function call")?;
                }
            }
        }
        let result = self.pop()?;
        if self.stack.is_empty() {
            Ok(result)
        } else {
            Err(anyhow!("stack not empty upon termination"))
        }
    }

    fn pop(&mut self) -> Result<Value> {
        self.stack.pop().context("tried to pop empty stack")
    }
}
