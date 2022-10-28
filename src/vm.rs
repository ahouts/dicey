use anyhow::{anyhow, Context, Result};
use std::collections::BTreeMap;
use std::fmt;

use crate::bytecode::Chunk;

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Value {
    Number(f64),
    Boolean(bool),
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
        }
    }
}

#[derive(Debug, Default)]
pub struct Vm {
    stack: Vec<Value>,
    locals: BTreeMap<u32, Value>,
    pc: usize,
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
                    self.locals.insert(local.id, value);
                }
                crate::bytecode::Instruction::PopLocal(local) => {
                    self.locals
                        .remove(&local.id)
                        .with_context(|| anyhow!("tried to pop unknown local {}", local.id))?;
                }
                crate::bytecode::Instruction::LoadLocal(local) => {
                    let value = self
                        .locals
                        .get(&local.id)
                        .with_context(|| anyhow!("tried to load unknown local {}", local.id))?;
                    self.stack.push(value.clone());
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
