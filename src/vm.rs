use crate::bytecode::{Call, Chunk, Instruction, InstructionImpl, OpCode};
use anyhow::{anyhow, Context, Result};
use fastrand::Rng;
use num::ToPrimitive;
use std::collections::BTreeMap;
use std::fmt;

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub enum Value {
    Number(f64),
    Boolean(bool),
    Function { n_args: u8, loc: usize },
    IfCond { do_if: bool },
}

impl Value {
    fn to_number(self) -> Result<f64> {
        match self {
            Self::Number(n) => Ok(n),
            unexpected => Err(anyhow!("expected number, found {unexpected}")),
        }
    }

    fn to_bool(self) -> Result<bool> {
        match self {
            Self::Boolean(value) => Ok(value),
            unexpected => Err(anyhow!("expected boolean, found {unexpected}")),
        }
    }

    fn to_func(self) -> Result<(u8, usize)> {
        match self {
            Self::Function { n_args, loc } => Ok((n_args, loc)),
            unexpected => Err(anyhow!("expected function, found {unexpected}")),
        }
    }

    fn to_if_cond(self) -> Result<bool> {
        match self {
            Self::IfCond { do_if } => Ok(do_if),
            unexpected => Err(anyhow!("expected if cond, found {unexpected}")),
        }
    }
}

impl From<f64> for Value {
    fn from(value: f64) -> Self {
        Self::Number(value)
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Self::Boolean(value)
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Number(value) => write!(f, "{value}"),
            Self::Boolean(value) => write!(f, "{value}"),
            Self::Function { .. } => write!(f, "<function>"),
            Self::IfCond { do_if } => write!(f, "IfCond {{ do_if: {do_if} }}"),
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
            self.execute_ins(ins, chunk)?;
        }
        let result = self.pop()?;
        if self.stack.is_empty() {
            Ok(result)
        } else {
            Err(anyhow!("stack not empty upon termination"))
        }
    }

    #[allow(clippy::too_many_lines)]
    fn execute_ins(
        &mut self,
        ins: crate::bytecode::Instruction,
        chunk: &Chunk,
    ) -> Result<(), anyhow::Error> {
        log::trace!("{:10} {:?} {:?}", self.pc, self.stack, ins);
        match ins {
            crate::bytecode::Instruction::NumberLit(lit) => {
                self.stack.push(Value::Number(lit.value));
            }
            crate::bytecode::Instruction::Add(_) => {
                let b = self.pop_number(chunk)?;
                let a = self.pop_number(chunk)?;
                self.stack.push(Value::Number(a + b));
            }
            crate::bytecode::Instruction::Subtract(_) => {
                let b = self.pop_number(chunk)?;
                let a = self.pop_number(chunk)?;
                self.stack.push(Value::Number(a - b));
            }
            crate::bytecode::Instruction::Multiply(_) => {
                let b = self.pop_number(chunk)?;
                let a = self.pop_number(chunk)?;
                self.stack.push(Value::Number(a * b));
            }
            crate::bytecode::Instruction::Divide(_) => {
                let b = self.pop_number(chunk)?;
                let a = self.pop_number(chunk)?;
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
                let b = self.pop_bool(chunk)?;
                let a = self.pop_bool(chunk)?;
                self.stack.push(Value::Boolean(a || b));
            }
            crate::bytecode::Instruction::And(_) => {
                let b = self.pop_bool(chunk)?;
                let a = self.pop_bool(chunk)?;
                self.stack.push(Value::Boolean(a && b));
            }
            crate::bytecode::Instruction::Negate(_) => {
                let a = self.pop_number(chunk)?;
                self.stack.push(Value::Number(-a));
            }
            crate::bytecode::Instruction::Not(_) => {
                let a = self.pop_bool(chunk)?;
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
                self.stack.push(local.value);
            }
            crate::bytecode::Instruction::Random(_) => {
                let n = self
                    .pop_number(chunk)?
                    .to_u64()
                    .context("requested random number too big")?;
                self.stack.push(Value::Number(
                    (self.rng.u64(1..=n))
                        .to_f64()
                        .context("requested random number too big")?,
                ));
            }
            crate::bytecode::Instruction::Call(call) => {
                self.call(&call)?;
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
            crate::bytecode::Instruction::If(_) => {
                if self.pop_bool(chunk)? {
                    self.stack.push(Value::IfCond { do_if: true });
                } else {
                    self.stack.push(Value::IfCond { do_if: false });
                }
            }
            crate::bytecode::Instruction::BeginIf(_) => {
                if !self.pop()?.to_if_cond()? {
                    self.skip_cond(chunk)?;
                    self.stack.push(Value::IfCond { do_if: false });
                }
            }
            crate::bytecode::Instruction::EndIf(_) => {
                self.stack.push(Value::IfCond { do_if: true });
            }
            crate::bytecode::Instruction::BeginElse(_) => {
                if self.pop()?.to_if_cond()? {
                    self.skip_cond(chunk)?;
                }
            }
            crate::bytecode::Instruction::EndElse(_) => {}
        };
        Ok(())
    }

    fn call(&mut self, call: &Call) -> Result<(), anyhow::Error> {
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
        Ok(())
    }

    fn as_number(&mut self, chunk: &Chunk, value: Value) -> Result<f64> {
        match value {
            Value::Number(n) => Ok(n),
            Value::Function { n_args: 0, .. } => {
                self.delegate_no_arg_func(value, chunk)?.to_number()
            }
            unexpected => Err(anyhow!("expected number, found {unexpected}")),
        }
    }

    fn as_bool(&mut self, chunk: &Chunk, value: Value) -> Result<bool> {
        match value {
            Value::Boolean(v) => Ok(v),
            Value::Function { n_args: 0, .. } => self.delegate_no_arg_func(value, chunk)?.to_bool(),
            unexpected => Err(anyhow!("expected boolean, found {unexpected}")),
        }
    }

    fn delegate_no_arg_func(&mut self, value: Value, chunk: &Chunk) -> Result<Value> {
        self.stack.push(value);
        let depth = self.return_addrs.len();
        self.call(&Call { n_args: 0 })?;
        loop {
            if depth == self.return_addrs.len() {
                return self.pop();
            }
            if self.pc >= chunk.data.len() {
                return Err(anyhow!(
                    "unexpected end of program while executing function"
                ));
            }
            let (ins, off) = chunk.read(self.pc)?;
            self.pc += off;
            self.execute_ins(ins, chunk)?;
        }
    }

    fn pop_number(&mut self, chunk: &Chunk) -> Result<f64> {
        let tmp = self.pop()?;
        self.as_number(chunk, tmp)
    }

    fn pop_bool(&mut self, chunk: &Chunk) -> Result<bool> {
        let tmp = self.pop()?;
        self.as_bool(chunk, tmp)
    }

    fn pop(&mut self) -> Result<Value> {
        self.stack.pop().context("tried to pop empty stack")
    }

    fn skip_cond(&mut self, chunk: &Chunk) -> Result<()> {
        let mut depth = 0;
        loop {
            if self.pc >= chunk.data.len() {
                return Err(anyhow!(
                    "unexpected end of program while executing function"
                ));
            }
            let (ins, off) = chunk.read(self.pc)?;
            self.pc += off;
            log::trace!(
                "{:10} {:?} {:?} SKIPPED depth = {}",
                self.pc,
                self.stack,
                ins,
                depth
            );
            match ins {
                Instruction::BeginIf(_) | Instruction::BeginElse(_) => {
                    depth += 1;
                }
                Instruction::EndIf(_) | Instruction::EndElse(_) => {
                    if depth == 0 {
                        return Ok(());
                    }
                    depth -= 1;
                }
                _ => {}
            }
        }
    }
}
