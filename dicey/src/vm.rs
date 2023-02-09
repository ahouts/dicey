use crate::bytecode::{Call, Chunk, Instruction, InstructionImpl, OpCode, Roll};
use anyhow::{anyhow, Context, Result};
use fastrand::Rng;
use num::ToPrimitive;
use std::collections::BTreeMap;
use std::fmt;
use std::sync::{Arc, Mutex, MutexGuard};

#[derive(Clone)]
pub enum Value {
    Number(f64),
    Boolean(bool),
    Function { n_args: u8, loc: usize },
    IfCond { do_if: bool },
    List(Arc<Mutex<Vec<Value>>>),
}

impl Value {
    pub fn to_number(&self) -> Result<f64> {
        match self {
            Self::Number(n) => Ok(*n),
            unexpected => Err(anyhow!("expected number, found {unexpected}")),
        }
    }

    fn to_func(&self) -> Result<(u8, usize)> {
        match self {
            Self::Function { n_args, loc } => Ok((*n_args, *loc)),
            unexpected => Err(anyhow!("expected function, found {unexpected}")),
        }
    }

    fn to_if_cond(&self) -> Result<bool> {
        match self {
            Self::IfCond { do_if } => Ok(*do_if),
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

impl PartialEq for Value {
    #[allow(clippy::significant_drop_in_scrutinee)]
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Number(n1), Self::Number(n2)) => n1 == n2,
            (Self::Boolean(b1), Self::Boolean(b2)) => b1 == b2,
            (Self::List(l1), Self::List(l2)) => match (l1.lock(), l2.lock()) {
                (Ok(g1), Ok(g2)) => g1.as_slice() == g2.as_slice(),
                _ => false,
            },
            _ => false,
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Self::Number(n1), Self::Number(n2)) => n1.partial_cmp(n2),
            (Self::Boolean(b1), Self::Boolean(b2)) => b1.partial_cmp(b2),
            _ => None,
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Number(value) => write!(f, "{value}"),
            Self::Boolean(value) => write!(f, "{value}"),
            Self::Function { .. } => write!(f, "<function>"),
            Self::IfCond { do_if } => write!(f, "IfCond {{ do_if: {do_if} }}"),
            Self::List(reference) => {
                write!(f, "[ ")?;
                match reference.lock() {
                    Ok(ls) => {
                        for value in &*ls {
                            write!(f, "{value}, ")?;
                        }
                    }
                    Err(_) => {
                        write!(f, "POISON!")?;
                    }
                }
                write!(f, "]")?;
                Ok(())
            }
        }
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self}")
    }
}

#[derive(Debug, Clone)]
struct Local {
    value: Value,
}

#[derive(Debug, Clone)]
struct FuncScope {
    locals: BTreeMap<u32, Local>,
    return_addr: usize,
}

#[derive(Debug, Default)]
pub struct Vm {
    stack: Vec<Value>,
    globals: BTreeMap<u32, Local>,
    scopes: Vec<FuncScope>,
    pc: usize,
    pub(crate) rng: Rng,
    recurse_depth: u8,
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

        let result = loop {
            let result = self.pop()?;

            match result {
                result @ Value::Function { n_args: 0, .. } => {
                    self.recurse_depth = self.recurse_depth.saturating_add(1);
                    let res = self.delegate_to_no_arg_func(result, chunk)?;
                    self.stack.push(res);
                }
                result => break result,
            };
        };

        if self.stack.is_empty() {
            Ok(result)
        } else {
            Err(anyhow!("stack not empty upon termination"))
        }
    }

    #[allow(clippy::too_many_lines)]
    fn execute_ins(&mut self, ins: Instruction, chunk: &Chunk) -> Result<(), anyhow::Error> {
        log::trace!("{:10} {:?} {:?}", self.pc, self.stack, ins);
        match ins {
            Instruction::NumberLit(lit) => {
                self.stack.push(Value::Number(lit.value));
            }
            Instruction::Add(_) => {
                let b = self.pop_number(chunk)?;
                let a = self.pop_number(chunk)?;
                self.stack.push(Value::Number(a + b));
            }
            Instruction::Subtract(_) => {
                let b = self.pop_number(chunk)?;
                let a = self.pop_number(chunk)?;
                self.stack.push(Value::Number(a - b));
            }
            Instruction::Multiply(_) => {
                let b = self.pop_number(chunk)?;
                let a = self.pop_number(chunk)?;
                self.stack.push(Value::Number(a * b));
            }
            Instruction::Divide(_) => {
                let b = self.pop_number(chunk)?;
                let a = self.pop_number(chunk)?;
                self.stack.push(Value::Number(a / b));
            }
            Instruction::Equal(_) => {
                let b = self.pop_comparable(chunk)?;
                let a = self.pop_comparable(chunk)?;
                self.stack.push(Value::Boolean(a == b));
            }
            Instruction::NotEqual(_) => {
                let b = self.pop_comparable(chunk)?;
                let a = self.pop_comparable(chunk)?;
                self.stack.push(Value::Boolean(a != b));
            }
            Instruction::GreaterEqual(_) => {
                let b = self.pop_comparable(chunk)?;
                let a = self.pop_comparable(chunk)?;
                self.stack.push(Value::Boolean(a >= b));
            }
            Instruction::Greater(_) => {
                let b = self.pop_comparable(chunk)?;
                let a = self.pop_comparable(chunk)?;
                self.stack.push(Value::Boolean(a > b));
            }
            Instruction::LessEqual(_) => {
                let b = self.pop_comparable(chunk)?;
                let a = self.pop_comparable(chunk)?;
                self.stack.push(Value::Boolean(a <= b));
            }
            Instruction::Less(_) => {
                let b = self.pop_comparable(chunk)?;
                let a = self.pop_comparable(chunk)?;
                self.stack.push(Value::Boolean(a < b));
            }
            Instruction::Pop(_) => {
                self.pop()?;
            }
            Instruction::Or(_) => {
                let b = self.pop_bool(chunk)?;
                let a = self.pop_bool(chunk)?;
                self.stack.push(Value::Boolean(a || b));
            }
            Instruction::And(_) => {
                let b = self.pop_bool(chunk)?;
                let a = self.pop_bool(chunk)?;
                self.stack.push(Value::Boolean(a && b));
            }
            Instruction::Negate(_) => {
                let a = self.pop_number(chunk)?;
                self.stack.push(Value::Number(-a));
            }
            Instruction::Not(_) => {
                let a = self.pop_bool(chunk)?;
                self.stack.push(Value::Boolean(!a));
            }
            Instruction::BooleanLit(lit) => {
                self.stack.push(Value::Boolean(lit.value));
            }
            Instruction::PushLocal(local) => {
                let value = self.pop()?;
                self.push_local(local.id, value);
            }
            Instruction::LoadLocal(local) => {
                let value = self.get_local(local.id)?;
                self.stack.push(value);
            }
            Instruction::Call(call) => {
                self.call(&call)?;
            }
            Instruction::Function(func) => {
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
            Instruction::EndFunction(_) => {
                self.pc = self
                    .scopes
                    .pop()
                    .context("returned when not in function call")?
                    .return_addr;
            }
            Instruction::If(_) => {
                if self.pop_bool(chunk)? {
                    self.stack.push(Value::IfCond { do_if: true });
                } else {
                    self.stack.push(Value::IfCond { do_if: false });
                }
            }
            Instruction::BeginIf(_) => {
                if !self.pop()?.to_if_cond()? {
                    self.skip_cond(chunk)?;
                    self.stack.push(Value::IfCond { do_if: false });
                }
            }
            Instruction::EndIf(_) => {
                self.stack.push(Value::IfCond { do_if: true });
            }
            Instruction::BeginElse(_) => {
                if self.pop()?.to_if_cond()? {
                    self.skip_cond(chunk)?;
                }
            }
            Instruction::EndElse(_) => {}
            Instruction::Mod(_) => {
                let b = self.pop_number(chunk)?;
                let a = self.pop_number(chunk)?;
                self.stack.push(Value::Number(a % b));
            }
            Instruction::Roll(Roll { n, d }) => {
                let mut result = 0.;
                for _ in 0..n {
                    result += self
                        .rng
                        .u8(1..=d)
                        .to_f64()
                        .context("error converting u8 to f64")?;
                }

                self.stack.push(Value::Number(result));
            }
            Instruction::PushList(_) => {
                self.stack
                    .push(Value::List(Arc::new(Mutex::new(Vec::new()))));
            }
            Instruction::Push(_) => {
                let value = self.pop()?;
                let mut ls = self.peek_list(chunk)?;
                ls.push(value);
            }
            Instruction::Index(_) => {
                let idx_raw = self.pop()?;
                let idx: usize =
                    num::cast(self.as_number(chunk, idx_raw)?).context("invalid index")?;
                let ls = self.peek_list(chunk)?;
                if let Some(value) = ls.get(idx) {
                    let new = value.clone();
                    drop(ls);
                    self.pop()?;
                    self.stack.push(new);
                } else {
                    return Err(anyhow!("index {idx} out of bounds"));
                }
            }
        };
        Ok(())
    }

    #[allow(clippy::equatable_if_let)]
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
        self.scopes.push(FuncScope {
            locals: BTreeMap::new(),
            return_addr: self.pc,
        });
        self.pc = offset;
        args.into_iter().rev().for_each(|arg| self.stack.push(arg));
        Ok(())
    }

    fn as_number(&mut self, chunk: &Chunk, mut value: Value) -> Result<f64> {
        loop {
            match value {
                Value::Number(n) => return Ok(n),
                Value::Function { n_args: 0, .. } => {
                    value = self.delegate_to_no_arg_func(value, chunk)?;
                }
                unexpected => return Err(anyhow!("expected number, found {unexpected}")),
            }
        }
    }

    fn as_bool(&mut self, chunk: &Chunk, mut value: Value) -> Result<bool> {
        loop {
            match value {
                Value::Boolean(v) => return Ok(v),
                Value::Function { n_args: 0, .. } => {
                    value = self.delegate_to_no_arg_func(value, chunk)?;
                }
                unexpected => return Err(anyhow!("expected boolean, found {unexpected}")),
            }
        }
    }

    fn as_comparable(&mut self, chunk: &Chunk, mut value: Value) -> Result<Value> {
        loop {
            match value {
                Value::Number(n) => return Ok(Value::Number(n)),
                Value::Boolean(v) => return Ok(Value::Boolean(v)),
                Value::Function { n_args: 0, .. } => {
                    value = self.delegate_to_no_arg_func(value, chunk)?;
                }
                unexpected => return Err(anyhow!("expected boolean, found {unexpected}")),
            }
        }
    }

    fn delegate_to_no_arg_func(&mut self, value: Value, chunk: &Chunk) -> Result<Value> {
        self.recurse_depth = self.recurse_depth.saturating_add(1);
        self.stack.push(value);
        let depth = self.scopes.len();
        self.call(&Call { n_args: 0 })?;
        loop {
            if depth == self.scopes.len() {
                self.recurse_depth = self.recurse_depth.saturating_sub(1);
                return self.pop();
            }
            if self.pc >= chunk.data.len() {
                return Err(anyhow!(
                    "unexpected end of program while executing function"
                ));
            }
            if self.recurse_depth == u8::MAX {
                return Err(anyhow!("stack overflow"));
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

    fn pop_comparable(&mut self, chunk: &Chunk) -> Result<Value> {
        let tmp = self.pop()?;
        self.as_comparable(chunk, tmp)
    }

    fn pop(&mut self) -> Result<Value> {
        self.stack.pop().context("tried to pop empty stack")
    }

    fn peek_list(&mut self, chunk: &Chunk) -> Result<MutexGuard<Vec<Value>>> {
        loop {
            match self.stack.last() {
                Some(Value::Function { n_args: 0, .. }) => {
                    let func = self.stack.pop().context("somehow empty stack")?;
                    let value = self.delegate_to_no_arg_func(func, chunk)?;
                    self.stack.push(value);
                }
                Some(Value::List(_)) => break,
                Some(value) => return Err(anyhow!("tried to peek list, found {value}")),
                None => return Err(anyhow!("tried to peek list with nothing on stack")),
            };
        }
        match self.stack.last() {
            Some(Value::List(reference)) => match reference.lock() {
                Ok(ls) => Ok(ls),
                Err(err) => Err(anyhow!("reference to list poisoned: {err}")),
            },
            Some(value) => Err(anyhow!("tried to peek list, found {value}")),
            None => Err(anyhow!("tried to peek list with nothing on stack")),
        }
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

    fn push_local(&mut self, id: u32, value: Value) {
        if let Some(local_scope) = self.scopes.last_mut() {
            local_scope.locals.insert(id, Local { value });
        } else {
            self.globals.insert(id, Local { value });
        }
    }

    fn get_local(&self, id: u32) -> Result<Value> {
        for local_scope in self.scopes.iter().rev() {
            if let Some(local) = local_scope.locals.get(&id) {
                return Ok(local.value.clone());
            }
        }
        Ok(self
            .globals
            .get(&id)
            .context("could not find local")?
            .value
            .clone())
    }
}
