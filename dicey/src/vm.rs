use crate::bytecode::{Call, Chunk, FieldAccess, Instruction, InstructionImpl, OpCode, Roll};
use anyhow::{anyhow, Context, Result};
use fastrand::Rng;
use num::ToPrimitive;
use std::cell::RefCell;
use std::collections::BTreeMap;
use std::fmt;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Builtin {
    Roll { n: u8, d: u8 },
    Map(Rc<Vec<Value>>),
    Filter(Rc<Vec<Value>>),
    Random(Rc<Vec<Value>>),
    Repeat,
}

impl fmt::Display for Builtin {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self:?}")
    }
}

#[derive(Clone)]
pub enum Value {
    Number(f64),
    Boolean(bool),
    Function { n_args: u8, loc: usize },
    Builtin(Builtin),
    List(Rc<Vec<Value>>),

    IfCond { do_if: bool },
    ListPartial(RefCell<Vec<Value>>),
}

impl Value {
    pub fn to_number(&self) -> Result<f64> {
        match self {
            Self::Number(n) => Ok(*n),
            unexpected => Err(anyhow!("expected number, found {unexpected}")),
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
            (Self::List(l1), Self::List(l2)) => l1 == l2,
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
            Self::Builtin(builtin) => write!(f, "{builtin}"),
            Self::List(ls) => {
                write!(f, "[ ")?;
                for value in ls.iter() {
                    write!(f, "{value}, ")?;
                }
                write!(f, "]")?;
                Ok(())
            }

            Self::IfCond { do_if } => write!(f, "IfCond {{ do_if: {do_if} }}"),
            Self::ListPartial(reference) => {
                write!(f, "Partial [ ")?;
                match reference.try_borrow() {
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
    const MAX_STACK_SIZE: usize = 128;
    const MAX_RECURSE_DEPTH: u8 = 64;

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

        let raw_result = self.pop()?;
        let result = self.materialize(chunk, raw_result)?;

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
                self.push(Value::Number(lit.value))?;
            }
            Instruction::Add(_) => {
                let b = self.pop_number(chunk)?;
                let a = self.pop_number(chunk)?;
                self.push(Value::Number(a + b))?;
            }
            Instruction::Subtract(_) => {
                let b = self.pop_number(chunk)?;
                let a = self.pop_number(chunk)?;
                self.push(Value::Number(a - b))?;
            }
            Instruction::Multiply(_) => {
                let b = self.pop_number(chunk)?;
                let a = self.pop_number(chunk)?;
                self.push(Value::Number(a * b))?;
            }
            Instruction::Divide(_) => {
                let b = self.pop_number(chunk)?;
                let a = self.pop_number(chunk)?;
                self.push(Value::Number(a / b))?;
            }
            Instruction::Equal(_) => {
                let b = self.pop_comparable(chunk)?;
                let a = self.pop_comparable(chunk)?;
                self.push(Value::Boolean(a == b))?;
            }
            Instruction::NotEqual(_) => {
                let b = self.pop_comparable(chunk)?;
                let a = self.pop_comparable(chunk)?;
                self.push(Value::Boolean(a != b))?;
            }
            Instruction::GreaterEqual(_) => {
                let b = self.pop_comparable(chunk)?;
                let a = self.pop_comparable(chunk)?;
                self.push(Value::Boolean(a >= b))?;
            }
            Instruction::Greater(_) => {
                let b = self.pop_comparable(chunk)?;
                let a = self.pop_comparable(chunk)?;
                self.push(Value::Boolean(a > b))?;
            }
            Instruction::LessEqual(_) => {
                let b = self.pop_comparable(chunk)?;
                let a = self.pop_comparable(chunk)?;
                self.push(Value::Boolean(a <= b))?;
            }
            Instruction::Less(_) => {
                let b = self.pop_comparable(chunk)?;
                let a = self.pop_comparable(chunk)?;
                self.push(Value::Boolean(a < b))?;
            }
            Instruction::Pop(_) => {
                self.pop()?;
            }
            Instruction::Or(_) => {
                let b = self.pop_bool(chunk)?;
                let a = self.pop_bool(chunk)?;
                self.push(Value::Boolean(a || b))?;
            }
            Instruction::And(_) => {
                let b = self.pop_bool(chunk)?;
                let a = self.pop_bool(chunk)?;
                self.push(Value::Boolean(a && b))?;
            }
            Instruction::Negate(_) => {
                let a = self.pop_number(chunk)?;
                self.push(Value::Number(-a))?;
            }
            Instruction::Not(_) => {
                let a = self.pop_bool(chunk)?;
                self.push(Value::Boolean(!a))?;
            }
            Instruction::BooleanLit(lit) => {
                self.push(Value::Boolean(lit.value))?;
            }
            Instruction::PushLocal(local) => {
                let value = self.pop()?;
                self.push_local(local.id, value);
            }
            Instruction::LoadLocal(local) => {
                let value = self.get_local(local.id)?;
                self.push(value)?;
            }
            Instruction::Call(call) => {
                self.call(&call, chunk)?;
            }
            Instruction::Function(func) => {
                self.push(Value::Function {
                    n_args: func.n_args,
                    loc: self.pc,
                })?;
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
                    self.push(Value::IfCond { do_if: true })?;
                } else {
                    self.push(Value::IfCond { do_if: false })?;
                }
            }
            Instruction::BeginIf(_) => {
                if !self.pop()?.to_if_cond()? {
                    self.skip_cond(chunk)?;
                    self.push(Value::IfCond { do_if: false })?;
                }
            }
            Instruction::EndIf(_) => {
                self.push(Value::IfCond { do_if: true })?;
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
                self.push(Value::Number(a % b))?;
            }
            Instruction::Roll(Roll { n, d }) => {
                self.stack.push(Value::Builtin(Builtin::Roll { n, d }));
            }
            Instruction::BeginList(_) => {
                self.stack
                    .push(Value::ListPartial(RefCell::new(Vec::new())));
            }
            Instruction::Push(_) => {
                let value = self.pop()?;
                loop {
                    match self.stack.last() {
                        Some(Value::Function { n_args: 0, .. }) => {
                            let func = self.stack.pop().context("somehow empty stack")?;
                            let value = self.delegate_to_no_arg_func(func, chunk)?;
                            self.push(value)?;
                        }
                        Some(Value::ListPartial(_)) => break,
                        Some(value) => return Err(anyhow!("tried to peek list, found {value}")),
                        None => return Err(anyhow!("tried to peek list with nothing on stack")),
                    };
                }
                match self.stack.last() {
                    Some(Value::ListPartial(reference)) => match reference.try_borrow_mut() {
                        Ok(mut ls) => {
                            ls.push(value);
                        }
                        Err(err) => return Err(anyhow!("reference to list poisoned: {err}")),
                    },
                    Some(value) => return Err(anyhow!("tried to peek list, found {value}")),
                    None => return Err(anyhow!("tried to peek list with nothing on stack")),
                }
            }
            Instruction::Index(_) => {
                let idx_raw = self.pop()?;
                let idx: usize =
                    num::cast(self.as_number(chunk, idx_raw)?).context("invalid index")?;

                let tmp = self.pop()?;
                match Rc::try_unwrap(self.as_list(chunk, tmp)?) {
                    Ok(mut ls) => {
                        if idx < ls.len() {
                            self.push(ls.swap_remove(idx))?;
                        } else {
                            return Err(anyhow!("index {idx} out of bounds"));
                        }
                    }
                    Err(ls) => {
                        if let Some(value) = ls.get(idx) {
                            let new = value.clone();
                            self.push(new)?;
                        } else {
                            return Err(anyhow!("index {idx} out of bounds"));
                        }
                    }
                }
            }
            Instruction::Strict(_) => loop {
                let value = self.pop()?;
                match value {
                    Value::Function { n_args: 0, .. } => {
                        let new_value = self.delegate_to_no_arg_func(value, chunk)?;
                        self.push(new_value)?;
                    }
                    Value::Builtin(Builtin::Roll { n, d }) => {
                        let res = self.eval_roll(n, d)?;
                        self.push(res)?;
                    }
                    value => {
                        self.push(value)?;
                        break;
                    }
                }
            },
            Instruction::FieldAccess(FieldAccess { field }) => {
                let ls = self.pop_list(chunk)?;
                match field.as_str() {
                    "length" => {
                        let len = ls.len();
                        self.push(Value::Number(
                            num::cast(len).context("list too long to index")?,
                        ))?;
                    }
                    "map" => self.push(Value::Builtin(Builtin::Map(ls)))?,
                    "filter" => self.push(Value::Builtin(Builtin::Filter(ls)))?,
                    "random" => self.push(Value::Builtin(Builtin::Random(ls)))?,
                    unexpected => return Err(anyhow!("unexpected field {unexpected}")),
                }
            }
            Instruction::FinalizeList(_) => match self.pop()? {
                Value::ListPartial(ls) => self.push(Value::List(Rc::new(ls.into_inner())))?,
                unexpected => return Err(anyhow!("expected list partial, found {unexpected}")),
            },
            Instruction::Repeat(_) => {
                self.push(Value::Builtin(Builtin::Repeat))?;
            }
        };
        Ok(())
    }

    #[allow(clippy::equatable_if_let)]
    fn call(&mut self, call: &Call, chunk: &Chunk) -> Result<(), anyhow::Error> {
        self.recurse_depth = self.recurse_depth.saturating_add(1);
        if self.recurse_depth >= Self::MAX_RECURSE_DEPTH {
            return Err(anyhow!("call stack overflow"));
        }

        let mut args = Vec::new();
        for _ in 0..call.n_args {
            args.push(self.pop().context("not enough arguments for call")?);
        }

        let result = match self.pop().context("tried to call with empty stack")? {
            Value::Function {
                mut n_args,
                mut loc,
            } => {
                self.flatten_nested_function(call.n_args, &mut n_args, &mut loc, chunk)?;
                if n_args != call.n_args {
                    return Err(anyhow!(
                        "called function with incorrect number of arguments"
                    ));
                }
                self.scopes.push(FuncScope {
                    locals: BTreeMap::new(),
                    return_addr: self.pc,
                });
                self.pc = loc;
                for arg in args.into_iter().rev() {
                    self.push(arg)?;
                }
                Ok(())
            }
            Value::Builtin(Builtin::Map(ls)) => {
                if call.n_args != 1 {
                    return Err(anyhow!("incorrect number of arguments: List.map(mapping)"));
                }

                let mut results = Vec::new();
                for value in ls.iter() {
                    self.push(args[0].clone())?;
                    self.push(value.clone())?;
                    self.call_and_eval(1, chunk)?;
                    results.push(self.pop()?);
                }

                self.push(Value::List(Rc::new(results)))?;

                Ok(())
            }
            Value::Builtin(Builtin::Filter(ls)) => {
                if call.n_args != 1 {
                    return Err(anyhow!(
                        "incorrect number of arguments: List.filter(predicate)"
                    ));
                }

                let mut results = Vec::new();
                for value in ls.iter() {
                    self.push(args[0].clone())?;
                    self.push(value.clone())?;
                    self.call_and_eval(1, chunk)?;
                    if self.pop_bool(chunk)? {
                        results.push(value.clone());
                    }
                }

                self.push(Value::List(Rc::new(results)))?;

                Ok(())
            }
            Value::Builtin(Builtin::Random(ls)) => {
                if call.n_args != 1 {
                    return Err(anyhow!(
                        "incorrect number of arguments: List.random(default_value)"
                    ));
                }

                if ls.is_empty() {
                    self.push(args[0].clone())?;
                } else {
                    let res = ls[self.rng.usize(0..ls.len())].clone();
                    self.push(res)?;
                }

                Ok(())
            }
            Value::Builtin(Builtin::Repeat) => {
                if call.n_args != 2 {
                    return Err(anyhow!(
                        "incorrect number of arguments: repeat(num_times, expression)"
                    ));
                }

                let times = self
                    .as_number(chunk, args[1].clone())?
                    .to_u16()
                    .context("invalid number of times to repeat expression")?;

                let mut result = Vec::new();
                for _ in 0..times {
                    result.push(args[0].clone());
                }
                self.push(Value::List(Rc::new(result)))?;

                Ok(())
            }
            unexpected => Err(anyhow!("expected function, found {unexpected}")),
        };

        self.recurse_depth = self.recurse_depth.saturating_sub(1);
        result
    }

    fn call_and_eval(&mut self, n_args: u8, chunk: &Chunk) -> Result<(), anyhow::Error> {
        let depth = self.scopes.len();
        self.call(&Call { n_args }, chunk)?;
        loop {
            if depth == self.scopes.len() {
                break;
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
        Ok(())
    }

    fn flatten_nested_function(
        &mut self,
        real_args: u8,
        n_args: &mut u8,
        loc: &mut usize,
        chunk: &Chunk,
    ) -> Result<()> {
        while real_args != 0 && *n_args == 0 {
            match self.delegate_to_no_arg_func(
                Value::Function {
                    n_args: *n_args,
                    loc: *loc,
                },
                chunk,
            )? {
                Value::Function {
                    n_args: new_args,
                    loc: new_loc,
                } => {
                    *n_args = new_args;
                    *loc = new_loc;
                }
                unexpected => return Err(anyhow!("expected function, found {unexpected}")),
            }
        }
        Ok(())
    }

    fn as_number(&mut self, chunk: &Chunk, mut value: Value) -> Result<f64> {
        loop {
            match value {
                Value::Number(n) => return Ok(n),
                Value::Function { n_args: 0, .. } => {
                    value = self.delegate_to_no_arg_func(value, chunk)?;
                }
                Value::Builtin(Builtin::Roll { n, d }) => {
                    value = self.eval_roll(n, d)?;
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
                Value::Builtin(Builtin::Roll { n, d }) => return self.eval_roll(n, d),
                unexpected => return Err(anyhow!("expected boolean, found {unexpected}")),
            }
        }
    }

    fn eval_roll(&mut self, n: u8, d: u8) -> Result<Value> {
        let mut result = 0.;

        if d != 0 {
            for _ in 0..n {
                result += self
                    .rng
                    .u8(1..=d)
                    .to_f64()
                    .context("error converting u8 to f64")?;
            }
        }

        log::trace!("{:10} {:?} {}d{} = {}", self.pc, self.stack, n, d, result);

        Ok(Value::Number(result))
    }

    fn as_list(&mut self, chunk: &Chunk, mut value: Value) -> Result<Rc<Vec<Value>>> {
        loop {
            match value {
                Value::List(ls) => return Ok(ls),
                Value::Function { n_args: 0, .. } => {
                    value = self.delegate_to_no_arg_func(value, chunk)?;
                }
                unexpected => return Err(anyhow!("expected list, found {unexpected}")),
            }
        }
    }

    fn delegate_to_no_arg_func(&mut self, value: Value, chunk: &Chunk) -> Result<Value> {
        self.push(value)?;
        self.call_and_eval(0, chunk)?;
        self.pop()
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

    fn pop_list(&mut self, chunk: &Chunk) -> Result<Rc<Vec<Value>>> {
        let tmp = self.pop()?;
        self.as_list(chunk, tmp)
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

    fn push(&mut self, value: Value) -> Result<()> {
        self.stack.push(value);
        if self.stack.len() >= Self::MAX_STACK_SIZE {
            Err(anyhow!("stack overflow"))
        } else {
            Ok(())
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

    fn materialize(&mut self, chunk: &Chunk, mut value: Value) -> Result<Value> {
        loop {
            match value {
                result @ Value::Function { n_args: 0, .. } => {
                    self.recurse_depth = self.recurse_depth.saturating_add(1);
                    let res = self.delegate_to_no_arg_func(result, chunk)?;
                    value = res;
                }
                Value::Builtin(Builtin::Roll { n, d }) => {
                    let res = self.eval_roll(n, d)?;
                    value = res;
                }
                Value::List(reference) => {
                    let res = (*reference).clone();
                    let mut mat = Vec::with_capacity(res.len());
                    for item in res {
                        mat.push(self.materialize(chunk, item)?);
                    }
                    return Ok(Value::List(Rc::new(mat)));
                }
                result => return Ok(result),
            };
        }
    }
}
