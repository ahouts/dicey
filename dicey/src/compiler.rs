use std::collections::HashMap;

use crate::bytecode::{
    Add, And, BeginElse, BeginIf, BeginList, BooleanLit, Call, Chunk, Divide, EndElse, EndFunction,
    EndIf, Equal, FieldAccess, FinalizeList, Function, Greater, GreaterEqual, If, Index,
    Instruction, Less, LessEqual, LoadLocal, Mod, Multiply, Negate, Not, NotEqual, NumberLit, Or,
    Push, PushLocal, Repeat, Roll, Strict, Subtract,
};
use anyhow::{anyhow, Context, Result};
use pest::{iterators::Pair, Parser};
use pest_derive::Parser;
use smartstring::SmartString;

#[derive(Parser)]
#[grammar = "dicey.pest"]
struct DiceyParser;

#[derive(Default, Debug, Clone)]
struct Scope {
    locals: HashMap<String, u32>,
}

#[derive(Debug, Clone)]
pub struct Compiler {
    chunk: Chunk,
    scopes: Vec<Scope>,
    next_local_id: u32,
}

impl Default for Compiler {
    fn default() -> Self {
        Self {
            chunk: Chunk::default(),
            scopes: vec![Scope::default()],
            next_local_id: 0,
        }
    }
}

impl Compiler {
    pub fn compile(mut self, code: &str) -> Result<Chunk> {
        let f = DiceyParser::parse(Rule::file, code)
            .context("error parsing code")?
            .next()
            .context("internal parsing error, file rule not found")?;

        self.file(f)?;

        Ok(self.chunk)
    }

    fn file(&mut self, f: Pair<Rule>) -> Result<()> {
        {
            let local_id = self.add_local("repeat")?;
            self.chunk.push(Repeat)?;
            self.chunk.push(PushLocal { id: local_id })?;
        }

        for pair in f.into_inner() {
            match pair.as_rule() {
                Rule::declaration => {
                    self.declaration(pair)?;
                }
                Rule::assignment => {
                    todo!()
                }
                Rule::expression => {
                    self.expression(pair)?;
                }
                Rule::EOI => return Ok(()),
                unexpected => {
                    return Err(anyhow!(
                        "unexpected {:?} at {}",
                        unexpected,
                        pair.as_span().as_str()
                    ));
                }
            }
        }
        Ok(())
    }

    fn block(&mut self, f: Pair<Rule>) -> Result<()> {
        self.begin_scope();
        for pair in f.into_inner() {
            match pair.as_rule() {
                Rule::declaration => {
                    self.declaration(pair)?;
                }
                Rule::assignment => {
                    todo!()
                }
                Rule::expression => {
                    self.expression(pair)?;
                }
                unexpected => {
                    return Err(anyhow!(
                        "unexpected {:?} at {}",
                        unexpected,
                        pair.as_span().as_str()
                    ));
                }
            }
        }
        self.end_scope()?;
        Ok(())
    }

    fn declaration(&mut self, decl: Pair<Rule>) -> Result<()> {
        let mut inner = decl.into_inner();

        let ident = inner
            .next()
            .context("internal parsing error, expected identifier in declaration")?;
        let expr = inner
            .next()
            .context("internal parsing error, expected expression in declaration")?;

        let local_id = self.add_local(ident.as_str())?;

        match expr.as_rule() {
            Rule::lazy_expression => {
                self.function_raw(
                    std::iter::empty(),
                    expr.into_inner()
                        .next()
                        .context("error unwrapping lazy expression")?,
                )?;
            }
            Rule::expression => {
                self.expression(expr)?;
                self.chunk.push(Strict)?;
            }
            Rule::function => {
                self.function(expr)?;
            }
            unexpected => {
                return Err(anyhow!(
                    "unexpected {:?} at {}",
                    unexpected,
                    expr.as_span().as_str(),
                ))
            }
        }

        self.chunk.push(PushLocal { id: local_id })?;

        Ok(())
    }

    fn expression(&mut self, expr: Pair<Rule>) -> Result<()> {
        let inner = expr
            .into_inner()
            .next()
            .context("internal parsing error, expected expression to have inner value")?;
        match inner.as_rule() {
            Rule::if_statement => self.if_statement(inner),
            Rule::block => self.block(inner),
            Rule::logic_or => self.logic_or(inner),
            unexpected => Err(anyhow!(
                "unexpected {:?} at {}",
                unexpected,
                inner.as_span().as_str(),
            )),
        }
    }

    fn if_statement(&mut self, if_stmt: Pair<Rule>) -> Result<()> {
        let mut inner = if_stmt.into_inner();
        let cond = inner
            .next()
            .context("internal parsing error, expected if condition")?;
        let body = inner
            .next()
            .context("internal parsing error, expected if body")?;
        let els = inner
            .next()
            .context("internal parsing error, expected if else")?;

        self.expression(cond)?;
        self.chunk.push(If)?;

        self.chunk.push(BeginIf)?;
        self.expression(body)?;
        self.chunk.push(EndIf)?;

        self.chunk.push(BeginElse)?;
        self.expression(els)?;
        self.chunk.push(EndElse)?;

        Ok(())
    }

    fn logic_or(&mut self, or: Pair<Rule>) -> Result<()> {
        let mut inner = or.into_inner().fuse();
        self.logic_and(
            inner
                .next()
                .context("internal parsing error, expected at least one logic_and")?,
        )?;
        for item in inner {
            self.logic_and(item)?;
            self.chunk.push(Or)?;
        }
        Ok(())
    }

    fn logic_and(&mut self, and: Pair<Rule>) -> Result<()> {
        let mut inner = and.into_inner().fuse();
        self.equality(
            inner
                .next()
                .context("internal parsing error, expected at least one equality")?,
        )?;
        for item in inner {
            self.equality(item)?;
            self.chunk.push(And)?;
        }
        Ok(())
    }

    fn equality(&mut self, eq: Pair<Rule>) -> Result<()> {
        let mut inner = eq.into_inner().fuse();
        self.comparison(
            inner
                .next()
                .context("internal parsing error, expected at least one comparison")?,
        )?;
        while let (Some(op), Some(item)) = (inner.next(), inner.next()) {
            self.comparison(item)?;
            self.chunk.push(match op.as_str() {
                "==" => Instruction::from(Equal),
                "!=" => Instruction::from(NotEqual),
                _ => {
                    return Err(anyhow!(
                        "internal parsing error, unexpected {}",
                        op.as_span().as_str()
                    ))
                }
            })?;
        }
        Ok(())
    }

    fn comparison(&mut self, cmp: Pair<Rule>) -> Result<()> {
        let mut inner = cmp.into_inner().fuse();
        self.term(
            inner
                .next()
                .context("internal parsing error, expected at least one term")?,
        )?;
        while let (Some(op), Some(item)) = (inner.next(), inner.next()) {
            self.term(item)?;
            self.chunk.push(match op.as_str() {
                ">=" => Instruction::from(GreaterEqual),
                ">" => Instruction::from(Greater),
                "<=" => Instruction::from(LessEqual),
                "<" => Instruction::from(Less),
                _ => {
                    return Err(anyhow!(
                        "internal parsing error, unexpected {}",
                        op.as_span().as_str()
                    ))
                }
            })?;
        }
        Ok(())
    }

    fn term(&mut self, trm: Pair<Rule>) -> Result<()> {
        let mut inner = trm.into_inner().fuse();
        self.factor(
            inner
                .next()
                .context("internal parsing error, expected at least one factor")?,
        )?;
        while let (Some(op), Some(item)) = (inner.next(), inner.next()) {
            self.factor(item)?;
            self.chunk.push(match op.as_str() {
                "+" => Instruction::from(Add),
                "-" => Instruction::from(Subtract),
                _ => {
                    return Err(anyhow!(
                        "internal parsing error, unexpected {}",
                        op.as_span().as_str()
                    ))
                }
            })?;
        }
        Ok(())
    }

    fn factor(&mut self, fct: Pair<Rule>) -> Result<()> {
        let mut inner = fct.into_inner().fuse();
        self.unary(
            inner
                .next()
                .context("internal parsing error, expected at least one unary")?,
        )?;
        while let (Some(op), Some(item)) = (inner.next(), inner.next()) {
            self.unary(item)?;
            self.chunk.push(match op.as_str() {
                "*" => Instruction::from(Multiply),
                "/" => Instruction::from(Divide),
                "%" => Instruction::from(Mod),
                _ => {
                    return Err(anyhow!(
                        "internal parsing error, unexpected {}",
                        op.as_span().as_str()
                    ))
                }
            })?;
        }
        Ok(())
    }

    fn unary(&mut self, un: Pair<Rule>) -> Result<()> {
        let mut ops = Vec::new();
        for item in un.into_inner() {
            match item.as_rule() {
                Rule::unary_op => match item.as_str() {
                    "!" => ops.push(Instruction::from(Not)),
                    "-" => ops.push(Instruction::from(Negate)),
                    _ => {
                        return Err(anyhow!(
                            "internal parsing error, unexpected {}",
                            item.as_span().as_str()
                        ))
                    }
                },
                _ => self.qualify(item)?,
            }
        }
        for op in ops.into_iter().rev() {
            self.chunk.push(op)?;
        }
        Ok(())
    }

    fn qualify(&mut self, c: Pair<Rule>) -> Result<()> {
        let mut inner = c.into_inner();
        let pri = inner
            .next()
            .context("internal parsing error, expected primary")?;
        self.primary(pri)?;
        for qualifier in inner {
            match qualifier.as_rule() {
                Rule::call_args => {
                    let mut n_args = 0;
                    for arg in qualifier.into_inner() {
                        match arg.as_rule() {
                            Rule::lazy_argument => {
                                self.function_raw(
                                    std::iter::empty(),
                                    arg.into_inner()
                                        .next()
                                        .context("error unwrapping lazy argument")?,
                                )?;
                            }
                            Rule::strict_argument => {
                                self.expression(
                                    arg.into_inner()
                                        .next()
                                        .context("error unwrapping strict argument")?,
                                )?;
                            }
                            rule => {
                                return Err(anyhow!("unexpected {rule:?} when parsing argument"))
                            }
                        }
                        n_args += 1;
                    }
                    self.chunk.push(Call { n_args })?;
                }
                Rule::index => {
                    self.expression(
                        qualifier
                            .into_inner()
                            .next()
                            .context("no value for index")?,
                    )?;
                    self.chunk.push(Index)?;
                }
                Rule::identifier => self.chunk.push(FieldAccess {
                    field: SmartString::from(qualifier.as_str()),
                })?,
                _ => return Err(anyhow!("unexpected qualifier: {qualifier}")),
            }
        }
        Ok(())
    }

    fn primary(&mut self, pri: Pair<Rule>) -> Result<()> {
        match pri.as_rule() {
            Rule::number_or_roll => self.number_or_roll(&pri)?,
            Rule::boolean => self.chunk.push(BooleanLit {
                value: match pri.as_str() {
                    "true" => true,
                    "false" => false,
                    unexpected => return Err(anyhow!("invalid boolean {unexpected}")),
                },
            })?,
            Rule::identifier => self.identifier(&pri)?,
            Rule::list => self.list(pri)?,
            Rule::function => self.function(pri)?,
            Rule::expression => self.expression(pri)?,
            _ => {
                return Err(anyhow!(
                    "internal parsing error, unexpected {}",
                    pri.as_span().as_str()
                ))
            }
        }
        Ok(())
    }

    fn number_or_roll(&mut self, number_or_lit: &Pair<Rule>) -> Result<()> {
        let text = number_or_lit.as_str();
        if let Some(idx) = text.find('d') {
            let n = if idx == 0 {
                1
            } else {
                text[..idx]
                    .parse()
                    .context("error parsing number of dice to roll")?
            };

            self.chunk.push(Function { n_args: 0 })?;
            self.begin_scope();
            self.chunk.push(Roll {
                n,
                d: text[(idx + 1)..]
                    .parse()
                    .context("error parsing number of dice sides")?,
            })?;
            self.end_scope()?;
            self.chunk.push(EndFunction)?;
        } else {
            self.chunk.push(NumberLit {
                value: number_or_lit
                    .as_str()
                    .parse()
                    .context("error parsing number")?,
            })?;
        }
        Ok(())
    }

    fn identifier(&mut self, id: &Pair<Rule>) -> Result<()> {
        let local_id = self.resolve_local(id.as_str())?;
        self.chunk.push(LoadLocal { id: local_id })?;
        Ok(())
    }

    fn list(&mut self, ls: Pair<Rule>) -> Result<()> {
        self.chunk.push(BeginList)?;
        for elem in ls.into_inner() {
            self.expression(elem)?;
            self.chunk.push(Push)?;
        }
        self.chunk.push(FinalizeList)?;
        Ok(())
    }

    fn function(&mut self, func: Pair<Rule>) -> Result<()> {
        let mut inner = func.into_inner();
        let parameters = inner
            .next()
            .context("internal parsing error, expected function parameters")?;
        let expr = inner
            .next()
            .context("internal parsing error, expected function body")?;
        self.function_raw(parameters.into_inner().rev(), expr)
    }

    fn function_raw<'a>(
        &mut self,
        parameters: impl Iterator<Item = Pair<'a, Rule>>,
        expr: Pair<'a, Rule>,
    ) -> Result<()> {
        let mut args = Vec::new();
        for arg in parameters {
            let local_id = self.add_local(arg.as_str())?;
            args.push(PushLocal { id: local_id });
        }

        self.chunk.push(Function {
            n_args: args
                .len()
                .try_into()
                .context("too many arguments to function")?,
        })?;
        self.begin_scope();
        for arg in args {
            self.chunk.push(arg)?;
        }
        self.expression(expr)?;
        self.end_scope()?;
        self.chunk.push(EndFunction)?;

        Ok(())
    }

    fn begin_scope(&mut self) {
        self.scopes.push(Scope::default());
    }

    fn end_scope(&mut self) -> Result<()> {
        if self.scopes.pop().is_some() {
            Ok(())
        } else {
            Err(anyhow!("internal parsing error, ended root scope"))
        }
    }

    fn add_local(&mut self, name: &str) -> Result<u32> {
        {
            let mut iter = name.chars().fuse();
            let first = iter.next();
            let second = iter.next();

            if let (Some(f), Some(s)) = (first, second) {
                if f == 'd' && s.is_ascii_digit() {
                    return Err(anyhow!("identifier {} is ambiguous with a dice roll", name,));
                }
            }
        }

        let next_id = self.next_local_id();
        if let Some(scope) = self.scopes.last_mut() {
            scope.locals.insert(name.to_string(), next_id);
            Ok(next_id)
        } else {
            Err(anyhow!(
                "internal parsing error, tried to add local without scope"
            ))
        }
    }

    fn next_local_id(&mut self) -> u32 {
        let next = self.next_local_id;
        self.next_local_id += 1;
        next
    }

    fn resolve_local(&self, name: &str) -> Result<u32> {
        for scope in self.scopes.iter().rev() {
            if let Some(id) = scope.locals.get(name) {
                return Ok(*id);
            }
        }
        Err(anyhow!("unknown local {name}"))
    }
}
