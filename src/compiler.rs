use std::collections::HashMap;

use crate::bytecode::{
    Add, And, BooleanLit, Chunk, Divide, Equal, Greater, GreaterEqual, Instruction, Less,
    LessEqual, LoadLocal, Multiply, Negate, Not, NotEqual, NumberLit, Or, PopLocal, PushLocal,
    Subtract,
};
use anyhow::{anyhow, Context, Result};
use pest::{iterators::Pair, Parser};
use pest_derive::Parser;

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
            scopes: vec![Scope::default()],
            next_local_id: u32::MAX,
            chunk: Chunk::default(),
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
        for pair in f.into_inner() {
            match pair.as_rule() {
                Rule::declaration => {
                    self.declaration(pair)?;
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

        self.expression(expr)?;

        let local_id = self.add_local(ident.as_str())?;
        self.chunk.push(PushLocal { id: local_id });

        Ok(())
    }

    fn expression(&mut self, expr: Pair<Rule>) -> Result<()> {
        let inner = expr
            .into_inner()
            .next()
            .context("internal parsing error, expected expression to have inner value")?;
        match inner.as_rule() {
            Rule::block => self.block(inner),
            Rule::logic_or => self.logic_or(inner),
            unexpected => {
                return Err(anyhow!(
                    "unexpected {:?} at {}",
                    unexpected,
                    inner.as_span().as_str()
                ));
            }
        }
    }

    fn logic_or(&mut self, or: Pair<Rule>) -> Result<()> {
        let mut inner = or.into_inner().fuse();
        self.logic_and(
            inner
                .next()
                .context("internal parsing error, expected at least one logic_and")?,
        )?;
        while let (_, Some(item)) = (inner.next(), inner.next()) {
            self.logic_and(item)?;
            self.chunk.push(Or);
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
        while let (_, Some(item)) = (inner.next(), inner.next()) {
            self.equality(item)?;
            self.chunk.push(And);
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
            });
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
            });
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
            });
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
                _ => {
                    return Err(anyhow!(
                        "internal parsing error, unexpected {}",
                        op.as_span().as_str()
                    ))
                }
            });
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
                _ => self.call(item)?,
            }
        }
        for op in ops.into_iter().rev() {
            self.chunk.push(op);
        }
        Ok(())
    }

    fn call(&mut self, c: Pair<Rule>) -> Result<()> {
        let mut inner = c.into_inner();
        let pri = inner
            .next()
            .context("internal parsing error, expected primary")?;
        self.primary(pri)?;
        for _ in inner {
            todo!()
        }
        Ok(())
    }

    fn primary(&mut self, pri: Pair<Rule>) -> Result<()> {
        match pri.as_rule() {
            Rule::number => self.chunk.push(NumberLit {
                value: pri.as_str().parse().context("error parsing number")?,
            }),
            Rule::boolean => self.chunk.push(BooleanLit {
                value: match pri.as_str() {
                    "true" => true,
                    "false" => false,
                    unexpected => return Err(anyhow!("invalid boolean {unexpected}")),
                },
            }),
            Rule::identifier => self.identifier(pri)?,
            Rule::function => todo!(),
            _ => {
                return Err(anyhow!(
                    "internal parsing error, unexpected {}",
                    pri.as_span().as_str()
                ))
            }
        }
        Ok(())
    }

    fn identifier(&mut self, id: Pair<Rule>) -> Result<()> {
        let local_id = self.resolve_local(id.as_str())?;
        self.chunk.push(LoadLocal { id: local_id });
        Ok(())
    }

    fn begin_scope(&mut self) {
        self.scopes.push(Scope::default());
    }

    fn end_scope(&mut self) -> Result<()> {
        if let Some(scope) = self.scopes.pop() {
            for (_, id) in scope.locals {
                self.chunk.push(PopLocal { id });
            }
            Ok(())
        } else {
            Err(anyhow!("internal parsing error, ended root scope"))
        }
    }

    fn add_local(&mut self, name: &str) -> Result<u32> {
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
        self.next_local_id -= 1;
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
