use std::collections::HashMap;

use crate::ast::{BinaryOp, Expr, Program, Statement, UnaryOp};
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
    raw_dice_handling: RawDiceHandling,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum RawDiceHandling {
    Zero,
    D12,
}

impl Default for Compiler {
    fn default() -> Self {
        Self {
            chunk: Chunk::default(),
            scopes: vec![Scope::default()],
            next_local_id: 0,
            raw_dice_handling: RawDiceHandling::Zero,
        }
    }
}

impl Compiler {
    #[must_use]
    pub fn new(raw_dice_handling: RawDiceHandling) -> Self {
        Self {
            raw_dice_handling,
            ..Self::default()
        }
    }

    pub fn compile(mut self, code: &str) -> Result<Chunk> {
        let program = self.parse(code)?;
        self.generate_program(&program)?;
        Ok(self.chunk)
    }

    fn generate_program(&mut self, program: &Program) -> Result<()> {
        {
            let local_id = self.add_local("repeat")?;
            self.chunk.push(Repeat)?;
            self.chunk.push(PushLocal { id: local_id })?;
        }

        for statement in &program.0 {
            self.generate_statement(statement)?;
        }
        Ok(())
    }

    fn generate_statement(&mut self, statement: &Statement) -> Result<()> {
        match statement {
            Statement::Let {
                name,
                value,
                strict,
            } => {
                let local_id = self.add_local(name)?;
                self.generate_expr(value)?;
                if *strict {
                    self.chunk.push(Strict)?;
                }
                self.chunk.push(PushLocal { id: local_id })?;
            }
            Statement::Expr(expr) => {
                self.generate_expr(expr)?;
            }
        }
        Ok(())
    }

    #[allow(clippy::too_many_lines)]
    fn generate_expr(&mut self, expr: &Expr) -> Result<()> {
        match expr {
            Expr::Number(n) => self.chunk.push(NumberLit { value: *n })?,
            Expr::Boolean(b) => self.chunk.push(BooleanLit { value: *b })?,
            Expr::Identifier(name) => {
                let local_id = self.resolve_local(name)?;
                self.chunk.push(LoadLocal { id: local_id })?;
            }
            Expr::List(items) => {
                self.chunk.push(BeginList)?;
                for item in items {
                    self.generate_expr(item)?;
                    self.chunk.push(Push)?;
                }
                self.chunk.push(FinalizeList)?;
            }
            Expr::Function { params, body } => {
                let mut args = Vec::new();
                for param in params {
                    let local_id = self.add_local(param)?;
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
                self.generate_expr(body)?;
                self.end_scope()?;
                self.chunk.push(EndFunction)?;
            }
            Expr::Call { callee, args } => {
                self.generate_expr(callee)?;
                for arg in args {
                    self.generate_expr(arg)?;
                }
                self.chunk.push(Call {
                    n_args: args
                        .len()
                        .try_into()
                        .context("too many arguments to function")?,
                })?;
            }
            Expr::Index { container, index } => {
                self.generate_expr(container)?;
                self.generate_expr(index)?;
                self.chunk.push(Index)?;
            }
            Expr::FieldAccess { container, field } => {
                self.generate_expr(container)?;
                self.chunk.push(FieldAccess {
                    field: field.clone(),
                })?;
            }
            Expr::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.generate_expr(condition)?;
                self.chunk.push(If)?;
                self.chunk.push(BeginIf)?;
                self.generate_expr(then_branch)?;
                self.chunk.push(EndIf)?;
                self.chunk.push(BeginElse)?;
                self.generate_expr(else_branch)?;
                self.chunk.push(EndElse)?;
            }
            Expr::Block(statements) => {
                self.begin_scope();
                for statement in statements {
                    self.generate_statement(statement)?;
                }
                self.end_scope()?;
            }
            Expr::BinaryOp { op, left, right } => {
                self.generate_expr(left)?;
                self.generate_expr(right)?;
                self.chunk.push(match op {
                    BinaryOp::Add => Instruction::from(Add),
                    BinaryOp::Subtract => Instruction::from(Subtract),
                    BinaryOp::Multiply => Instruction::from(Multiply),
                    BinaryOp::Divide => Instruction::from(Divide),
                    BinaryOp::Mod => Instruction::from(Mod),
                    BinaryOp::Equal => Instruction::from(Equal),
                    BinaryOp::NotEqual => Instruction::from(NotEqual),
                    BinaryOp::Greater => Instruction::from(Greater),
                    BinaryOp::GreaterEqual => Instruction::from(GreaterEqual),
                    BinaryOp::Less => Instruction::from(Less),
                    BinaryOp::LessEqual => Instruction::from(LessEqual),
                    BinaryOp::And => Instruction::from(And),
                    BinaryOp::Or => Instruction::from(Or),
                })?;
            }
            Expr::UnaryOp { op, expr } => {
                self.generate_expr(expr)?;
                self.chunk.push(match op {
                    UnaryOp::Negate => Instruction::from(Negate),
                    UnaryOp::Not => Instruction::from(Not),
                })?;
            }
            Expr::Roll { n, d } => {
                self.chunk.push(Function { n_args: 0 })?;
                self.begin_scope();
                self.chunk.push(Roll { n: *n, d: *d })?;
                self.end_scope()?;
                self.chunk.push(EndFunction)?;
            }
        }
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

            match (first, second) {
                (Some('d'), s) if s.is_none_or(|s| s.is_ascii_digit()) => {
                    return Err(anyhow!("identifier {} is ambiguous with a dice roll", name,))
                }
                _ => {}
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

    const fn next_local_id(&mut self) -> u32 {
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

    fn parse(&self, code: &str) -> Result<Program> {
        let f = DiceyParser::parse(Rule::file, code)
            .context("error parsing code")?
            .next()
            .context("internal parsing error, file rule not found")?;
        self.parse_file(f)
    }

    fn parse_file(&self, f: Pair<Rule>) -> Result<Program> {
        let mut statements = Vec::new();
        for pair in f.into_inner() {
            match pair.as_rule() {
                Rule::declaration => {
                    statements.push(self.parse_declaration(pair)?);
                }
                Rule::expression => {
                    statements.push(Statement::Expr(self.parse_expression(pair)?));
                }
                Rule::EOI => break,
                unexpected => {
                    return Err(anyhow!(
                        "unexpected {:?} at {}",
                        unexpected,
                        pair.as_span().as_str()
                    ));
                }
            }
        }
        Ok(Program(statements))
    }

    fn parse_block(&self, f: Pair<Rule>) -> Result<Expr> {
        let mut statements = Vec::new();
        for pair in f.into_inner() {
            match pair.as_rule() {
                Rule::declaration => {
                    statements.push(self.parse_declaration(pair)?);
                }
                Rule::expression => {
                    statements.push(Statement::Expr(self.parse_expression(pair)?));
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
        Ok(Expr::Block(statements))
    }

    fn parse_declaration(&self, decl: Pair<Rule>) -> Result<Statement> {
        let mut inner = decl.into_inner();

        let ident = inner
            .next()
            .context("internal parsing error, expected identifier in declaration")?;
        let expr_pair = inner
            .next()
            .context("internal parsing error, expected expression in declaration")?;

        let (value, strict) = match expr_pair.as_rule() {
            Rule::lazy_expression => {
                let body = self.parse_expression(
                    expr_pair
                        .into_inner()
                        .next()
                        .context("error unwrapping lazy expression")?,
                )?;
                (
                    Expr::Function {
                        params: vec![],
                        body: Box::new(body),
                    },
                    false,
                )
            }
            Rule::expression => (self.parse_expression(expr_pair)?, true),
            Rule::function => (self.parse_function(expr_pair)?, false),
            unexpected => {
                return Err(anyhow!(
                    "unexpected {:?} at {}",
                    unexpected,
                    expr_pair.as_span().as_str(),
                ))
            }
        };

        Ok(Statement::Let {
            name: ident.as_str().into(),
            value,
            strict,
        })
    }

    fn parse_expression(&self, expr: Pair<Rule>) -> Result<Expr> {
        let inner = expr
            .into_inner()
            .next()
            .context("internal parsing error, expected expression to have inner value")?;
        match inner.as_rule() {
            Rule::if_statement => self.parse_if_statement(inner),
            Rule::block => self.parse_block(inner),
            Rule::logic_or => self.parse_logic_or(inner),
            unexpected => Err(anyhow!(
                "unexpected {:?} at {}",
                unexpected,
                inner.as_span().as_str(),
            )),
        }
    }

    fn parse_if_statement(&self, if_stmt: Pair<Rule>) -> Result<Expr> {
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

        Ok(Expr::If {
            condition: Box::new(self.parse_expression(cond)?),
            then_branch: Box::new(self.parse_expression(body)?),
            else_branch: Box::new(self.parse_expression(els)?),
        })
    }

    fn parse_logic_or(&self, or: Pair<Rule>) -> Result<Expr> {
        let mut inner = or.into_inner().fuse();
        let mut expr = self.parse_logic_and(
            inner
                .next()
                .context("internal parsing error, expected at least one logic_and")?,
        )?;
        for item in inner {
            expr = Expr::BinaryOp {
                op: BinaryOp::Or,
                left: Box::new(expr),
                right: Box::new(self.parse_logic_and(item)?),
            };
        }
        Ok(expr)
    }

    fn parse_logic_and(&self, and: Pair<Rule>) -> Result<Expr> {
        let mut inner = and.into_inner().fuse();
        let mut expr = self.parse_equality(
            inner
                .next()
                .context("internal parsing error, expected at least one equality")?,
        )?;
        for item in inner {
            expr = Expr::BinaryOp {
                op: BinaryOp::And,
                left: Box::new(expr),
                right: Box::new(self.parse_equality(item)?),
            };
        }
        Ok(expr)
    }

    fn parse_equality(&self, eq: Pair<Rule>) -> Result<Expr> {
        let mut inner = eq.into_inner().fuse();
        let mut expr = self.parse_comparison(
            inner
                .next()
                .context("internal parsing error, expected at least one comparison")?,
        )?;
        while let (Some(op), Some(item)) = (inner.next(), inner.next()) {
            expr = Expr::BinaryOp {
                op: match op.as_str() {
                    "==" => BinaryOp::Equal,
                    "!=" => BinaryOp::NotEqual,
                    _ => {
                        return Err(anyhow!(
                            "internal parsing error, unexpected {}",
                            op.as_span().as_str()
                        ))
                    }
                },
                left: Box::new(expr),
                right: Box::new(self.parse_comparison(item)?),
            };
        }
        Ok(expr)
    }

    fn parse_comparison(&self, cmp: Pair<Rule>) -> Result<Expr> {
        let mut inner = cmp.into_inner().fuse();
        let mut expr = self.parse_term(
            inner
                .next()
                .context("internal parsing error, expected at least one term")?,
        )?;
        while let (Some(op), Some(item)) = (inner.next(), inner.next()) {
            expr = Expr::BinaryOp {
                op: match op.as_str() {
                    ">=" => BinaryOp::GreaterEqual,
                    ">" => BinaryOp::Greater,
                    "<=" => BinaryOp::LessEqual,
                    "<" => BinaryOp::Less,
                    _ => {
                        return Err(anyhow!(
                            "internal parsing error, unexpected {}",
                            op.as_span().as_str()
                        ))
                    }
                },
                left: Box::new(expr),
                right: Box::new(self.parse_term(item)?),
            };
        }
        Ok(expr)
    }

    fn parse_term(&self, trm: Pair<Rule>) -> Result<Expr> {
        let mut inner = trm.into_inner().fuse();
        let mut expr = self.parse_factor(
            inner
                .next()
                .context("internal parsing error, expected at least one factor")?,
        )?;
        while let (Some(op), Some(item)) = (inner.next(), inner.next()) {
            expr = Expr::BinaryOp {
                op: match op.as_str() {
                    "+" => BinaryOp::Add,
                    "-" => BinaryOp::Subtract,
                    _ => {
                        return Err(anyhow!(
                            "internal parsing error, unexpected {}",
                            op.as_span().as_str()
                        ))
                    }
                },
                left: Box::new(expr),
                right: Box::new(self.parse_factor(item)?),
            };
        }
        Ok(expr)
    }

    fn parse_factor(&self, fct: Pair<Rule>) -> Result<Expr> {
        let mut inner = fct.into_inner().fuse();
        let mut expr = self.parse_unary(
            inner
                .next()
                .context("internal parsing error, expected at least one unary")?,
        )?;
        while let (Some(op), Some(item)) = (inner.next(), inner.next()) {
            expr = Expr::BinaryOp {
                op: match op.as_str() {
                    "*" => BinaryOp::Multiply,
                    "/" => BinaryOp::Divide,
                    "%" => BinaryOp::Mod,
                    _ => {
                        return Err(anyhow!(
                            "internal parsing error, unexpected {}",
                            op.as_span().as_str()
                        ))
                    }
                },
                left: Box::new(expr),
                right: Box::new(self.parse_unary(item)?),
            };
        }
        Ok(expr)
    }

    fn parse_unary(&self, un: Pair<Rule>) -> Result<Expr> {
        let mut inner = un.into_inner();
        let last = inner.next_back().context("empty unary rule")?;
        let mut expr = self.parse_qualify(last)?;
        for op in inner.rev() {
            expr = Expr::UnaryOp {
                op: match op.as_str() {
                    "!" => UnaryOp::Not,
                    "-" => UnaryOp::Negate,
                    _ => {
                        return Err(anyhow!(
                            "internal parsing error, unexpected {}",
                            op.as_span().as_str()
                        ))
                    }
                },
                expr: Box::new(expr),
            };
        }
        Ok(expr)
    }

    fn parse_qualify(&self, c: Pair<Rule>) -> Result<Expr> {
        let mut inner = c.into_inner();
        let pri = inner
            .next()
            .context("internal parsing error, expected primary")?;
        let mut expr = self.parse_primary(pri)?;
        for qualifier in inner {
            expr = match qualifier.as_rule() {
                Rule::call_args => {
                    let mut args = Vec::new();
                    for arg in qualifier.into_inner() {
                        args.push(match arg.as_rule() {
                            Rule::lazy_argument => {
                                let value = self.parse_expression(
                                    arg.into_inner()
                                        .next()
                                        .context("error unwrapping lazy argument")?,
                                )?;
                                Expr::Function {
                                    params: vec![],
                                    body: Box::new(value),
                                }
                            }
                            Rule::strict_argument => self.parse_expression(
                                arg.into_inner()
                                    .next()
                                    .context("error unwrapping strict argument")?,
                            )?,
                            rule => {
                                return Err(anyhow!("unexpected {rule:?} when parsing argument"))
                            }
                        });
                    }
                    Expr::Call {
                        callee: Box::new(expr),
                        args,
                    }
                }
                Rule::index => Expr::Index {
                    container: Box::new(expr),
                    index: Box::new(
                        self.parse_expression(
                            qualifier
                                .into_inner()
                                .next()
                                .context("no value for index")?,
                        )?,
                    ),
                },
                Rule::identifier => Expr::FieldAccess {
                    container: Box::new(expr),
                    field: SmartString::from(qualifier.as_str()),
                },
                _ => return Err(anyhow!("unexpected qualifier: {qualifier}")),
            }
        }
        Ok(expr)
    }

    fn parse_primary(&self, pri: Pair<Rule>) -> Result<Expr> {
        match pri.as_rule() {
            Rule::number_or_roll => self.parse_number_or_roll(&pri),
            Rule::boolean => Ok(Expr::Boolean(match pri.as_str() {
                "true" => true,
                "false" => false,
                unexpected => return Err(anyhow!("invalid boolean {unexpected}")),
            })),
            Rule::identifier => Ok(Expr::Identifier(pri.as_str().into())),
            Rule::list => self.parse_list(pri),
            Rule::function => self.parse_function(pri),
            Rule::expression => self.parse_expression(pri),
            _ => Err(anyhow!(
                "internal parsing error, unexpected {}",
                pri.as_span().as_str()
            )),
        }
    }

    fn parse_number_or_roll(&self, number_or_lit: &Pair<Rule>) -> Result<Expr> {
        let text = number_or_lit.as_str();
        if let Some(idx) = text.find('d') {
            let n = if idx == 0 {
                1
            } else {
                text[..idx]
                    .parse()
                    .context("error parsing number of dice to roll")?
            };

            Ok(Expr::Roll {
                n,
                d: if text.ends_with('d') {
                    match self.raw_dice_handling {
                        RawDiceHandling::Zero => 0,
                        RawDiceHandling::D12 => 12,
                    }
                } else {
                    text[(idx + 1)..]
                        .parse()
                        .context("error parsing number of dice sides")?
                },
            })
        } else {
            Ok(Expr::Number(
                number_or_lit
                    .as_str()
                    .parse()
                    .context("error parsing number")?,
            ))
        }
    }

    fn parse_list(&self, ls: Pair<Rule>) -> Result<Expr> {
        let mut items = Vec::new();
        for elem in ls.into_inner() {
            items.push(self.parse_expression(elem)?);
        }
        Ok(Expr::List(items))
    }

    fn parse_function(&self, func: Pair<Rule>) -> Result<Expr> {
        let mut inner = func.into_inner();
        let parameters = inner
            .next()
            .context("internal parsing error, expected function parameters")?;
        let expr = inner
            .next()
            .context("internal parsing error, expected function body")?;

        Ok(Expr::Function {
            params: parameters
                .into_inner()
                .rev()
                .map(|p| p.as_str().into())
                .collect(),
            body: Box::new(self.parse_expression(expr)?),
        })
    }
}
