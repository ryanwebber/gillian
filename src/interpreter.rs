use std::{fmt::Display, ops::Range};

#[derive(Debug, Clone)]
pub enum Value {
    Number(f64),
    List(Vec<Value>),
    Range(Range<i64>),
}

impl Value {
    pub fn typeid(&self) -> TypeId {
        match self {
            Value::Number(_) => TypeId::Number,
            Value::List(_) => TypeId::List,
            Value::Range(_) => TypeId::Range,
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{}", n),
            Value::List(values) => {
                write!(f, "[")?;
                for (i, value) in values.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", value)?;
                }
                write!(f, "]")
            }
            Value::Range(range) => {
                for (i, n) in range.clone().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", n)?;
                }

                Ok(())
            }
        }
    }
}

#[derive(Debug)]
pub enum TypeId {
    Number,
    List,
    Range,
}

impl Display for TypeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeId::Number => write!(f, "number"),
            TypeId::List => write!(f, "list"),
            TypeId::Range => write!(f, "range"),
        }
    }
}

pub enum Token {
    Instruction(Instruction),
    NumberLiteral(f64),
}

pub enum Instruction {
    Add,
    Add1,
    Mul,
    Mul1,
    Mul2,
    Mul10,
    Mul100,
    Mul1000,
    Push,
    PushN,
    RangeTo,
    Repeat,
}

impl Instruction {
    pub fn try_from(c: char) -> Option<Instruction> {
        match c {
            '+' => Some(Instruction::Add),
            'A' => Some(Instruction::Add1),
            '*' => Some(Instruction::Mul),
            'I' => Some(Instruction::Mul1),
            '@' => Some(Instruction::Mul2),
            'X' => Some(Instruction::Mul10),
            'C' => Some(Instruction::Mul100),
            'M' => Some(Instruction::Mul1000),
            'p' => Some(Instruction::Push),
            'P' => Some(Instruction::PushN),
            'R' => Some(Instruction::RangeTo),
            '.' => Some(Instruction::Repeat),
            _ => None,
        }
    }
}

pub struct Interpreter {
    stack: Vec<Value>,
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter { stack: Vec::new() }
    }

    pub fn evaluate(&mut self, source: &str) -> anyhow::Result<()> {
        let mut token_iter = parser::TokenIterator::new(source);
        while let Some(token) = token_iter.next() {
            match token {
                Err(e) => {
                    return Err(anyhow::anyhow!("{}", e.into_contextual(source)));
                }
                Ok((span, token)) => {
                    self.eval_token(token).map_err(|e| {
                        anyhow::anyhow!(
                            "{}",
                            chic::Error::new(e.to_string())
                                .error(
                                    span.line_number,
                                    span.char_offset,
                                    span.char_offset + span.length,
                                    source,
                                    "here",
                                )
                                .to_string()
                        )
                    })?;
                }
            }
        }

        let unpacked_stack_values = {
            if let Some((Value::List(values), &[])) = self.stack.split_last() {
                values
            } else {
                &self.stack
            }
        };

        for value in unpacked_stack_values {
            println!("{}", value);
        }

        Ok(())
    }

    fn eval_token(&mut self, token: Token) -> anyhow::Result<()> {
        match token {
            Token::NumberLiteral(n) => {
                self.stack.push(Value::Number(n));
                Ok(())
            }
            Token::Instruction(Instruction::Add) => {
                // Simple addition with top 2 values
                self.eval_binary_operator::<operator::Add>()
            }
            Token::Instruction(Instruction::Add1) => {
                self.push(Value::Number(1.0));
                self.eval_binary_operator::<operator::Add>()
            }
            Token::Instruction(Instruction::Mul) => {
                // Simple multiplication with top 2 values
                self.eval_binary_operator::<operator::Multiply>()
            }
            Token::Instruction(Instruction::Mul1) => {
                // No-op, this is basically a number literal separator
                Ok(())
            }
            Token::Instruction(Instruction::Mul2) => {
                self.push(Value::Number(2.0));
                self.eval_binary_operator::<operator::Multiply>()
            }
            Token::Instruction(Instruction::Mul10) => {
                self.push(Value::Number(10.0));
                self.eval_binary_operator::<operator::Multiply>()
            }
            Token::Instruction(Instruction::Mul100) => {
                self.push(Value::Number(100.0));
                self.eval_binary_operator::<operator::Multiply>()
            }
            Token::Instruction(Instruction::Mul1000) => {
                self.push(Value::Number(1000.0));
                self.eval_binary_operator::<operator::Multiply>()
            }
            Token::Instruction(Instruction::Push) => {
                if let Some(value) = self.stack.last().cloned() {
                    self.push(value);
                }

                Ok(())
            }
            Token::Instruction(Instruction::PushN) => {
                let count = match self.pop() {
                    Value::Number(n) => n as usize,
                    value => {
                        return Err(anyhow::anyhow!(
                            "Invalid type for push count (wanted a {}, got a {})",
                            TypeId::Number,
                            value.typeid()
                        ))
                    }
                };

                let values = (0..count).map(|i| self.peek(i)).collect::<Vec<Value>>();
                self.stack.extend(values.into_iter().rev());

                Ok(())
            }
            Token::Instruction(Instruction::RangeTo) => {
                let end = match self.pop() {
                    Value::Number(n) => n as i64,
                    value => {
                        return Err(anyhow::anyhow!(
                            "Invalid type for range end (wanted a {}, got a {})",
                            TypeId::Number,
                            value.typeid()
                        ))
                    }
                };

                self.push(Value::Range(0..end));
                Ok(())
            }
            Token::Instruction(Instruction::Repeat) => {
                let count = match self.pop() {
                    Value::Number(n) => n as usize,
                    value => {
                        return Err(anyhow::anyhow!(
                            "Invalid type for repeat count (wanted a {}, got a {})",
                            TypeId::Number,
                            value.typeid()
                        ))
                    }
                };

                let value_to_repeat = self.pop();
                let repeated_value = (0..count)
                    .into_iter()
                    .map(|_| value_to_repeat.clone())
                    .collect::<Vec<Value>>();

                self.push(Value::List(repeated_value));
                Ok(())
            }
        }
    }

    fn push(&mut self, value: Value) {
        self.stack.push(value);
    }

    fn pop(&mut self) -> Value {
        self.stack.pop().unwrap_or_else(|| Value::Number(0.0))
    }

    fn peek(&self, i: usize) -> Value {
        if i < self.stack.len() {
            self.stack[self.stack.len() - i - 1].clone()
        } else {
            Value::Number(0.0)
        }
    }

    fn eval_binary_operator<Op: operator::BinaryOperator>(&mut self) -> anyhow::Result<()> {
        // Replace ranges with lists if we're applying a binary operator
        for i in 0..2 {
            let index = self.stack.len() - i - 1;
            if let Some(value) = self.stack.get_mut(index) {
                if let Value::Range(range) = value {
                    *value = Value::List(range.map(|i| Value::Number(i as f64)).collect())
                }
            }
        }

        let value = match (self.pop(), self.pop()) {
            (Value::List(lhs), Value::List(rhs)) => {
                let result = lhs
                    .into_iter()
                    .flat_map(|lhs| {
                        rhs.iter()
                            .map(move |rhs| Op::apply_binary_operator(lhs.clone(), rhs.clone()))
                    })
                    .collect::<anyhow::Result<Vec<Value>>>()?;

                Value::List(result)
            }
            (Value::List(lhs), rhs) => {
                let result = lhs
                    .into_iter()
                    .map(|lhs| Op::apply_binary_operator(lhs, rhs.clone()))
                    .collect::<anyhow::Result<Vec<Value>>>()?;

                Value::List(result)
            }
            (lhs, Value::List(rhs)) => {
                let result = rhs
                    .into_iter()
                    .map(|rhs| Op::apply_binary_operator(lhs.clone(), rhs))
                    .collect::<anyhow::Result<Vec<Value>>>()?;

                Value::List(result)
            }
            (lhs, rhs) => Op::apply_binary_operator(lhs, rhs)?,
        };

        self.push(value);
        Ok(())
    }
}

mod operator {
    use super::Value;

    pub trait BinaryOperator {
        fn apply_binary_operator(lhs: Value, rhs: Value) -> anyhow::Result<Value>;
    }

    pub struct Multiply;

    impl BinaryOperator for Multiply {
        fn apply_binary_operator(lhs: Value, rhs: Value) -> anyhow::Result<Value> {
            match (lhs, rhs) {
                (Value::Number(lhs), Value::Number(rhs)) => Ok(Value::Number(lhs * rhs)),
                _ => Err(anyhow::anyhow!("Invalid types for multiplication")),
            }
        }
    }

    pub struct Add;

    impl BinaryOperator for Add {
        fn apply_binary_operator(lhs: Value, rhs: Value) -> anyhow::Result<Value> {
            match (lhs, rhs) {
                (Value::Number(lhs), Value::Number(rhs)) => Ok(Value::Number(lhs + rhs)),
                _ => Err(anyhow::anyhow!("Invalid types for addition")),
            }
        }
    }
}

mod parser {
    use std::{
        error::Error,
        fmt::{Debug, Display},
        iter::{Enumerate, Peekable},
        str::Chars,
    };

    use super::{Instruction, Token};

    #[derive(Debug)]
    pub struct Span {
        pub line_number: usize,
        pub char_offset: usize,
        pub length: usize,
    }

    #[derive(Debug)]
    pub struct TokenizerError {
        span: Span,
        message: String,
        label: Option<String>,
        help: Option<String>,
    }

    impl TokenizerError {
        pub fn into_contextual(self, source: &str) -> ContextualTokenizerError {
            ContextualTokenizerError {
                source,
                inner: self,
            }
        }
    }

    impl Display for TokenizerError {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "Tokenizer error: {}", self.message)?;
            if let Some(label) = &self.label {
                write!(f, " ({})", label)?;
            }

            write!(
                f,
                " at line {} char {}",
                self.span.line_number, self.span.char_offset
            )?;

            if let Some(help) = &self.help {
                write!(f, " - {}", help)?;
            }

            Ok(())
        }
    }

    #[derive(Debug)]
    pub struct ContextualTokenizerError<'a> {
        source: &'a str,
        inner: TokenizerError,
    }

    impl Error for ContextualTokenizerError<'_> {}

    impl Display for ContextualTokenizerError<'_> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            let mut printable_error = chic::Error::new(self.inner.message.clone()).error(
                self.inner.span.line_number,
                self.inner.span.char_offset,
                self.inner.span.char_offset + self.inner.span.length,
                self.source,
                self.inner
                    .label
                    .as_ref()
                    .unwrap_or_else(|| &self.inner.message),
            );

            if let Some(help) = self.inner.help.clone() {
                printable_error = printable_error.help(help);
            }

            write!(f, "{}", printable_error.to_string())
        }
    }

    pub struct TokenIterator<'a> {
        line_number: usize,
        inner_iter: Peekable<Enumerate<Chars<'a>>>,
    }

    impl TokenIterator<'_> {
        pub fn new(source: &str) -> TokenIterator {
            TokenIterator {
                line_number: 0,
                inner_iter: source.chars().enumerate().peekable(),
            }
        }
    }

    impl<'a> Iterator for TokenIterator<'a> {
        type Item = Result<(Span, Token), TokenizerError>;

        fn next(&mut self) -> Option<Self::Item> {
            match self.inner_iter.peek().cloned() {
                Some((char_offset, c)) => match c {
                    '0'..='9' => {
                        let mut number = String::new();
                        while let Some((_, c)) = self.inner_iter.peek() {
                            if c.is_digit(10) {
                                number.push(*c);
                                self.inner_iter.next();
                            } else {
                                break;
                            }
                        }

                        let span = Span {
                            line_number: self.line_number,
                            char_offset,
                            length: number.len(),
                        };

                        Some(Ok((span, Token::NumberLiteral(number.parse().unwrap()))))
                    }
                    '\n' => {
                        self.line_number += 1;
                        self.inner_iter.next();
                        self.next()
                    }
                    _ => match Instruction::try_from(c) {
                        Some(command) => {
                            self.inner_iter.next();
                            let span = Span {
                                line_number: self.line_number,
                                char_offset,
                                length: 1,
                            };

                            Some(Ok((span, Token::Instruction(command))))
                        }
                        None => Some(Err(TokenizerError {
                            span: Span {
                                line_number: self.line_number,
                                char_offset,
                                length: 1,
                            },
                            message: format!("Unknown instruction '{}'", c),
                            label: Some(String::from("unknown instruction")),
                            help: None,
                        })),
                    },
                },
                None => None,
            }
        }
    }
}
