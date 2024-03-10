use std::{fmt::Display, ops::Range};

#[derive(Debug, Clone)]
pub enum Value {
    Number(f64),
    List(Vec<Value>),
    Range(Range<i64>),
    String(String),
}

impl Value {
    pub fn typeid(&self) -> TypeId {
        match self {
            Value::Number(_) => TypeId::Number,
            Value::List(_) => TypeId::List,
            Value::Range(_) => TypeId::Range,
            Value::String(_) => TypeId::String,
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
            Value::String(s) => write!(f, "{}", s),
        }
    }
}

#[derive(Debug)]
pub enum TypeId {
    Number,
    List,
    Range,
    String,
}

impl Display for TypeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeId::Number => write!(f, "number"),
            TypeId::List => write!(f, "list"),
            TypeId::Range => write!(f, "range"),
            TypeId::String => write!(f, "string"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Token {
    Instruction(Instruction),
    NumberLiteral(f64),
    StringLiteral(String),
}

#[derive(Debug, Clone)]
pub enum Instruction {
    Add,
    Add1,
    Exec,
    For,
    Mul,
    Mul1,
    Mul2,
    Mul10,
    Mul100,
    Mul1000,
    Push,
    PushLoopIteration,
    PushLoopElement,
    PushN,
    PushSource,
    RangeTo,
    Repeat,
}

impl Instruction {
    pub fn try_from(c: char) -> Option<Instruction> {
        match c {
            '+' => Some(Instruction::Add),
            'A' => Some(Instruction::Add1),
            'E' => Some(Instruction::Exec),
            'F' => Some(Instruction::For),
            '*' => Some(Instruction::Mul),
            'I' => Some(Instruction::Mul1),
            '@' => Some(Instruction::Mul2),
            'X' => Some(Instruction::Mul10),
            'C' => Some(Instruction::Mul100),
            'M' => Some(Instruction::Mul1000),
            'p' => Some(Instruction::Push),
            '#' => Some(Instruction::PushLoopIteration),
            '_' => Some(Instruction::PushLoopElement),
            'P' => Some(Instruction::PushN),
            '$' => Some(Instruction::PushSource),
            'R' => Some(Instruction::RangeTo),
            '.' => Some(Instruction::Repeat),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
struct LoopContext {
    element: Value,
    iteration: Value,
}

pub struct Interpreter {
    stack: Vec<Value>,
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter { stack: Vec::new() }
    }

    pub fn exit_values<'a>(&'a self) -> &'a [Value] {
        if let Some((Value::List(values), &[])) = self.stack.split_last() {
            values
        } else {
            &self.stack
        }
    }

    pub fn eval(&mut self, source: &str) -> anyhow::Result<()> {
        let source = source.trim();
        self.eval_with_context(source, source, None)
    }

    fn eval_with_context(
        &mut self,
        source: &str,
        global_source: &str,
        loop_context: Option<LoopContext>,
    ) -> anyhow::Result<()> {
        let mut token_iter = parser::TokenIterator::new(source);
        while let Some(token) = token_iter.next() {
            match token {
                Err(e) => {
                    return Err(anyhow::anyhow!("{}", e.into_contextual(source)));
                }
                Ok((span, token)) => {
                    self.eval_token(token, global_source, loop_context.clone())
                        .map_err(|e| {
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

        Ok(())
    }

    fn eval_token(
        &mut self,
        token: Token,
        global_source: &str,
        loop_context: Option<LoopContext>,
    ) -> anyhow::Result<()> {
        match token {
            Token::NumberLiteral(n) => {
                self.stack.push(Value::Number(n));
                Ok(())
            }
            Token::StringLiteral(s) => {
                self.stack.push(Value::String(s));
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
            Token::Instruction(Instruction::Exec) => {
                let value = self.pop();
                match value {
                    Value::String(s) => {
                        let mut interpreter = Interpreter::new();
                        interpreter.eval(&s)?;
                        self.stack.extend(interpreter.stack);
                    }
                    value => {
                        return Err(anyhow::anyhow!(
                            "Invalid type for exec (wanted a {}, got a {})",
                            TypeId::String,
                            value.typeid()
                        ))
                    }
                }

                Ok(())
            }
            Token::Instruction(Instruction::For) => match (self.pop(), self.pop()) {
                (Value::String(body), Value::Range(range)) => {
                    for (i, v) in range.enumerate() {
                        self.eval_with_context(
                            &body,
                            global_source,
                            Some(LoopContext {
                                element: Value::Number(v as f64),
                                iteration: Value::Number(i as f64),
                            }),
                        )?;
                    }

                    Ok(())
                }
                (Value::String(body), Value::Number(count)) => {
                    for i in 0..count as i64 {
                        self.eval_with_context(
                            &body,
                            global_source,
                            Some(LoopContext {
                                element: Value::Number(i as f64),
                                iteration: Value::Number(i as f64),
                            }),
                        )?;
                    }

                    Ok(())
                }
                (lhs, rhs) => {
                    return Err(anyhow::anyhow!(
                        "Invalid types for for loop (got a {} and a {})",
                        lhs.typeid(),
                        rhs.typeid()
                    ))
                }
            },
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
                self.push(self.peek(0));
                Ok(())
            }
            Token::Instruction(Instruction::PushLoopIteration) => {
                if let Some(LoopContext { iteration, .. }) = loop_context {
                    self.push(iteration);
                } else {
                    return Err(anyhow::anyhow!(
                        "No loop context available for push loop iteration"
                    ));
                }

                Ok(())
            }
            Token::Instruction(Instruction::PushLoopElement) => {
                if let Some(LoopContext { element, .. }) = loop_context {
                    self.push(element);
                } else {
                    return Err(anyhow::anyhow!(
                        "No loop context available for push loop element"
                    ));
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
            Token::Instruction(Instruction::PushSource) => {
                self.push(Value::String(global_source.to_string()));
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
                (Value::Number(n), Value::String(s)) => Ok(Value::String(s.repeat(n as usize))),
                (lhs, rhs) => Err(anyhow::anyhow!(
                    "Invalid types for multiplication ({} and {})",
                    lhs.typeid(),
                    rhs.typeid()
                )),
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

    #[derive(Debug, Clone, Copy)]
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
        yield_next: Option<(Span, Token)>,
        inner_iter: Peekable<Enumerate<Chars<'a>>>,
    }

    impl TokenIterator<'_> {
        pub fn new(source: &str) -> TokenIterator {
            TokenIterator {
                line_number: 0,
                yield_next: None,
                inner_iter: source.chars().enumerate().peekable(),
            }
        }
    }

    impl<'a> Iterator for TokenIterator<'a> {
        type Item = Result<(Span, Token), TokenizerError>;

        fn next(&mut self) -> Option<Self::Item> {
            if let Some(next) = self.yield_next.take() {
                return Some(Ok(next));
            }

            match self.inner_iter.peek().cloned() {
                Some((char_offset, c)) => match c {
                    '\n' => {
                        self.line_number += 1;
                        self.inner_iter.next();
                        self.next()
                    }
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
                    '"' => {
                        self.inner_iter.next();
                        let mut string = String::new();
                        while let Some((_, c)) = self.inner_iter.peek() {
                            if *c == '"' {
                                self.inner_iter.next();
                                break;
                            } else {
                                string.push(*c);
                                self.inner_iter.next();
                            }
                        }

                        let span = Span {
                            line_number: self.line_number,
                            char_offset,
                            length: string.len(),
                        };

                        Some(Ok((span, Token::StringLiteral(string))))
                    }
                    '{' => {
                        self.inner_iter.next();
                        let mut string = String::new();
                        while let Some((_, c)) = self.inner_iter.peek() {
                            if *c == '}' {
                                self.inner_iter.next();
                                break;
                            } else {
                                string.push(*c);
                                self.inner_iter.next();
                            }
                        }

                        let span = Span {
                            line_number: self.line_number,
                            char_offset,
                            length: string.len(),
                        };

                        self.yield_next = Some((span, Token::Instruction(Instruction::For)));
                        Some(Ok((span, Token::StringLiteral(string))))
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
