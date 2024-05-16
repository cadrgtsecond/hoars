use std::collections::HashMap;
use std::error::Error;
use std::fmt::Write;

#[derive(Debug, PartialEq)]
pub enum Token<'a> {
    Word(&'a str),
    Number(i64),
    Str(&'a str),
}

pub struct Tokenizer<'a> {
    input: &'a str,
}

impl<'a> Tokenizer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self { input }
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.input = self.input.trim_start();
        if let Some(c @ ('\'' | '"')) = self.input.chars().next() {
            self.input = &self.input[1..];
            let (res, rest) = self.input.split_once(c)?;
            self.input = rest;
            return Some(Token::Str(res));
        }

        let next_whitespace = self
            .input
            .find(char::is_whitespace)
            .unwrap_or(self.input.len());
        let (res, rest) = self.input.split_at(next_whitespace);
        self.input = rest;

        match res.chars().next() {
            Some(c) if c.is_numeric() => Some(Token::Number(res.parse::<i64>().ok()?)),
            _ if !rest.is_empty() => Some(Token::Word(res)),
            _ => None,
        }
    }
}

type Word = fn(&VM);
pub struct VM<'a> {
    tokens: Tokenizer<'a>,
    ret_stack: Vec<Word>,
    dict_compile: HashMap<String, Word>,
}
impl<'a> VM<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            tokens: Tokenizer::new(input),
            ret_stack: vec![],
            dict_compile: HashMap::new(),
        }
    }

    pub fn run(&mut self, stdout: &mut dyn Write) -> Result<(), Box<dyn Error>> {
        let Some(word) = self.tokens.next() else {
            return Ok(());
        };
        let Token::Word(word) = word else {
            return Err("Expected word".into());
        };

        if word == "log" {
            match self.tokens.next().ok_or("Expected argument. Got nothing")? {
                Token::Number(n) => {
                    writeln!(stdout, "{}", n)?;
                    Ok(())
                }
                Token::Str(s) => {
                    writeln!(stdout, "{}", s)?;
                    Ok(())
                }
                Token::Word(_) => todo!("Variables not implemented yet"),
            }
        } else {
            Err("Unknown word".into())
        }
    }
}

#[cfg(test)]
mod tests {
    use std::io::stdin;

    use super::*;

    #[test]
    pub fn tokenizer_test() {
        let code = "let x be 10 then print x then print 'hello world!'";
        let mut tokens = Tokenizer::new(code);
        assert_eq!(tokens.next(), Some(Token::Word("let")));
        assert_eq!(tokens.next(), Some(Token::Word("x")));
        assert_eq!(tokens.next(), Some(Token::Word("be")));
        assert_eq!(tokens.next(), Some(Token::Number(10)));
        assert_eq!(tokens.next(), Some(Token::Word("then")));
        assert_eq!(tokens.next(), Some(Token::Word("print")));
        assert_eq!(tokens.next(), Some(Token::Word("x")));
        assert_eq!(tokens.next(), Some(Token::Word("then")));
        assert_eq!(tokens.next(), Some(Token::Word("print")));
        assert_eq!(tokens.next(), Some(Token::Str("hello world!")));
        assert_eq!(tokens.next(), None);
    }

    #[test]
    pub fn vm_run() -> Result<(), Box<dyn Error>> {
        let code = "log 'hello world'";
        let mut vm = VM::new(code);
        let mut output = String::new();
        vm.run(&mut output)?;
        assert_eq!(output, "hello world\n");
        Ok(())
    }
}
