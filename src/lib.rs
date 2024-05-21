#![deny(clippy::pedantic)]

use std::arch::x86_64::_MM_FLUSH_ZERO_MASK;
use std::array::from_mut;
use std::error::Error;
use std::fmt::Write;

use dict::Dictionary;

mod dict;

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
    #[must_use]
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

/// This type represents how values are represented within the virtual machine
///
/// Because this language is statically typed, we do not need to keep track of type tags
type Value = usize;

pub struct VM<'a> {
    tokens: Tokenizer<'a>,
    pub compile_dict: Dictionary,
    heap_mem: Vec<Value>,
    curr_word: usize,

    ret_stack: Vec<usize>,

    type_stack: Vec<Value>,
    var_stack: Vec<String>,
    val_stack: Vec<Value>,
}
impl<'a> VM<'a> {
    #[must_use]
    pub fn new(input: &'a str) -> Self {
        Self {
            tokens: Tokenizer::new(input),
            compile_dict: Dictionary::new(),
            ret_stack: vec![],
            var_stack: vec![],
            type_stack: vec![],
            val_stack: vec![],
            heap_mem: vec![],
            curr_word: 0,
        }
    }

    /// Runs the code in the current virtual machine
    /// # Errors
    /// TODO
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
                    writeln!(stdout, "{n}")?;
                    Ok(())
                }
                Token::Str(s) => {
                    writeln!(stdout, "{s}")?;
                    Ok(())
                }
                Token::Word(_) => todo!("Variables not implemented yet"),
            }
        } else {
            Err("Unknown word".into())
        }
    }

    pub fn exec_compile_word(&mut self, word_ptr: usize) {
        self.curr_word = word_ptr;
        unsafe {
            let code = word_ptr + 3;
            std::mem::transmute::<Value, fn(&VM)>(self.compile_dict[code])(self);
        }
    }
}

#[cfg(test)]
mod tests {
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
}
