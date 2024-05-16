use std::collections::HashMap;
use std::error::Error;
use std::fmt::Write;
use std::ptr::NonNull;

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

/// This type represents how values are represented within the virtual machine
///
/// Because this language is statically typed, we do not need to keep track of type tags
type Value = u64;

pub fn str_to_val(val: &&str) -> Value {
   (val as *const &str) as Value
}
type Word = fn(&VM);
pub struct VM<'a> {
    tokens: Tokenizer<'a>,
    heap_mem: Vec<Value>,
    dict_mem: Vec<Value>,
    curr_word: usize,

    ret_stack: Vec<Word>,
    var_stack: Vec<String>,
    typ_stack: Vec<Value>,
    val_stack: Vec<Value>,
}
impl<'a> VM<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            tokens: Tokenizer::new(input),
            ret_stack: vec![],
            var_stack: vec![],
            typ_stack: vec![],
            val_stack: vec![],
            heap_mem: vec![],
            dict_mem: vec![],
            curr_word: 0,
        }
    }

    pub fn create_word(&mut self, name: &&str, code: Word) {
        self.push_dict((name as *const &str) as Value);
        self.push_dict(code as Value);
    }
    pub fn push_dict(&mut self, val: Value) {
        self.dict_mem.push(val);
    }
    pub fn exec_word(&mut self, word_ptr: usize) {
      self.curr_word = word_ptr;
      unsafe {
        let code = word_ptr + 1;
        std::mem::transmute::<Value, Word>(self.dict_mem[code])(self);
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
    #[should_panic(expected = "Success")]
    pub fn compile_hello_world() {
        let mut vm = VM::new("");
        let name = "hello";
        let code: Word = |_| {
          panic!("Success!");
        };
        vm.create_word(&name, code);
        assert_eq!(vm.dict_mem, [(&name as *const &str) as Value, code as Value]);
        vm.exec_word(0);
    }
}
