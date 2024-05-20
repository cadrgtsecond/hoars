use std::collections::HashMap;
use std::error::Error;
use std::fmt::Write;
use std::marker::PhantomData;
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
type Value = usize;

pub fn str_to_val(val: &&str) -> Value {
    (val as *const &str) as Value
}

pub struct VM<'a> {
    tokens: Tokenizer<'a>,
    heap_mem: Vec<Value>,
    dict_mem: Vec<Value>,
    top_word: usize,
    curr_word: usize,

    ret_stack: Vec<usize>,

    type_stack: Vec<Value>,
    var_stack: Vec<String>,
    val_stack: Vec<Value>,
}
impl<'a> VM<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            tokens: Tokenizer::new(input),
            ret_stack: vec![],
            var_stack: vec![],
            type_stack: vec![],
            val_stack: vec![],
            heap_mem: vec![],
            dict_mem: vec![],
            top_word: 0,
            curr_word: 0,
        }
    }

    /// Creates a word.
    /// The structure of a word is as follows
    /// <link_ptr> | <name pointer> | <code pointer> | <parameters>.....
    pub fn create_word(&mut self, name: &&str, code: fn(&VM)) {
        let prev = self.top_word;
        self.top_word = self.dict_mem.len();
        self.push_dict(prev);
        self.push_dict((name as *const &str) as Value);
        self.push_dict(code as Value);
    }
    pub fn push_dict(&mut self, val: Value) {
        self.dict_mem.push(val);
    }
    pub fn exec_word(&mut self, word_ptr: usize) {
        self.curr_word = word_ptr;
        unsafe {
            let code = word_ptr + 2;
            std::mem::transmute::<Value, fn(&VM)>(self.dict_mem[code])(self);
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
    #[should_panic(expected = "Gege Akutami must be stopped")]
    pub fn compile_hello_world() {
        let mut vm = VM::new("");
        let msg1 = "hello";
        let code1: fn(&VM) = |_| {
            panic!("Success!");
        };
        vm.create_word(&msg1, code1);

        assert_eq!(
            vm.dict_mem,
            [0, (&msg1 as *const &str) as Value, code1 as Value]
        );

        let msg2 = "world";
        let code2: fn(&VM) = |_| {
            panic!("Failure!");
        };
        vm.create_word(&msg2, code2);

        assert_eq!(
            vm.dict_mem,
            [0, (&msg1 as *const &str) as Value, code1 as Value, 0, (&msg2 as *const &str) as Value, code2 as Value]
        );

        let msg3 = "stop";
        let code3: fn(&VM) = |_| {
            panic!("Gege Akutami must be stopped");
        };
        vm.create_word(&msg3, code3);

        assert_eq!(
            vm.dict_mem,
            [0, (&msg1 as *const &str) as Value, code1 as Value, 0, (&msg2 as *const &str) as Value, code2 as Value, 3, (&msg3 as *const &str) as Value, code3 as Value]
        );
        vm.exec_word(6);
    }
}
