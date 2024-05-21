#![deny(clippy::pedantic)]

use std::error::Error;

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
    stdout: Box<dyn std::io::Write + 'a>,
}

impl<'a> VM<'a> {
    #[must_use]
    pub fn new(input: &'a str, compile_dict: Dictionary) -> Self {
        Self {
            tokens: Tokenizer::new(input),
            compile_dict,
            ret_stack: vec![],
            var_stack: vec![],
            type_stack: vec![],
            val_stack: vec![],
            heap_mem: vec![],
            curr_word: 0,
            stdout: Box::new(std::io::stdout()),
        }
    }

    /// Runs the code in the current virtual machine
    /// # Errors
    /// TODO
    pub fn run(&mut self) -> Result<(), Box<dyn Error>> {
        let Some(word) = self.tokens.next() else {
            return Ok(());
        };
        let Token::Word(word) = word else {
            return Err("Expected word".into());
        };
        let dict_entry = self
            .compile_dict
            .lookup_word(word)
            .ok_or("Dictionary lookup error")?;
        self.exec_compile_word(dict_entry);
        Ok(())
    }

    pub fn exec_compile_word(&mut self, word_ptr: usize) {
        self.curr_word = word_ptr;
        unsafe {
            let code = word_ptr + 3;
            std::mem::transmute::<Value, fn(&mut VM)>(self.compile_dict[code])(self);
        }
    }
}

fn word_log(vm: &mut VM) {
    match vm.tokens.next().expect("Expected argument. Got nothing") {
        Token::Number(n) => {
            writeln!(vm.stdout ,"{n}");
        }
        Token::Str(s) => {
            writeln!(vm.stdout, "{s}");
        }
        Token::Word(_) => todo!("Variables not implemented yet"),
    }
}

fn default_compile_dictionary() -> Dictionary {
    let mut dict = Dictionary::new();
    dict.create_word("log", word_log);
    dict
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
    pub fn hello_test() -> Result<(), Box<dyn Error>> {
        let mut output = Vec::new();
        {
            let mut vm = VM::new("log 'Hello World!'", default_compile_dictionary());
            vm.stdout = Box::new(&mut output);
            vm.run()?;
        }
        assert_eq!(output, "Hello World!\n".as_bytes());
        Ok(())
    }
}
