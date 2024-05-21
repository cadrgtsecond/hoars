#![deny(clippy::pedantic)]

use std::arch::x86_64::_MM_FLUSH_ZERO_MASK;
use std::array::from_mut;
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
    #[must_use]
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
    /// <`link_ptr`> | <`len`> | <`name`> | <`code`> | parameters.....
    pub fn create_word(&mut self, name: &'static str, code: fn(&VM)) {
        let prev = self.top_word;
        self.top_word = self.dict_mem.len();
        self.push_dict(prev);
        self.push_dict(name.len());
        self.push_dict(name.as_ptr() as Value);
        self.push_dict(code as Value);
    }

    /// Pushes a word onto the virtual machine's dictionary
    pub fn push_dict(&mut self, val: Value) {
        self.dict_mem.push(val);
    }

    pub fn exec_word(&mut self, word_ptr: usize) {
        self.curr_word = word_ptr;
        unsafe {
            let code = word_ptr + 3;
            std::mem::transmute::<Value, fn(&VM)>(self.dict_mem[code])(self);
        }
    }

    /// Looks up a word in the dictionary
    pub fn lookup_word(&mut self, name: &str) -> Option<Value> {
        let mut curr = self.top_word;
        loop {
            // safe because we created it this way
            let curr_name: &[u8] = unsafe {
                let len = self.dict_mem[curr + 1];
                let ptr = self.dict_mem[curr + 2] as *const u8;
                std::slice::from_raw_parts(ptr, len)
            };
            if name.as_bytes() == curr_name {
                return Some(curr);
            }
            // The default entry always has an empty name
            if curr_name.len() == 0 {
                return Some(curr);
            }
            if curr == self.dict_mem[curr] {
                break;
            }
            curr = self.dict_mem[curr];
        }
        None
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

    fn create_words(vm: &mut VM) {
        let msg1 = "";
        let code1: fn(&VM) = |_| {
            panic!("Unknown word!");
        };
        vm.create_word(&msg1, code1);

        assert_eq!(
            vm.dict_mem,
            [0, msg1.len(), msg1.as_ptr() as Value, code1 as Value]
        );
        assert_eq!(vm.top_word, 0);

        let msg2 = "hello";
        let code2: fn(&VM) = |_| {
            panic!("Success!");
        };
        vm.create_word(&msg2, code2);

        assert_eq!(
            vm.dict_mem,
            [
                0,
                msg1.len(),
                msg1.as_ptr() as Value,
                code1 as Value,
                0,
                msg2.len(),
                msg2.as_ptr() as Value,
                code2 as Value
            ]
        );
        assert_eq!(vm.top_word, 4);

        let msg3 = "stop";
        let code3: fn(&VM) = |_| {
            panic!("Gege Akutami must be stopped");
        };
        vm.create_word(&msg3, code3);

        assert_eq!(
            vm.dict_mem,
            [
                0,
                msg1.len(),
                msg1.as_ptr() as Value,
                code1 as Value,
                0,
                msg2.len(),
                msg2.as_ptr() as Value,
                code2 as Value,
                4,
                msg3.len(),
                msg3.as_ptr() as Value,
                code3 as Value
            ]
        );
        assert_eq!(vm.top_word, 8);
    }

    #[test]
    #[should_panic(expected = "Gege Akutami must be stopped")]
    pub fn compile_hello_world() {
        let mut vm = VM::new("");
        create_words(&mut vm);
        vm.exec_word(8);
    }

    #[test]
    pub fn lookup_words() {
        let mut vm = VM::new("");
        create_words(&mut vm);
        assert_eq!(vm.lookup_word("hello"), Some(4));
        assert_eq!(vm.lookup_word("unknown"), Some(0));
        assert_eq!(vm.lookup_word("stop"), Some(8));
    }

    #[test]
    #[should_panic(expected = "Unknown word")]
    pub fn lookup_unknown_word() {
        let mut vm = VM::new("");
        create_words(&mut vm);
        let word = vm.lookup_word("unknown").unwrap();
        vm.exec_word(word);
    }
}
