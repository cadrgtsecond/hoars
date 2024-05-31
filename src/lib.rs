#![deny(clippy::pedantic)]

use std::{collections::HashMap, error::Error};

use fetch::{Fetch, Ptr};
use tokenizer::{Token, Tokenizer};

mod fetch;
mod tokenizer;

/// This type represents how values are represented within the virtual machine
///
/// Because this language is statically typed, we do not need to keep track of type tags
type Value = usize;
/// A pointer to some native code
type CodePtr = fn(&mut VM);

pub struct VM<'a> {
    tokens: Tokenizer<'a>,
    run_dict: HashMap<String, Ptr<CodePtr>>,
    heap_mem: Vec<Value>,
    curr_word: Ptr<CodePtr>,
    /// Address of the current word being compiled
    curr_code: Ptr<CodePtr>,

    ret: Ptr<Value>,

    stdout: Box<dyn std::io::Write + 'a>,
}

impl<'a> VM<'a> {
    #[must_use]
    pub fn new(input: &'a str) -> Self {
        Self {
            tokens: Tokenizer::new(input),
            run_dict: HashMap::new(),
            heap_mem: Vec::with_capacity(4096),
            curr_word: Ptr::new(0),
            stdout: Box::new(std::io::stdout()),
            ret: Ptr::new(0),
            curr_code: Ptr::new(0),
        }
    }

    /// Runs the code in the current virtual machine. Returning the [`Value`] returned upon
    /// evaluating said code
    /// # Errors
    /// TODO
    pub fn run(&mut self) -> Result<Ptr<Value>, Box<dyn Error>> {
        let addr = self.heap_mem.len();
        self.compile_expr();
        self.exec_code(Ptr::new(addr));
        Ok(self.ret)
    }

    /// Reads tokens from [`Tokenizer`] and compiles an expression onto `comptime_dict`
    /// ## Panics
    /// If builtins like `__value` are not found
    pub fn compile_expr(&mut self) {
        let Some(token) = self.tokens.next() else {
            return;
        };
        self.heap_mem.push(exec_compiled as Value);
        match token {
            Token::Word(w) => match self.run_dict.get(w) {
                Some(word) => self.heap_mem.push(word.inner()),
                None => todo!(),
            },
            Token::Number(n) => {
                self.heap_mem.push(builtin_value as Value);
                self.heap_mem.push(n.try_into().expect("Numeric literal out of range"));
            }
            Token::Str(_) => {
                todo!("Strings to be implemented")
            }
        }
    }

    pub fn exec_code(&mut self, code: Ptr<CodePtr>) {
        let old = self.curr_word;
        self.curr_word = code;
        code.fetch(&self.heap_mem)(self);
        self.curr_word = old;
    }

    fn create_word(&mut self, arg: &str, word: fn(&mut VM<'static>)) {
      self.heap_mem.push(word as Value);
      self.run_dict.insert(arg.to_string(), Ptr::new(self.heap_mem.len() - 1));
    }
}

fn exec_compiled(vm: &mut VM) {
  unsafe {
    vm.curr_code = vm.curr_code.offset(1);
    vm.exec_code(vm.curr_code);
  }
}

fn builtin_value(vm: &mut VM) {
    unsafe {
        vm.curr_code = vm.curr_code.offset(1);
        vm.ret = vm.curr_code.cast_to().fetch(&vm.heap_mem);
    }
}

fn word_log(vm: &mut VM) {
    match vm.tokens.next().expect("Expected argument. Got nothing") {
        Token::Number(n) => {
            writeln!(vm.stdout, "{n}").unwrap();
        }
        Token::Str(s) => {
            writeln!(vm.stdout, "{s}").unwrap();
        }
        Token::Word(_) => todo!("Variables not implemented yet"),
    }
}

pub fn create_default_words(vm: &mut VM) {
    vm.create_word("__value", builtin_value);
    vm.create_word("log", word_log);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn hello_test() -> Result<(), Box<dyn Error>> {
        let mut output = Vec::new();
        {
            let mut vm = VM::new("log 'Hello World!'");
            create_default_words(&mut vm);
            vm.stdout = Box::new(&mut output);
            vm.run()?;
        }
        assert_eq!(output, "Hello World!\n".as_bytes());
        Ok(())
    }
}
