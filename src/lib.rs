#![deny(clippy::pedantic)]

use std::error::Error;

use dict::Dictionary;
use tokenizer::{Token, Tokenizer};

mod dict;
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
    pub run_dict: Dictionary,
    heap_mem: Vec<Value>,
    curr_word: usize,
    next_word: usize,

    ret: usize,

    type_stack: Vec<Value>,
    var_stack: Vec<String>,
    val_stack: Vec<Value>,
    stdout: Box<dyn std::io::Write + 'a>,
}

impl<'a> VM<'a> {
    #[must_use]
    pub fn new(input: &'a str, run_dict: Dictionary) -> Self {
        Self {
            tokens: Tokenizer::new(input),
            run_dict,
            var_stack: vec![],
            type_stack: vec![],
            val_stack: vec![],
            heap_mem: vec![],
            curr_word: 0,
            stdout: Box::new(std::io::stdout()),
            next_word: 0,
            ret: 0,
        }
    }

    /// Runs the code in the current virtual machine. Returning the [`Value`] returned upon
    /// evaluating said code
    /// # Errors
    /// TODO
    pub fn run(&mut self) -> Result<Value, Box<dyn Error>> {
        let Some(word) = self.tokens.next() else {
            return Ok(0);
        };
        let Token::Word(word) = word else {
            return Err("Expected word".into());
        };
        let dict_entry = self
            .run_dict
            .lookup_word(word)
            .ok_or("Dictionary lookup error")?;
        self.exec_runtime_word(dict_entry);
        Ok(self.ret)
    }

    /// Reads tokens from [`Tokenizer`] and compiles an expression onto `comptime_dict`
    /// ## Panics
    /// If builtins like `__value` are not found
    pub fn compile_expr(&mut self) {
        let Some(token) = self.tokens.next() else {
            return;
        };
        match token {
            Token::Word(w) => {
                if let Some(word) = self.run_dict.lookup_word(w) {
                    self.run_dict.push(word);
                }
            }
            Token::Number(n) => {
                let push_value = self
                    .run_dict
                    .lookup_word("__value")
                    .expect("Builtin __value not found. Cannot continue forward");
                self.run_dict.push(push_value);
                self.run_dict
                    .push(n.try_into().expect("Numeric literal out of range"));
            }
            Token::Str(_) => {
                todo!("Strings to be implemented")
            }
        }
    }

    pub fn exec_runtime_word(&mut self, word_ptr: usize) {
        self.curr_word = word_ptr;
        unsafe {
            let code = word_ptr + 3;
            std::mem::transmute::<Value, fn(&mut VM)>(self.run_dict[code])(self);
        }
    }
}

fn builtin_value(vm: &mut VM) {
    let val = vm.run_dict[vm.next_word];
    vm.ret = val;
}
fn builtin_pointer(vm: &mut VM) {
    vm.ret = vm.next_word;
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

pub fn default_compile_dictionary() -> Dictionary {
    let mut dict = Dictionary::new();
    dict.create_word("__value", builtin_value);
    dict.create_word("__pointer", builtin_pointer);
    dict.create_word("log", word_log);
    dict
}

#[cfg(test)]
mod tests {
    use super::*;

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
