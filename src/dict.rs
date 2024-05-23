use std::ops::Index;

use crate::{Value, VM};

pub struct Dictionary {
    mem: Vec<Value>,
    top_word: usize,
}
impl Dictionary {
    pub fn new() -> Self {
        Self {
            mem: vec![],
            top_word: 0,
        }
    }
    /// Pushes a word onto the virtual machine's dictionary
    pub fn push(&mut self, val: Value) {
        self.mem.push(val);
    }

    /// Creates a word.
    /// The structure of a word is as follows
    /// <`link_ptr`> | <`len`> | <`name`> | <`code`> | parameters.....
    pub fn create_word(&mut self, name: &'static str, code: fn(&mut VM)) {
        let prev = self.top_word;
        self.top_word = self.mem.len();
        self.push(prev);
        self.push(name.len());
        self.push(name.as_ptr() as Value);
        self.push(code as Value);
    }

    /// Looks up a word in the dictionary
    pub fn lookup_word(&self, name: &str) -> Option<Value> {
        let mut curr = self.top_word;
        loop {
            // safe because we created it this way
            let curr_name: &[u8] = unsafe {
                let len = self.mem[curr + 1];
                let ptr = self.mem[curr + 2] as *const u8;
                std::slice::from_raw_parts(ptr, len)
            };
            if name.as_bytes() == curr_name {
                return Some(curr);
            }
            // The default entry always has an empty name
            if curr_name.len() == 0 {
                return Some(curr);
            }
            if curr == self.mem[curr] {
                break;
            }
            curr = self.mem[curr];
        }
        None
    }

    /// Returns the total length of the dictionary memory
    pub fn len(&self) -> usize {
      self.mem.len()
    }
}

impl Default for Dictionary {
    fn default() -> Self {
        Self::new()
    }
}

impl Index<usize> for Dictionary {
    type Output = Value;

    fn index(&self, index: usize) -> &Self::Output {
      self.mem.index(index)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    fn create_words(dict: &mut Dictionary) {
        let msg1 = "";
        let code1:fn(&mut VM) = |_| {
            panic!("Unknown word!");
        };
        dict.create_word(msg1, code1);

        assert_eq!(
            dict.mem,
            [0, msg1.len(), msg1.as_ptr() as Value, code1 as Value]
        );
        assert_eq!(dict.top_word, 0);

        let msg2 = "hello";
        let code2: fn(&mut VM) = |_| {
            panic!("Success!");
        };
        dict.create_word(msg2, code2);

        assert_eq!(
            dict.mem,
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
        assert_eq!(dict.top_word, 4);

        let msg3 = "stop";
        let code3: fn(&mut VM) = |_| {
            panic!("Gege Akutami must be stopped");
        };
        dict.create_word(msg3, code3);

        assert_eq!(
            dict.mem,
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
        assert_eq!(dict.top_word, 8);
    }

    #[test]
    #[should_panic(expected = "Gege Akutami must be stopped")]
    pub fn compile_hello_world() {
        let mut vm = VM::new("", Dictionary::new());
        create_words(&mut vm.run_dict);
        vm.exec_runtime_word(8);
    }

    #[test]
    pub fn lookup_words() {
        let mut dict = Dictionary::new();
        create_words(&mut dict);
        assert_eq!(dict.lookup_word("hello"), Some(4));
        assert_eq!(dict.lookup_word("unknown"), Some(0));
        assert_eq!(dict.lookup_word("stop"), Some(8));
    }

    #[test]
    #[should_panic(expected = "Unknown word")]
    pub fn lookup_unknown_word() {
        let mut vm = VM::new("", Dictionary::new());
        create_words(&mut vm.run_dict);
        let word = vm.run_dict.lookup_word("unknown").unwrap();
        vm.exec_runtime_word(word);
    }
}
