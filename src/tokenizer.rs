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
