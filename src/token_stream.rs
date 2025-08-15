use std::fmt;

#[derive(PartialEq, Eq)]
pub enum Token {
    String(String),
    /// (
    LeftParen,
    /// )
    RightParen,
    /// {
    LeftBrace,
    /// }
    RightBrace,
    /// [
    LeftBracket,
    /// ]
    RightBracket,
    /// ,
    Comma,
    /// .
    Period,
    /// ?
    Questionmark,
    /// !
    Exclamationmark,
    /// " or '
    Quote,
    /// :
    Colon,
    /// ;
    Semicolon,
    /// /
    Slash,
    /// \*
    Asterisk,
    /// +
    Plus,
    /// -
    Minus,
}

impl Token {
    fn char_byte_to_token(char: u8) -> Option<Token> {
        Some(match char {
            b'(' => Token::LeftParen,
            b')' => Token::RightParen,
            b'{' => Token::LeftBrace,
            b'}' => Token::RightBrace,
            b'[' => Token::LeftBracket,
            b']' => Token::RightBracket,
            b',' => Token::Comma,
            b'.' => Token::Period,
            b'?' => Token::Questionmark,
            b'!' => Token::Exclamationmark,
            b'\'' | b'"' => Token::Quote,
            b':' => Token::Colon,
            b';' => Token::Semicolon,
            b'/' => Token::Slash,
            b'*' => Token::Asterisk,
            b'+' => Token::Plus,
            b'-' => Token::Minus,
            _ => return None,
        })
    }

    fn to_str(&self) -> &str {
        match self {
            Token::String(str) => str,
            Token::LeftParen => "(",
            Token::RightParen => ")",
            Token::LeftBrace => "{",
            Token::RightBrace => "}",
            Token::LeftBracket => "[",
            Token::RightBracket => "]",
            Token::Comma => ",",
            Token::Period => ".",
            Token::Questionmark => "?",
            Token::Exclamationmark => "!",
            Token::Quote => "\"",
            Token::Colon => ":",
            Token::Semicolon => ";",
            Token::Slash => "/",
            Token::Asterisk => "*",
            Token::Plus => "+",
            Token::Minus => "-",
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.to_str());
        Ok(())
    }
}

pub struct TokenStream {
    raw: Vec<u8>,
    offset: usize,
}

impl TokenStream {
    pub fn from(str: String) -> Self {
        Self {
            raw: str.into_bytes(),
            offset: 0,
        }
    }

    pub fn peek(&mut self) -> Option<Token> {
        let prev_offest = self.offset;
        let res = self.next();

        if res.is_some() {
            self.offset = prev_offest;
            res
        } else {
            None
        }
    }
}

impl Iterator for TokenStream {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let mut token_start = self.offset;

        while self.offset < self.raw.len() {
            match self.raw[self.offset] {
                b' ' => {
                    if token_start != self.offset {
                        self.offset += 1;
                        return Some(Token::String(
                            String::from_utf8(self.raw[token_start..self.offset - 1].to_vec())
                                .unwrap(),
                        ));
                    } else {
                        token_start += 1;
                    }
                }
                char => {
                    if let Some(token) = Token::char_byte_to_token(char) {
                        if token_start != self.offset {
                            return Some(Token::String(
                                String::from_utf8(self.raw[token_start..self.offset].to_vec())
                                    .unwrap(),
                            ));
                        } else {
                            self.offset += 1;
                            return Some(token);
                        }
                    }
                }
            }

            self.offset += 1;
        }

        None
    }
}
