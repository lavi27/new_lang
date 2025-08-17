use std::fmt;

use crate::s;

#[derive(PartialEq, Eq, Clone)]
pub enum Token {
    String(String),
    Intager(String),
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
    /// <
    LeftAngleBracket,
    /// >
    RightAngleBracket,
    /// ,
    Comma,
    /// .
    Period,
    /// ?
    Questionmark,
    /// !
    Exclamationmark,
    /// '
    Quote,
    /// "
    DoubleQuote,
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
    /// =
    Equal,
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
            b'<' => Token::LeftAngleBracket,
            b'>' => Token::RightAngleBracket,
            b',' => Token::Comma,
            b'.' => Token::Period,
            b'?' => Token::Questionmark,
            b'!' => Token::Exclamationmark,
            b'\'' => Token::Quote,
            b'"' => Token::DoubleQuote,
            b':' => Token::Colon,
            b';' => Token::Semicolon,
            b'/' => Token::Slash,
            b'*' => Token::Asterisk,
            b'+' => Token::Plus,
            b'-' => Token::Minus,
            b'=' => Token::Equal,
            _ => return None,
        })
    }

    fn to_string(&self) -> String {
        match self {
            Token::String(str) => str.to_owned(),
            Token::Intager(int) => int.to_string(),
            Token::LeftParen => s!("("),
            Token::RightParen => s!(")"),
            Token::LeftBrace => s!("{"),
            Token::RightBrace => s!("}"),
            Token::LeftBracket => s!("["),
            Token::RightBracket => s!("]"),
            Token::LeftAngleBracket => s!("<"),
            Token::RightAngleBracket => s!(">"),
            Token::Comma => s!(","),
            Token::Period => s!("."),
            Token::Questionmark => s!("?"),
            Token::Exclamationmark => s!("!"),
            Token::Quote => s!("\'"),
            Token::DoubleQuote => s!("\""),
            Token::Colon => s!(":"),
            Token::Semicolon => s!(";"),
            Token::Slash => s!("/"),
            Token::Asterisk => s!("*"),
            Token::Plus => s!("+"),
            Token::Minus => s!("-"),
            Token::Equal => s!("="),
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.to_string());
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

    pub fn string_before_char(&mut self, char_byte: u8) -> String {
        let token_start = self.offset;

        while self.offset < self.raw.len() {
            if self.raw[self.offset] == char_byte {
                self.offset += 1;
                return String::from_utf8(self.raw[token_start..self.offset - 1].to_vec()).unwrap();
            }

            self.offset += 1;
        }

        panic!("Expected token: {}", char_byte);
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
