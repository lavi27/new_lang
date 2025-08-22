use std::{fmt, ops::Range};

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
    curr_col: usize,
    curr_row_range: Range<usize>,
}

impl TokenStream {
    pub fn from(str: String) -> Self {
        Self {
            raw: str.into_bytes(),
            offset: 0,
            curr_col: 1,
            curr_row_range: Range { start: 1, end: 1 },
        }
    }

    pub fn peek(&mut self) -> Option<Token> {
        let prev_offest = self.offset;
        let res = self.next();
        self.offset = prev_offest;

        res
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

    pub fn get_curr_position(&self) -> (usize, Range<usize>) {
        (self.curr_col, self.curr_row_range.clone())
    }
}

impl Iterator for TokenStream {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let mut token_start = self.offset;
        let mut row_start_offset = self.offset;
        let mut is_expect_intager = false;

        while self.offset < self.raw.len() {
            match self.raw[self.offset] {
                b' ' | b'/t' => {
                    if token_start != self.offset { 
                        let result = String::from_utf8(self.raw[token_start..self.offset - 1].to_vec()).unwrap();
                        let result = if is_expect_intager {
                            Token::Intager(result)
                        } else {
                            Token::String(result)
                        };

                        self.curr_row_range.start = token_start - row_start_offset + 1;
                        self.curr_row_range.end = self.offset - 1 - row_start_offset + 1;
                        self.offset += 1;
                        return Some(result);
                    } else {
                        token_start += 1;
                    }
                }
                b'0'..b'9' => {
                    if token_start == self.offset {
                        is_expect_intager = true;
                    }
                }

                b'\n' => {
                    row_start_offset = self.curr_col;
                    self.curr_col += 1;
                }
                char => {
                    if let Some(token) = Token::char_byte_to_token(char) {
                        if token_start != self.offset {
                            let result =
                                String::from_utf8(self.raw[token_start..self.offset - 1].to_vec())
                                    .unwrap();
                            let result = if is_expect_intager {
                                Token::Intager(result)
                            } else {
                                Token::String(result)
                            };

                            self.curr_row_range.start = token_start - row_start_offset + 1;
                            self.curr_row_range.end = self.offset - 1 - row_start_offset + 1;
                            return Some(result);
                        } else {
                            self.curr_row_range.start = token_start - row_start_offset + 1;
                            self.curr_row_range.end = self.offset - row_start_offset + 1;
                            self.offset += 1;
                            return Some(token);
                        }
                    }

                    if token_start != self.offset && is_expect_intager {
                        self.curr_row_range.start = token_start - row_start_offset + 1;
                        self.curr_row_range.end = self.offset - row_start_offset + 1;
                        panic!("Number can't be prefix of any expression.");
                    }
                }
            }

            self.offset += 1;
        }

        None
    }
}
