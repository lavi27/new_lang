use std::{collections::VecDeque, fmt, ops::Range, sync::LazyLock};

use strum::IntoEnumIterator;
use strum_macros::EnumIter;
use trie_rs::{inc_search::Answer, Trie, TrieBuilder};

use crate::s;

macro_rules! define_tokens {
    ($($name:ident => $str:expr),* $(,)?) => {
        #[derive(PartialEq, Eq, Clone, EnumIter)]
        pub enum Token {
            String(String),
            Intager(String),
            $($name),*
        }

        impl Token {
            fn from_str(s: &str) -> Option<Self> {
                match s {
                    $($str => Some(Token::$name),)*
                    _ => None,
                }
            }

            fn as_str(&self) -> String {
                match self {
                    $(Token::$name => $str.to_string(),)*
                    Token::String(str) => str.to_owned(),
                    Token::Intager(int) => int.to_owned(),
                }
            }
        }
    }
}

define_tokens! {
    LeftParen => "(",
    RightParen => ")",
    LeftBrace => "{",
    RightBrace => "}",
    LeftBracket => "[",
    RightBracket => "]",
    LeftAngleBracket => "<",
    RightAngleBracket => ">",
    Comma => ",",
    Period => ".",
    Questionmark => "?",
    Exclamationmark => "!",
    Quote => "\'",
    DoubleQuote => "\"",
    Colon => ":",
    Semicolon => ";",
    Slash => "/",
    Asterisk => "*",
    Plus => "+",
    Minus => "-",
    Equal => "=",
    Namespace => "::",
    AddAssign => "+=",
    SubAssign => "-=",
    MulAssign => "*=",
    DivAssign => "/=",
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.as_str())
    }
}

static SYMBOL_TOKEN_TRIE: LazyLock<Trie<u8>> = LazyLock::new(|| {
    let mut builder = TrieBuilder::new();

    for var in Token::iter() {
        match var {
            Token::String(_) | Token::Intager(_) => continue,
            _ => {}
        };

        builder.push(var.as_str());
    }

    builder.build()
});

#[derive(PartialEq)]
enum TokenType {
    None,
    Integer,
    String,
    Symbol,
}

use TokenType as TT;

pub struct TokenStream {
    raw: Vec<u8>,
    offset: usize,
    token_start: usize,

    token_state: TokenType,
    row_start_offset: usize,
    curr_row: usize,
    curr_col_range: Range<usize>,

    peek_cache: VecDeque<(Token, usize)>,
}

impl TokenStream {
    pub fn from(str: String) -> Self {
        Self {
            raw: str.into_bytes(),
            offset: 0,
            curr_row: 1,
            token_state: TT::None,
            curr_col_range: Range { start: 1, end: 1 },
            peek_cache: VecDeque::with_capacity(8),
            token_start: 0,
            row_start_offset: 0,
        }
    }

    pub fn peek(&mut self) -> Option<Token> {
        self.peek_nth(0)
    }

    pub fn peek_nth(&mut self, n: usize) -> Option<Token> {
        let init_offest = self.offset;
        let init_row = self.curr_row;
        let init_col_range = self.curr_col_range.clone();

        let mut res = None;

        if let Some((token, _)) = self.peek_cache.get(n) {
            return Some(token.clone());
        }

        if let Some((token, offset)) = self.peek_cache.back() {
            self.offset = *offset;
            res = Some(token.clone());
        }

        for _ in self.peek_cache.len()..=n {
            res = self.next_inner();

            if let Some(res) = res.clone() {
                self.peek_cache.push_back((res, self.offset));
            } else {
                break;
            }
        }

        self.offset = init_offest;
        self.curr_row = init_row;
        self.curr_col_range = init_col_range;

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
        (self.curr_row, self.curr_col_range.clone())
    }

    fn next_inner(&mut self) -> Option<Token> {
        self.token_start = self.offset;
        self.row_start_offset = self.offset;
        self.token_state = TT::None;

        let res = self._next_inner();

        self.curr_col_range.start = self.token_start - self.row_start_offset + 1;
        self.curr_col_range.end = self.offset - self.row_start_offset;

        res
    }

    fn extract_token(&self) -> Token {
        let result = String::from_utf8(self.raw[self.token_start..self.offset].to_vec()).unwrap();

        match self.token_state {
            TT::Integer => Token::Intager(result),
            TT::String => Token::String(result),
            TT::Symbol => Token::from_str(&result).unwrap(),
            TT::None => panic!(),
        }
    }

    fn _next_inner(&mut self) -> Option<Token> {
        let mut trie_search = SYMBOL_TOKEN_TRIE.inc_search();

        while self.offset < self.raw.len() {
            match self.raw[self.offset] {
                b'\n' => {
                    if self.token_state != TT::None {
                        return Some(self.extract_token());
                    }

                    self.row_start_offset = self.offset;
                    self.curr_row += 1;
                    self.token_start += 1;
                }
                b' ' | b'\t' | b'\r' => {
                    if self.token_state != TT::None {
                        return Some(self.extract_token());
                    }

                    self.token_start += 1;
                }
                b'0'..=b'9' => match self.token_state {
                    TT::None => {
                        self.token_state = TT::Integer;
                    }
                    TT::Symbol => {
                        return Some(self.extract_token());
                    }
                    _ => {}
                },
                symbol if trie_search.peek(&symbol).is_some() => {
                    if self.token_state != TT::None && self.token_state != TT::Symbol {
                        return Some(self.extract_token());
                    }

                    self.token_state = TT::Symbol;

                    let query_res = trie_search.query(&symbol).unwrap();

                    if query_res == Answer::Match {
                        self.offset += 1;
                        return Some(self.extract_token());
                    }
                }
                char => match self.token_state {
                    TT::Integer => {
                        self.curr_col_range.start = self.token_start - self.row_start_offset + 1;
                        self.curr_col_range.end = self.offset - self.row_start_offset + 1;
                        panic!("Number can't be prefix of any expression.");
                    }
                    TT::Symbol => {
                        return Some(self.extract_token());
                    }
                    TT::None => self.token_state = TT::String,
                    _ => {}
                },
            }

            self.offset += 1;

            if self.token_state == TT::None {
                self.token_start = self.offset;
            }
        }

        None
    }
}

impl Iterator for TokenStream {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some((token, offset)) = self.peek_cache.pop_front() {
            self.offset = offset;
            return Some(token);
        };

        self.next_inner()
    }
}
