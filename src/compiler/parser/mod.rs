mod token_stream;

use std::{
    mem::take,
    ops::Range,
    panic::{self, AssertUnwindSafe},
};

use crate::{
    compiler::{codegen::ToRust, exprs::*},
    s,
};

use token_stream::*;

pub struct SyntaxTree {
    pub main_routine: Vec<Expr>,
    pub is_threading_used: bool,
}

impl Default for SyntaxTree {
    fn default() -> Self {
        Self {
            main_routine: Vec::new(),
            is_threading_used: false,
        }
    }
}

pub struct Parser {
    token_stream: TokenStream,
    filename: String,
    result: SyntaxTree,
}

impl Parser {
    pub fn parse_static(filename: String, input_code: String) -> Result<SyntaxTree, String> {
        let mut parser = Self::new(filename, input_code);
        parser.parse()
    }

    pub fn new(filename: String, input_code: String) -> Self {
        Self {
            token_stream: TokenStream::from(input_code),
            filename,
            result: SyntaxTree::default(),
        }
    }

    pub fn parse(&mut self) -> Result<SyntaxTree, String> {
        #[cfg(not(debug_assertions))]
        {
            panic::set_hook(Box::new(|_| {}));
        }

        while self.token_stream.peek().is_some() {
            let line_res = panic::catch_unwind(AssertUnwindSafe(|| self.parse_codeline()));
            let Ok(line) = line_res else {
                let (col, Range { start, end }) = self.token_stream.get_curr_position();
                let err_msg = unsafe { line_res.unwrap_err_unchecked() };

                #[cfg(not(debug_assertions))]
                {
                    panic::take_hook();
                }

                let err_msg = if let Some(err_msg) = err_msg.downcast_ref::<String>() {
                    err_msg.to_string()
                } else if let Some(err_msg) = err_msg.downcast_ref::<&str>() {
                    err_msg.to_string()
                } else {
                    panic!("Hell nah.");
                };

                return Err(format!(
                    "Syntax Error (at {}:{col}:{start}): {}",
                    self.filename, err_msg
                ));
            };

            self.result.main_routine.push(line);
        }

        #[cfg(not(debug_assertions))]
        {
            panic::take_hook();
        }
        return Ok(take(&mut self.result));
    }

    // # LOW LEVEL METHODS #
    // self.token_stream을 직접 조작하는 저수준 매서드.

    fn assert_next_token(&mut self, token: Token) {
        let curr_token = self
            .token_stream
            .next()
            .expect("Expected '{token}'. But file is ended.");

        if curr_token != token {
            panic!("Expected '{token}'. Found '{curr_token}'.");
        }
    }

    fn get_string_token(&mut self) -> String {
        let curr_token = self
            .token_stream
            .next()
            .expect("Expected some name. But file is ended.");

        let Token::String(str) = curr_token else {
            panic!("Expected some name. Found '{curr_token}'.");
        };

        return str;
    }

    /// This method is syntax-sugary.
    ///
    /// When there is a same token, token_stream.next() called.
    ///
    /// When there is no same token, stream does not move to next.
    ///
    /// To high abstraction logic, this method isn't recomended to use.
    #[must_use]
    fn is_next_token(&mut self, token: Token) -> bool {
        let curr_token = self.token_stream.peek();

        let Some(curr_token) = curr_token else {
            return false;
        };

        if curr_token == token {
            self.token_stream.next();
            true
        } else {
            false
        }
    }

    // # PARSE_COMMA_- METHODS #
    // 콤마로 나뉜 표현식을 처리하기 위해 특화된 저수준 매서드.

    fn parse_comma_value_exprs(&mut self, end_token: Token) -> Vec<ValueExpr> {
        let mut items = Vec::new();

        if self.is_next_token(end_token.clone()) {
            return items;
        }

        items.push(self.parse_value_exprs());

        while !self.is_next_token(end_token.clone()) {
            self.assert_next_token(Token::Comma);
            items.push(self.parse_value_exprs());
        }

        return items;
    }

    fn parse_comma_define_exprs(&mut self, end_token: Token) -> Vec<VariableDefineExpr> {
        let mut items = Vec::new();

        if self.is_next_token(end_token.clone()) {
            return items;
        }

        items.push(self.parse_variable_define_expr());

        while !self.is_next_token(end_token.clone()) {
            self.assert_next_token(Token::Comma);

            items.push(self.parse_variable_define_expr());
        }

        return items;
    }

    fn parse_comma_type_exprs(&mut self, end_token: Token) -> Vec<TypeExpr> {
        let mut items = Vec::new();

        if self.is_next_token(end_token.clone()) {
            return items;
        }

        items.push(self.parse_type_expr());

        while !self.is_next_token(end_token.clone()) {
            self.assert_next_token(Token::Comma);

            items.push(self.parse_type_expr());
        }

        return items;
    }

    fn parse_namespace_tree(&mut self) -> NamespaceTree {
        let mut body = vec![self.get_string_token()];

        loop {
            if !self.is_next_token(Token::Namespace) {
                return NamespaceTree(body, None);
            };

            if self.is_next_token(Token::LeftBrace) {
                break;
            };

            body.push(self.get_string_token());
        }

        let mut tail = vec![self.parse_namespace_tree()];
        while self.is_next_token(Token::Comma) {
            tail.push(self.parse_namespace_tree());
        }

        self.assert_next_token(Token::RightBrace);

        NamespaceTree(body, Some(tail))
    }

    // # TRY- METHODS #
    // 조건 검증과 처리를 동시에 수행하는 매서드들, 여러번 호출해도 무방한 것들. 인자 외에는 호출시 조건이 필요 없음.
    // if let Some(_) = ... 구조로 사용할 것.

    fn try_parse_type_args(&mut self) -> Option<Vec<TypeExpr>> {
        let restore_offset = self.token_stream.offset;

        if !self.is_next_token(Token::LeftAngleBracket) {
            return None;
        };

        let mut items = Vec::new();

        if self.is_next_token(Token::RightAngleBracket) {
            return Some(items);
        }

        let Some(type_expr) = self._try_parse_type_expr() else {
            self.token_stream.restore(restore_offset);
            return None;
        };
        items.push(type_expr);

        while !self.is_next_token(Token::RightAngleBracket) {
            if !self.is_next_token(Token::Comma) {
                self.token_stream.restore(restore_offset);
                return None;
            }

            let Some(type_expr) = self._try_parse_type_expr() else {
                self.token_stream.restore(restore_offset);
                return None;
            };
            items.push(type_expr);
        }

        return Some(items);
    }

    fn _try_parse_type_expr(&mut self) -> Option<TypeExpr> {
        if self.is_next_token(Token::LeftParen) {
            let mut items = Vec::new();

            if self.is_next_token(Token::RightParen) {
                return Some(TypeExpr::Tuple(items));
            };

            let Some(item) = self._try_parse_type_expr() else {
                return None;
            };
            items.push(item);

            while !self.is_next_token(Token::RightParen) {
                if !self.is_next_token(Token::Comma) {
                    return None;
                }

                let Some(item) = self._try_parse_type_expr() else {
                    return None;
                };
                items.push(item);
            }

            return Some(TypeExpr::Tuple(items));
        } else {
            let str = self.token_stream.next().unwrap();
            let Token::String(str) = str else {
                return None;
            };

            if let Some(type_args) = self.try_parse_type_args() {
                return Some(TypeExpr::WithArgs(str, type_args));
            } else {
                return Some(TypeExpr::Name(str));
            };
        }
    }

    fn try_parse_namespace(&mut self, name: String) -> (NamespaceChain, String) {
        let mut items = vec![name];

        while self.is_next_token(Token::Namespace) {
            items.push(self.get_string_token());
        }

        let last = items.pop().unwrap();

        (NamespaceChain(items), last)
    }

    fn try_parse_fn_call(
        &mut self,
        fn_name: String,
        namespace: NamespaceChain,
    ) -> Option<ValueExpr> {
        let type_args = self.try_parse_type_args();

        if (self.token_stream.peek() != Some(Token::LeftParen)) && type_args.is_none() {
            return None;
        }

        self.assert_next_token(Token::LeftParen);
        let args = self.parse_comma_value_exprs(Token::RightParen);

        Some(ValueExpr::FnCall {
            namespace,
            name: fn_name,
            type_args,
            args,
        })
    }

    fn try_parse_macro(&mut self, name: String, namespace: NamespaceChain) -> Option<ValueExpr> {
        if !self.is_next_token(Token::Exclamationmark) {
            return None;
        }

        let args = if self.is_next_token(Token::LeftParen) {
            self.parse_comma_value_exprs(Token::RightParen)
        } else if self.is_next_token(Token::LeftBracket) {
            self.parse_comma_value_exprs(Token::LeftBracket)
        } else {
            return None;
        };

        Some(ValueExpr::MacroCall {
            namespace,
            name,
            args,
        })
    }

    fn try_parse_infix_value_exprs(&mut self, first: ValueExpr) -> Option<ValueExpr> {
        let Some(infix_token) = self.token_stream.peek() else {
            return None;
        };
        match infix_token {
            Token::Plus | Token::Minus => 2,
            Token::Asterisk | Token::Slash | Token::Percent => 3,
            Token::BoolAnd | Token::BoolOr => 0,
            Token::BoolEqual
            | Token::BoolNotEqual
            | Token::LeftAngleBracket
            | Token::RightAngleBracket => 1,
            Token::String(_) => 4,
            _ => return None,
        };

        #[allow(deprecated)]
        Some(self._try_parse_infix_value_exprs_recursive(first, 0))
    }

    #[deprecated(note = "This function should not be called directly")]
    fn _try_parse_infix_value_exprs_recursive(
        &mut self,
        left_val: ValueExpr,
        min_priority: usize,
    ) -> ValueExpr {
        let mut result = left_val.clone();

        loop {
            let Some(infix_token) = self.token_stream.peek() else {
                break;
            };
            let priority = match infix_token {
                Token::Plus | Token::Minus => 2,
                Token::Asterisk | Token::Slash | Token::Percent => 3,
                Token::BoolAnd | Token::BoolOr => 0,
                Token::BoolEqual
                | Token::BoolNotEqual
                | Token::LeftAngleBracket
                | Token::RightAngleBracket => 1,
                Token::String(_) => 4,
                _ => break,
            };

            if priority < min_priority {
                break;
            }

            self.token_stream.next();

            if let Token::String(string) = infix_token {
                if string == "as" {
                    result = ValueExpr::As {
                        value: Box::new(left_val.clone()),
                        type_: Box::new(self.parse_type_expr()),
                    };

                    continue;
                } else {
                    panic!("Expected some infix value expression. Found '{string}'.");
                }
            };

            let right_val = self.parse_single_value_expr();

            let right_val =
                self._try_parse_infix_value_exprs_recursive(right_val.clone(), priority);

            result = match infix_token {
                Token::Plus => ValueExpr::Add(Box::new(result.clone()), Box::new(right_val)),
                Token::Minus => ValueExpr::Sub(Box::new(result.clone()), Box::new(right_val)),
                Token::Asterisk => ValueExpr::Mul(Box::new(result.clone()), Box::new(right_val)),
                Token::Slash => ValueExpr::Div(Box::new(result.clone()), Box::new(right_val)),
                Token::Percent => {
                    ValueExpr::Remainder(Box::new(result.clone()), Box::new(right_val))
                }
                Token::BoolAnd => {
                    ValueExpr::BoolAnd(Box::new(result.clone()), Box::new(right_val.clone()))
                }
                Token::BoolOr => {
                    ValueExpr::BoolOr(Box::new(result.clone()), Box::new(right_val.clone()))
                }
                Token::BoolEqual => {
                    ValueExpr::BoolEqual(Box::new(result.clone()), Box::new(right_val.clone()))
                }
                Token::BoolNotEqual => {
                    ValueExpr::BoolNotEqual(Box::new(result.clone()), Box::new(right_val.clone()))
                }
                Token::LeftAngleBracket => {
                    ValueExpr::GreaterThan(Box::new(result.clone()), Box::new(right_val.clone()))
                }
                Token::RightAngleBracket => {
                    ValueExpr::LessThan(Box::new(result.clone()), Box::new(right_val.clone()))
                }
                _ => panic!("This compiler sucks."),
            };
        }

        result
    }

    fn try_parse_field_or_method(&mut self, first: ValueExpr) -> Option<ValueExpr> {
        if !self.is_next_token(Token::Period) {
            return None;
        }

        let token = self.get_string_token();

        let expr = if let Some(ValueExpr::FnCall {
            name,
            type_args,
            args,
            ..
        }) = self.try_parse_fn_call(token.clone(), NamespaceChain::new())
        {
            ValueExpr::MethodCall {
                object: Box::new(first),
                method: name,
                type_args,
                args,
            }
        } else {
            ValueExpr::ObjectField {
                objcet: Box::new(first),
                field: self.get_string_token(),
            }
        };

        Some(expr)
    }

    fn try_parse_indexing(&mut self, value: ValueExpr) -> Option<ValueExpr> {
        if !self.is_next_token(Token::LeftBracket) {
            return None;
        }

        let result = ValueExpr::Indexing {
            value: Box::new(value),
            index: Box::new(self.parse_value_exprs()),
        };

        self.assert_next_token(Token::RightBracket);

        Some(result)
    }

    fn try_parse_infix_expr(&mut self, variable: ValueExpr) -> Option<Expr> {
        Some(match self.token_stream.peek().unwrap() {
            Token::Equal => {
                self.token_stream.next();
                let value = self.parse_value_exprs();

                Expr::EqualAssign { variable, value }
            }
            Token::AddAssign => {
                self.token_stream.next();
                let value = self.parse_value_exprs();

                Expr::AddAssign { variable, value }
            }
            Token::SubAssign => {
                self.token_stream.next();
                let value = self.parse_value_exprs();

                Expr::SubAssign { variable, value }
            }
            Token::MulAssign => {
                self.token_stream.next();
                let value = self.parse_value_exprs();

                Expr::MulAssign { variable, value }
            }
            Token::DivAssign => {
                self.token_stream.next();
                let value = self.parse_value_exprs();

                Expr::DivAssign { variable, value }
            }
            _ => {
                return None;
            }
        })
    }

    fn try_parse_range_expr(&mut self, start: ValueExpr) -> Option<ValueExpr> {
        if self.is_next_token(Token::Range) {
            let end = self.parse_single_value_expr();

            return Some(ValueExpr::Range {
                start: Box::new(start),
                end: Box::new(end),
                is_inclusive: false,
            });
        } else if self.is_next_token(Token::InclusiveRange) {
            let end = self.parse_single_value_expr();

            return Some(ValueExpr::Range {
                start: Box::new(start),
                end: Box::new(end),
                is_inclusive: true,
            });
        }

        None
    }

    // # HIGH LEVEL METHODS #
    // 사이드이펙트가 최소이며, 언제든 호출해도 기능이 보장되는 매서드들.

    fn parse_type_expr(&mut self) -> TypeExpr {
        if self.is_next_token(Token::LeftParen) {
            let mut items = Vec::new();

            if self.is_next_token(Token::RightParen) {
                return TypeExpr::Tuple(items);
            };

            items.push(self.parse_type_expr());

            while !self.is_next_token(Token::RightParen) {
                self.assert_next_token(Token::Comma);

                items.push(self.parse_type_expr());
            }

            return TypeExpr::Tuple(items);
        } else {
            let str = self.get_string_token();

            if let Some(type_args) = self.try_parse_type_args() {
                return TypeExpr::WithArgs(str, type_args);
            } else {
                return TypeExpr::Name(str);
            };
        }
    }

    fn parse_variable_define_expr(&mut self) -> VariableDefineExpr {
        let curr_token = self
            .token_stream
            .next()
            .expect("Expected variable define expression. But file is ended.");

        if let Token::String(mut str) = curr_token {
            let is_mut = str == "mut";

            if is_mut {
                str = self.get_string_token();
            };

            let type_ = if self.is_next_token(Token::Colon) {
                Some(self.parse_type_expr())
            } else {
                None
            };

            return VariableDefineExpr::One {
                name: str,
                type_,
                is_mut,
            };
        } else if curr_token == Token::LeftParen {
            let items = self.parse_comma_define_exprs(Token::RightParen);
            return VariableDefineExpr::TupleDestruct(items);
        } else {
            panic!("Expected variable define expression. Found '{curr_token}'.");
        }
    }

    fn parse_value_exprs(&mut self) -> ValueExpr {
        let single = self.parse_single_value_expr();

        self.try_parse_infix_value_exprs(single.clone())
            .unwrap_or(single)
    }

    fn parse_single_value_expr(&mut self) -> ValueExpr {
        let curr_token = self
            .token_stream
            .next()
            .expect("Expected value expression. But File is ended.");

        #[cfg(debug_assertions)]
        {
            eprintln!("value {}", curr_token);
        }

        let result = match curr_token {
            Token::DoubleQuote => {
                let str = self.token_stream.string_before_char(b'"');
                ValueExpr::StringLiteral(str)
            }
            Token::Quote => {
                let str = self.token_stream.string_before_char(b'\'');
                ValueExpr::StringLiteral(str)
            }
            Token::Intager(int) => {
                if self.is_next_token(Token::Period) {
                    let token = self
                        .token_stream
                        .next()
                        .expect("Expected decimal places of float literal.");

                    let Token::Intager(decimal_places) = token else {
                        panic!("Invalid decimal places of float literal.");
                    };

                    ValueExpr::FloatLiteral(
                        format!("{}.{}", int, decimal_places)
                            .parse::<f64>()
                            .expect("Invalid float literal."),
                    )
                } else {
                    ValueExpr::IntagerLiteral(int.parse::<i64>().expect("Invalid int literal."))
                }
            }
            Token::LeftParen => {
                let items = self.parse_comma_value_exprs(Token::RightParen);

                ValueExpr::Tuple(items)
            }
            Token::Asterisk => {
                let var = self.parse_single_value_expr();
                ValueExpr::Dereference(Box::new(var))
            }
            Token::Ampersand => {
                let is_mut = self.is_next_token(Token::String(s!("mut")));
                let var = self.parse_single_value_expr();

                ValueExpr::Reference {
                    value: Box::new(var),
                    is_mut,
                }
            }
            Token::String(str) => {
                let (namespace, str) = self.try_parse_namespace(str.clone());

                if let Some(macro_expr) = self.try_parse_macro(str.clone(), namespace.clone()) {
                    macro_expr
                } else if let Some(fn_expr) = self.try_parse_fn_call(str.clone(), namespace) {
                    fn_expr
                } else {
                    ValueExpr::Variable(str)
                }
            }
            other => {
                panic!("Unexpected expression '{other}'.");
            }
        };

        let mut prev = result;

        loop {
            if let Some(chain_expr) = self.try_parse_field_or_method(prev.clone()) {
                prev = chain_expr;
            } else if let Some(index_expr) = self.try_parse_indexing(prev.clone()) {
                prev = index_expr;
            } else if let Some(range_expr) = self.try_parse_range_expr(prev.clone()) {
                prev = range_expr;
            } else {
                return prev;
            }
        }
    }

    fn parse_codeline(&mut self) -> Expr {
        let curr_token = self
            .token_stream
            .peek()
            .expect("Expected code line. But file is ended.");

        #[cfg(debug_assertions)]
        {
            eprintln!("line {}", curr_token);
        }

        match curr_token {
            Token::String(str) => match str.as_str() {
                "if" => {
                    self.token_stream.next();
                    let condition = self.parse_value_exprs();
                    let if_body = self.parse_codeblock();

                    let else_body = self
                        .is_next_token(Token::String(s!("else")))
                        .then(|| self.parse_codeblock());

                    Expr::If {
                        condition,
                        if_body,
                        else_body,
                    }
                }
                "while" => {
                    self.token_stream.next();
                    let condition = self.parse_value_exprs();
                    let body = self.parse_codeblock();

                    Expr::While { condition, body }
                }
                "for" => {
                    self.token_stream.next();
                    let iter_item = self.parse_variable_define_expr();

                    self.assert_next_token(Token::String(s!("in")));

                    let iter = self.parse_single_value_expr();
                    let iter_body = self.parse_codeblock();

                    let remain_body = self
                        .is_next_token(Token::String(s!("remain")))
                        .then(|| self.parse_codeblock());

                    Expr::ForIn {
                        iter_item,
                        iter,
                        iter_body,
                        remain_body,
                    }
                }
                "let" => {
                    self.token_stream.next();
                    let define_expr = self.parse_variable_define_expr();

                    self.assert_next_token(Token::Equal);

                    let value = self.parse_value_exprs();
                    self.assert_next_token(Token::Semicolon);

                    Expr::VariableLet { define_expr, value }
                }
                "var" => {
                    self.token_stream.next();
                    let define_expr = self.parse_variable_define_expr();
                    self.assert_next_token(Token::Equal);

                    let value = self.parse_value_exprs();
                    self.assert_next_token(Token::Semicolon);

                    Expr::VariableVar { define_expr, value }
                }
                "fn" => {
                    self.token_stream.next();
                    let name = self.get_string_token();

                    let type_args = self.try_parse_type_args();

                    self.assert_next_token(Token::LeftParen);
                    let args = self.parse_comma_define_exprs(Token::RightParen);

                    let return_type = if self.is_next_token(Token::Colon) {
                        self.parse_type_expr()
                    } else {
                        TypeExpr::Tuple(Vec::new())
                    };

                    let body = self.parse_codeblock();

                    Expr::FnDefine {
                        name,
                        type_args,
                        return_type,
                        args,
                        body,
                    }
                }
                "return" => {
                    self.token_stream.next();
                    let value = self.parse_value_exprs();

                    self.assert_next_token(Token::Semicolon);

                    Expr::Return(value)
                }
                "use" => {
                    self.token_stream.next();

                    Expr::NamespaceUse(self.parse_namespace_tree())
                }
                _ => {
                    let var = self.parse_value_exprs();

                    let res = self
                        .try_parse_infix_expr(var.clone())
                        .unwrap_or(Expr::ValueExpr(var));

                    if self.is_next_token(Token::Semicolon) {
                        res
                    } else {
                        Expr::ReturnExpr(Box::new(res))
                    }
                }
            },
            Token::Semicolon => {
                self.token_stream.next();
                self.parse_codeline()
            }
            Token::Comment => {
                self.token_stream.next();
                self.token_stream.skip_line();
                self.parse_codeline()
            }
            Token::Asterisk => {
                self.token_stream.next();
                let var = ValueExpr::Dereference(Box::new(self.parse_single_value_expr()));

                let res = self
                    .try_parse_infix_expr(var.clone())
                    .unwrap_or(Expr::ValueExpr(var));

                self.assert_next_token(Token::Semicolon);

                res
            }
            other => {
                panic!("Unexpected expression '{other}'.");
            }
        }
    }

    fn parse_codeblock(&mut self) -> CodeBlock {
        self.assert_next_token(Token::LeftBrace);

        let mut lines = Vec::new();

        while !self.is_next_token(Token::RightBrace) {
            lines.push(self.parse_codeline());
        }

        CodeBlock(lines)
    }
}
