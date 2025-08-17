use crate::{
    s,
    token_stream::{Token, TokenStream},
};

#[derive(Clone)]
enum Node {
    EqualAssign {
        variable: String,
        value: ValueExpr,
    },
    If {
        condition: ValueExpr,
        if_body: CodeBlock,
        else_body: Option<CodeBlock>,
    },
    ParallelForIn {
        iter_item: VariableDefineExpr,
        iter: ValueExpr,
        iter_body: CodeBlock,
        remain_body: Option<CodeBlock>,
    },
    ForIn {
        iter_item: VariableDefineExpr,
        iter: ValueExpr,
        iter_body: CodeBlock,
        remain_body: Option<CodeBlock>,
    },
    VariableLet {
        define_expr: VariableDefineExpr,
        value: ValueExpr,
    },
    VariableVar {
        define_expr: VariableDefineExpr,
        value: ValueExpr,
    },
    Namespace {
        parent: String,
        child: String,
    },
    FnDefine {
        name: String,
        type_args: Option<Vec<TypeExpr>>,
        return_type: Option<TypeExpr>,
        args: Vec<VariableDefineExpr>,
        body: CodeBlock,
    },
    Return(ValueExpr),
    ValueExpr(ValueExpr),
    CodeBlock(CodeBlock),
    VariableDefineExpr(VariableDefineExpr),
    TypeExpr(TypeExpr),
}

#[derive(Clone)]
enum ValueExpr {
    IntagerLiteral(i64),
    FloatLiteral(f64),
    StringLiteral(String),
    Add(Box<ValueExpr>, Box<ValueExpr>),
    Sub(Box<ValueExpr>, Box<ValueExpr>),
    Mul(Box<ValueExpr>, Box<ValueExpr>),
    Div(Box<ValueExpr>, Box<ValueExpr>),
    LessThan(Box<ValueExpr>, Box<ValueExpr>),
    GreaterThan(Box<ValueExpr>, Box<ValueExpr>),
    Tuple(Vec<ValueExpr>),
    Variable(String),
    FnCall {
        name: String,
        type_args: Option<Vec<TypeExpr>>,
        args: Vec<ValueExpr>,
    },
    ObjectChain(Vec<ValueExpr>),
    ObjectField {
        objcet: Box<ValueExpr>,
        field: String,
    },
    MethodCall {
        object: Box<ValueExpr>,
        method: String,
        type_args: Option<Vec<TypeExpr>>,
        args: Vec<ValueExpr>,
    },
}

#[derive(Clone)]
enum VariableDefineExpr {
    Name(String),
    WithType(String, TypeExpr),
    TupleDestruct(Vec<VariableDefineExpr>),
}

#[derive(Clone)]
enum TypeExpr {
    Name(String),
    WithArgs(String, Vec<TypeExpr>),
}

#[derive(Clone)]
struct CodeBlock(Vec<Node>);

pub struct AbstractSyntaxTree {
    pub main_routine: CodeBlock,
}

pub struct ASTParser {
    token_stream: TokenStream,
}

impl ASTParser {
    pub fn from(str: String) -> Self {
        Self {
            token_stream: TokenStream::from(str),
        }
    }

    pub fn parse(&mut self) {}

    fn assert_next_token(&mut self, token: Token) {
        let Some(curr_token) = self.token_stream.next() else {
            panic!("Expected '{token}'. But file is ended.");
        };

        if curr_token != token {
            panic!("Expected '{token}'. Found '{curr_token}'.");
        }
    }

    fn get_string_token(&mut self) -> String {
        let Some(curr_token) = self.token_stream.next() else {
            panic!("Expected some name. But file is ended.");
        };

        let Token::String(str) = curr_token else {
            panic!("'{curr_token}' can't be in to some name.");
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

    fn parse_type_args(&mut self) -> Vec<TypeExpr> {
        let mut items = Vec::new();

        if !self.is_next_token(Token::LeftAngleBracket) {
            return items;
        }

        if self.is_next_token(Token::RightAngleBracket) {
            return items;
        }

        items.push(self.parse_type_expr());

        while !self.is_next_token(Token::RightAngleBracket) {
            self.assert_next_token(Token::Comma);

            items.push(self.parse_type_expr());
        }

        return items;
    }

    fn parse_type_expr(&mut self) -> TypeExpr {
        let str = self.get_string_token();

        if self.is_next_token(Token::LeftAngleBracket) {
            let type_args = self.parse_type_args();

            return TypeExpr::WithArgs(str, type_args);
        } else {
            return TypeExpr::Name(str);
        };
    }

    /// Validate is it fn call expression by current token stream. And parse the expression.
    fn try_parse_fn_call(&mut self, fn_name: String) -> Option<ValueExpr> {
        if self.is_next_token(Token::LeftParen) {
            let args = self.parse_comma_value_exprs(Token::RightParen);

            Some(ValueExpr::FnCall {
                name: fn_name,
                type_args: None,
                args,
            })
        } else if self.is_next_token(Token::LeftAngleBracket) {
            let type_args = self.parse_type_args();
            self.assert_next_token(Token::LeftParen);
            let args = self.parse_comma_value_exprs(Token::RightParen);

            Some(ValueExpr::FnCall {
                name: fn_name,
                type_args: Some(type_args),
                args,
            })
        } else {
            None
        }
    }

    fn parse_comma_value_exprs(&mut self, end_token: Token) -> Vec<ValueExpr> {
        let mut items = Vec::new();

        if self.is_next_token(end_token.clone()) {
            return items;
        }

        let aaa = self.parse_value_expr();
        items.push(aaa);

        while {
            let is_end = self.is_next_token(end_token.clone());
            !is_end
        } {
            self.assert_next_token(Token::Comma);
            items.push(self.parse_value_expr());
        }

        return items;
    }

    fn try_parse_comma_define_exprs(&mut self, end_token: Token) -> Vec<VariableDefineExpr> {
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

    fn get_infix_token_priority(&mut self, infix_token: Token) -> Option<usize> {
        Some(match infix_token {
            Token::Plus | Token::Minus => 1,
            Token::Asterisk | Token::Slash => 2,
            _ => return None,
        })
    }

    fn try_parse_infix_value_exprs(&mut self, first: ValueExpr) -> Option<ValueExpr> {
        self._try_parse_infix_value_exprs_recursive(first, 0)
    }

    fn _try_parse_infix_value_exprs_recursive(
        &mut self,
        left_val: ValueExpr,
        min_priority: usize,
    ) -> Option<ValueExpr> {
        let mut result = Some(left_val);

        loop {
            let Some(infix_token) = self.token_stream.next() else {
                break;
            };
            let Some(priority) = self.get_infix_token_priority(infix_token.clone()) else {
                break;
            };

            if priority < min_priority {
                break;
            }

            let expr = self.parse_value_expr();

            let Some(right_val) = self._try_parse_infix_value_exprs_recursive(expr, priority)
            else {
                panic!("Expected right value of infix expression.");
            };

            result = Some(match infix_token {
                Token::Plus => ValueExpr::Add(Box::new(left_val), Box::new(right_val)),
                Token::Minus => ValueExpr::Sub(Box::new(left_val), Box::new(right_val)),
                Token::Asterisk => ValueExpr::Mul(Box::new(left_val), Box::new(right_val)),
                Token::Slash => ValueExpr::Div(Box::new(left_val), Box::new(right_val)),
                _ => panic!("This compiler sucks."),
            });
        }

        result
    }

    fn parse_value_expr(&mut self) -> ValueExpr {
        let Some(curr_token) = self.token_stream.next() else {
            panic!("Expected value expression. But File is ended.");
        };

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
                    let Some(token) = self.token_stream.next() else {
                        panic!("Expected decimal places of float literal.");
                    };

                    let Token::Intager(decimal_places) = token else {
                        panic!();
                    };

                    ValueExpr::FloatLiteral(
                        format!("{}.{}", int, decimal_places)
                            .parse::<f64>()
                            .expect("msg"),
                    )
                } else {
                    ValueExpr::IntagerLiteral(int.parse::<i64>().expect("msg"))
                }
            }
            Token::LeftParen => {
                let items = self.parse_comma_value_exprs(Token::RightParen);

                ValueExpr::Tuple(items)
            }
            Token::String(str) => {
                if let Some(fn_expr) = self.try_parse_fn_call(str.clone()) {
                    fn_expr
                } else if let Some(chain_expr) =
                    self.try_parse_object_chain(ValueExpr::Variable(str.clone()))
                {
                    chain_expr
                } else {
                    ValueExpr::Variable(str)
                }
            }
            other => {
                todo!();
            }
        };

        if let Some(chain_expr) = self.try_parse_object_chain(result.clone()) {
            return chain_expr;
        } else if let Some(infix_expr) = self.try_parse_infix_value_exprs(result.clone()) {
            return infix_expr;
        } else {
            return result;
        }
    }

    fn parse_variable_define_expr(&mut self) -> VariableDefineExpr {
        let Some(curr_token) = self.token_stream.next() else {
            panic!("Expected variable define expression. But file is ended.");
        };

        if let Token::String(str) = curr_token {
            if self.is_next_token(Token::Colon) {
                let type_expr = self.parse_type_expr();
                return VariableDefineExpr::WithType(str, type_expr);
            } else {
                return VariableDefineExpr::Name(str);
            }
        } else if curr_token == Token::LeftParen {
            let items = self.try_parse_comma_define_exprs(Token::RightParen);
            return VariableDefineExpr::TupleDestruct(items);
        } else {
            panic!("Expected variable define expression. Found '{curr_token}'.");
        }
    }

    fn try_parse_object_chain(&mut self, first: ValueExpr) -> Option<ValueExpr> {
        let mut chain = Vec::new();
        let mut last_expr = first;

        while self.is_next_token(Token::Period) {
            let token = self.get_string_token();

            let expr = if let Some(ValueExpr::FnCall {
                name,
                type_args,
                args,
            }) = self.try_parse_fn_call(token.clone())
            {
                ValueExpr::MethodCall {
                    object: Box::new(last_expr.clone()),
                    method: name,
                    type_args,
                    args,
                }
            } else {
                ValueExpr::Variable(token)
            };

            chain.push(expr.clone());
            last_expr = expr;
        }

        Some(ValueExpr::ObjectChain(chain))
    }

    fn parse_codeline(&mut self) -> Node {
        let Some(curr_token) = self.token_stream.next() else {
            panic!("Expected code line. But file is ended.");
        };

        match curr_token {
            Token::String(str) => match str.as_str() {
                "if" => {
                    let condition = self.parse_value_expr();
                    let if_body = self.parse_codeblock();

                    let else_body = if self.is_next_token(Token::String(s!("else"))) {
                        Some(self.parse_codeblock())
                    } else {
                        None
                    };

                    self.assert_next_token(Token::Semicolon);

                    Node::If {
                        condition,
                        if_body,
                        else_body,
                    }
                }
                "for" => {
                    let iter_item = self.parse_variable_define_expr();

                    self.assert_next_token(Token::String(s!("in")));

                    let iter = self.parse_value_expr();
                    let iter_body = self.parse_codeblock();

                    let remain_body = if self.is_next_token(Token::String(s!("remain"))) {
                        Some(self.parse_codeblock())
                    } else {
                        None
                    };

                    self.assert_next_token(Token::Semicolon);

                    Node::ForIn {
                        iter_item,
                        iter,
                        iter_body,
                        remain_body,
                    }
                }
                "let" => {
                    let define_expr = self.parse_variable_define_expr();
                    self.assert_next_token(Token::Equal);

                    let value = self.parse_value_expr();
                    self.assert_next_token(Token::Semicolon);

                    Node::VariableLet { define_expr, value }
                }
                "var" => {
                    let define_expr = self.parse_variable_define_expr();
                    self.assert_next_token(Token::Equal);

                    let value = self.parse_value_expr();
                    self.assert_next_token(Token::Semicolon);

                    Node::VariableVar { define_expr, value }
                }
                "fn" => {
                    let name = self.get_string_token();

                    self.assert_next_token(Token::LeftParen);
                    let args = self.try_parse_comma_define_exprs(Token::RightParen);

                    let type_args = self.parse_type_args();
                    let return_type = if self.is_next_token(Token::Colon) {
                        Some(self.parse_type_expr())
                    } else {
                        None
                    };

                    let body = self.parse_codeblock();

                    Node::FnDefine {
                        name,
                        type_args: Some(type_args),
                        return_type,
                        args,
                        body,
                    }
                }
                "return" => {
                    let value = self.parse_value_expr();

                    Node::Return(value)
                }
                thing => {
                    if let Some(chain_expr) =
                        self.try_parse_object_chain(ValueExpr::Variable(thing.to_string()))
                    {
                        Node::ValueExpr(chain_expr)
                    } else {
                        todo!()
                    }
                }
            },
            other => {
                todo!()
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
