use std::{mem::take, ops::Range, panic};

use crate::{
    s,
    compiler::token_stream::{Token, TokenStream},
};


trait ToRust {
    fn to_rust(&self) -> String;
}

#[derive(Clone)]
enum Node {
    EqualAssign {
        variable: ValueExpr,
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
    FnDefine {
        name: String,
        type_args: Option<Vec<TypeExpr>>,
        return_type: TypeExpr,
        args: Vec<VariableDefineExpr>,
        body: CodeBlock,
    },
    Return(ValueExpr),
    ValueExpr(ValueExpr),
    CodeBlock(CodeBlock),
    VariableDefineExpr(VariableDefineExpr),
    TypeExpr(TypeExpr),
    NamespaceChain(NamespaceChain),
}

impl ToRust for Node {
    fn to_rust(&self) -> String {
      match self {
        Node::CodeBlock(expr) => expr.to_rust() + ";",
        Node::ValueExpr(expr) => expr.to_rust() + ";",
        Node::VariableDefineExpr(expr) => expr.to_rust() + ";",
        Node::TypeExpr(expr) => expr.to_rust() + ";",
        Node::NamespaceChain(expr) => expr.to_rust() + ";",
        Node::EqualAssign {variable, value} => format!("{} = {};", variable.to_rust(), value.to_rust()),
        Node::If => {
            if let Some(else_body) = self.else_body {
                format!("if {self.condition.to_rust()} \{ {self.if_body.to_rust()} \} else \{ {else_body.to_rust()} \};")    
            } else {
                format!("if {self.condition.to_rust()} \{ {self.if_body.to_rust()} \};")
            }
        },
        Node::ForIn => format!("newlang_forin!(({self.iter_item.to_rust()}), ({self.iter.to_rust()}), {self.iter_body.to_rust()}, {self.remain_body.to_rust()});"),
        Node::ParallelForIn => format!("newlang_parallelforin!({self.iter_item.to_rust()}, {self.iter.to_rust()}, {self.iter_body.to_rust()}, {self.remain_body.to_rust()});"),
        Node::VariableLet => format!("let {self.define_expr.to_rust()} = {self.value.to_rust()};"),
        Node::VariableVar => format!("let mut {self.define_expr.to_rust()} = {self.value.to_rust()};"),
        Node::FnDefine => {
            if let Some(type_args) = self.type_args {
                format!("fn {self.name.to_rust()}<{type_args}>({self.return_type.to_rust()}) -> {self.return_type.to_rust()} \{ {self.body.to_rust()} \};")
            } else {
                format!("fn {self.name.to_rust()}({self.return_type.to_rust()}) -> {self.return_type.to_rust()} \{ {self.body.to_rust()} \};")
            }
        },
      }
    }
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
        namespace: NamespaceChain,
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

impl ToRust for ValueExpr {
    fn to_rust(&self) -> String {
        match self {
            ValueExpr::IntagerLiteral(int) => int.to_string(),
            ValueExpr::FloatLiteral(float) => float.to_string(),
            ValueExpr::StringLiteral(str) => str.to_owned(),
            ValueExpr::Add(lvel, rvel) => format!("{}+{}", lvel.to_rust(), rvel.to_rust()),
            ValueExpr::Sub(lvel, rvel) => format!("{lvel.to_rust()}-{rvel.to_rust()}"),
            ValueExpr::Mul(lvel, rvel) => format!("{lvel.to_rust()}*{rvel.to_rust()}"),
            ValueExpr::Div(lvel, rvel) => format!("{lvel.to_rust()}/{rvel.to_rust()}"),
            ValueExpr::LessThan(value_expr, value_expr1) => todo!(),
            ValueExpr::GreaterThan(value_expr, value_expr1) => todo!(),
            ValueExpr::Tuple(value_exprs) => todo!(),
            ValueExpr::Variable(_) => todo!(),
            ValueExpr::FnCall { namespace, name, type_args, args } => todo!(),
            ValueExpr::ObjectChain(value_exprs) => todo!(),
            ValueExpr::ObjectField { objcet, field } => todo!(),
            ValueExpr::MethodCall { object, method, type_args, args } => todo!(),
                    }
    }
}

#[derive(Clone)]
enum VariableDefineExpr {
    Name(String),
    WithType(String, TypeExpr),
    TupleDestruct(Vec<VariableDefineExpr>),
}

impl ToRust for VariableDefineExpr {
    fn to_rust(&self) -> String {
        match self {
            VariableDefineExpr::Name(name) => name.to_owned(),
            VariableDefineExpr::WithType(name, type_) => format!("{name}: {}", type_.to_rust()),
            VariableDefineExpr::TupleDestruct(items) => items.into_iter().map(|i| i.to_rust()).join(", "),
        }
    }
}

#[derive(Clone)]
enum TypeExpr {
    Name(String),
    WithArgs(String, Vec<TypeExpr>),
}

impl ToRust for TypeExpr {
    fn to_rust(&self) -> String {
        match self {
            TypeExpr::Name(name) => name.to_owned(),
            TypeExpr::WithArgs(name, args) => format!("{name}<{args.map(|i| i.to_rust()).join(", ")}>"),
        }
    }
}

#[derive(Clone)]
struct NamespaceChain(Vec<(String, String)>);

impl ToRust for NamespaceChain {
    fn to_rust(&self) -> String {
        let mut result = self.0[0].0;
        
        result += self.0.into_iter().map(|i| i.1).join("::");

        result
    }
}

#[derive(Clone)]
struct CodeBlock(Vec<Node>);

impl ToRust for CodeBlock {
    fn to_rust(&self) -> String {
        let mut result = s!("{\n");

        for line in self.0.iter() {
            result.push_str(&line.to_rust());
            result.push('\n');
        }

        result.push('}');
        result
    }
}

pub struct AbstractSyntaxTree {
    pub main_routine: CodeBlock,
    pub is_threading_used: bool,
}

impl Default for AbstractSyntaxTree {
    fn default() -> Self {
        Self {
            main_routine: ,
            is_threading_used: false,
        }
    }
}

pub struct ASTParser {
    token_stream: TokenStream,
    filename: String,
    result: AbstractSyntaxTree,
}

impl ASTParser {
    pub fn parse_static(filename: String, input_code: String) -> Result<AbstractSyntaxTree, String> {
        let mut parser = Self::new(filename, input_code);
        parser.parse()
    }

    pub fn new(filename: String, input_code: String) -> Self {
        Self {
            token_stream: TokenStream::from(input_code),
            filename,
            result: AbstractSyntaxTree::default(),
        }
    }

    pub fn parse(&mut self) -> Result<AbstractSyntaxTree, String> {
        while self.token_stream.peek().is_some() {
            let line_res = panic::catch_unwind(|| self.parse_codeline());
            let Ok(line) = line_res else {
                let (col, Range {start, end}) = self.token_stream.get_curr_position();

                return format!("Syntax Error (at {}:{col}:{start}): {}", self.filename, line_res.unwrap_err());
            };

            self.result.main_routine.0.push(line);
        }

        return Ok(take(&mut self.result));
    }

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
            panic!("'{curr_token}' can't be included to some name.");
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
                namespace: 
                name: fn_name,
                type_args: None,
                args,
            })
        } else if self.is_next_token(Token::LeftAngleBracket) {
            let type_args = self.parse_type_args();
            self.assert_next_token(Token::LeftParen);
            let args = self.parse_comma_value_exprs(Token::RightParen);

            Some(ValueExpr::FnCall {
                namespace
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
        let mut result = Some(left_val.clone());

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
                panic!("Unexpected expression '{other}'.");
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
                namespace
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
                "parallelFor" => {
                    self.result.is_threading_used = true;
                    todo!()
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
                    self.assert_next_token(Token::Colon);
                    let return_type = self.parse_type_expr();

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
