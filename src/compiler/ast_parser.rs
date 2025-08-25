use std::{
    mem::take,
    ops::Range,
    panic::{self, AssertUnwindSafe},
};

use crate::{
    compiler::{code_generater::{for_in_to_rust, parallel_for_in_to_rust}, token_stream::{Token, TokenStream}},
    s,
};

fn expr_vec_to_rust(vec: &Vec<impl ToRust>, sep: &str) -> String {
    vec.iter()
        .map(|i| i.to_rust())
        .collect::<Vec<String>>()
        .join(sep)
}

#[derive(Clone)]
pub enum Expr {
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

impl ToRust for Expr {
    fn to_rust(&self) -> String {
        match self {
            Self::CodeBlock(expr) => expr.to_rust() + ";",
            Self::ValueExpr(expr) => expr.to_rust() + ";",
            Self::VariableDefineExpr(expr) => expr.to_rust() + ";",
            Self::TypeExpr(expr) => expr.to_rust() + ";",
            Self::NamespaceChain(expr) => expr.to_rust() + ";",
            Self::EqualAssign { variable, value } => {
                format!("{} = {};", variable.to_rust(), value.to_rust())
            }
            Self::If {
                condition,
                if_body,
                else_body,
            } => {
                if let Some(else_body) = else_body {
                    format!(
                        "if {} {} else {};",
                        condition.to_rust(),
                        if_body.to_rust(),
                        else_body.to_rust()
                    )
                } else {
                    format!("if {} {};", condition.to_rust(), if_body.to_rust())
                }
            }
            Self::ForIn {
                iter_item,
                iter,
                iter_body,
                remain_body,
            } => {
                for_in_to_rust(iter_item, iter, iter_body, remain_body)
            }
            Self::ParallelForIn {
                iter_item,
                iter,
                iter_body,
                remain_body,
            } => {
                parallel_for_in_to_rust(iter_item, iter, iter_body, remain_body)
            }
            Self::VariableLet { define_expr, value } => {
                format!("let {} = {};", define_expr.to_rust(), value.to_rust())
            }
            Self::VariableVar { define_expr, value } => {
                format!("let mut {} = {};", define_expr.to_rust(), value.to_rust())
            }
            Self::FnDefine {
                name,
                type_args,
                args,
                return_type,
                body,
            } => {
                if let Some(type_args) = type_args {
                    format!(
                        "fn {}<{}>({}) -> {} {}",
                        name,
                        expr_vec_to_rust(type_args, ", "),
                        expr_vec_to_rust(args, ", "),
                        return_type.to_rust(),
                        body.to_rust()
                    )
                } else {
                    format!(
                        "fn {}({}) -> {} {}",
                        name,
                        expr_vec_to_rust(args, ", "),
                        return_type.to_rust(),
                        body.to_rust()
                    )
                }
            }
            Self::Return(expr) => format!("return {};", expr.to_rust()),
        }
    }
}

#[derive(Clone)]
pub enum ValueExpr {
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
    MacroCall {
        namespace: NamespaceChain,
        name: String,
        args: Vec<ValueExpr>,
    },
}

impl ToRust for ValueExpr {
    fn to_rust(&self) -> String {
        match self {
            Self::IntagerLiteral(int) => int.to_string(),
            Self::FloatLiteral(float) => float.to_string(),
            Self::StringLiteral(str) => format!("\"{}\"", str.to_owned()),
            Self::Add(lvel, rvel) => format!("{}+{}", lvel.to_rust(), rvel.to_rust()),
            Self::Sub(lvel, rvel) => format!("{}-{}", lvel.to_rust(), rvel.to_rust()),
            Self::Mul(lvel, rvel) => format!("{}*{}", lvel.to_rust(), rvel.to_rust()),
            Self::Div(lvel, rvel) => format!("{}/{}", lvel.to_rust(), rvel.to_rust()),
            Self::LessThan(lvel, rvel) => format!("{}<{}", lvel.to_rust(), rvel.to_rust()),
            Self::GreaterThan(lvel, rvel) => format!("{}>{}", lvel.to_rust(), rvel.to_rust()),
            Self::Tuple(exprs) => format!("({})", expr_vec_to_rust(exprs, ", ")),
            Self::Variable(var) => var.to_string(),
            Self::FnCall {
                namespace,
                name,
                type_args,
                args,
            } => {
                let mut namespace_prefix = namespace.to_rust();

                if !namespace_prefix.is_empty() {
                    namespace_prefix += "::";
                }

                if let Some(type_args) = type_args {
                    format!(
                        "{}{}<{}>({})",
                        namespace_prefix,
                        name,
                        expr_vec_to_rust(type_args, ", "),
                        expr_vec_to_rust(args, ", "),
                    )
                } else {
                    format!(
                        "{}{}({})",
                        namespace_prefix,
                        name,
                        expr_vec_to_rust(args, ", "),
                    )
                }
            }
            Self::ObjectChain(value_exprs) => expr_vec_to_rust(value_exprs, "."),
            Self::ObjectField { objcet, field } => {
                format!("{}.{}", objcet.to_rust(), field.to_owned())
            }

            Self::MethodCall {
                object,
                method,
                type_args,
                args,
            } => {
                if let Some(type_args) = type_args {
                    format!(
                        "{}.{}<{}>({})",
                        object.to_rust(),
                        method,
                        expr_vec_to_rust(type_args, ", "),
                        expr_vec_to_rust(args, ", ")
                    )
                } else {
                    format!(
                        "{}.{}({})",
                        object.to_rust(),
                        method,
                        expr_vec_to_rust(args, ", ")
                    )
                }
            }
            Self::MacroCall {
                namespace,
                name,
                args,
            } => {
                format!(
                    "{}{}!({})",
                    namespace.to_rust(),
                    name,
                    expr_vec_to_rust(args, ", ")
                )
            }
        }
    }
}

#[derive(Clone)]
pub enum VariableDefineExpr {
    Name(String),
    WithType(String, TypeExpr),
    TupleDestruct(Vec<VariableDefineExpr>),
}

impl ToRust for VariableDefineExpr {
    fn to_rust(&self) -> String {
        match self {
            Self::Name(name) => name.to_owned(),
            Self::WithType(name, type_) => format!("{name}: {}", type_.to_rust()),
            Self::TupleDestruct(items) => expr_vec_to_rust(items, ", "),
        }
    }
}

#[derive(Clone)]
pub enum TypeExpr {
    Name(String),
    Tuple(Vec<TypeExpr>),
    WithArgs(String, Vec<TypeExpr>),
}

impl ToRust for TypeExpr {
    fn to_rust(&self) -> String {
        match self {
            Self::Name(name) => name.to_owned(),
            Self::Tuple(types) => {
                format!("({})", types
                .iter()
                .map(|i| i.to_rust())
                .collect::<Vec<String>>()
                .join(", "))
            }
            Self::WithArgs(name, args) => {
                format!("{name}<{}>", expr_vec_to_rust(args, ", "))
            }
        }
    }
}

#[derive(Clone)]
pub struct NamespaceChain(Vec<String>);

impl NamespaceChain {
    fn new() -> Self {
        Self(Vec::new())
    }
}

impl ToRust for NamespaceChain {
    fn to_rust(&self) -> String {
        self.0
            .iter()
            .map(|i| i.clone())
            .collect::<Vec<String>>()
            .join("::")
    }
}

#[derive(Clone)]
pub struct CodeBlock(Vec<Expr>);

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
    pub main_routine: Vec<Expr>,
    pub is_threading_used: bool,
}

impl Default for AbstractSyntaxTree {
    fn default() -> Self {
        Self {
            main_routine: Vec::new(),
            is_threading_used: false,
        }
    }
}

impl ToRust for AbstractSyntaxTree {
    fn to_rust(&self) -> String {
        expr_vec_to_rust(&self.main_routine, "\n")
    }
}

pub struct ASTParser {
    token_stream: TokenStream,
    filename: String,
    result: AbstractSyntaxTree,
}

impl ASTParser {
    pub fn parse_static(
        filename: String,
        input_code: String,
    ) -> Result<AbstractSyntaxTree, String> {
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
        // panic::set_hook(Box::new(|_| {}));

        while self.token_stream.peek().is_some() {
            let line_res = panic::catch_unwind(AssertUnwindSafe(|| self.parse_codeline()));
            let Ok(line) = line_res else {
                let (col, Range { start, end }) = self.token_stream.get_curr_position();
                let err_msg = unsafe { line_res.unwrap_err_unchecked() };

                let Ok(err_msg) = err_msg.downcast::<String>() else {
                    panic!("Hell nah.");
                };

                panic::take_hook();
                return Err(format!(
                    "Syntax Error (at {}:{col}:{start}): {}",
                    self.filename, err_msg
                ));
            };

            self.result.main_routine.push(line);
        }

        panic::take_hook();
        return Ok(take(&mut self.result));
    }

    // # LOW LEVEL METHODS #
    // self.token_stream을 직접 조작하는 저수준 매서드.

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

        let aaa = self.parse_value_expr();
        items.push(aaa);

        while !self.is_next_token(end_token.clone()) {
            self.assert_next_token(Token::Comma);
            items.push(self.parse_value_expr());
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

        if !self.is_next_token(end_token.clone()) {
            return items;
        }

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

    // # TRY- METHODS #
    // 조건 검증과 처리를 동시에 수행하는 매서드들, 여러번 호출해도 무방한 것들. 인자 외에는 호출시 조건이 필요 없음.
    // if let Some(_) = ... 구조로 사용할 것.

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
        let type_args = self
            .is_next_token(Token::LeftAngleBracket)
            .then(|| self.parse_comma_type_exprs(Token::RightAngleBracket));

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
        #[allow(deprecated)]
        self._try_parse_infix_value_exprs_recursive(first, 0)
    }

    #[deprecated(note = "This function should not be called directly")]
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
            let priority = match infix_token {
                Token::Plus | Token::Minus => 1,
                Token::Asterisk | Token::Slash => 2,
                _ => break,
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
                Token::Plus => ValueExpr::Add(Box::new(left_val.clone()), Box::new(right_val)),
                Token::Minus => ValueExpr::Sub(Box::new(left_val.clone()), Box::new(right_val)),
                Token::Asterisk => ValueExpr::Mul(Box::new(left_val.clone()), Box::new(right_val)),
                Token::Slash => ValueExpr::Div(Box::new(left_val.clone()), Box::new(right_val)),
                _ => panic!("This compiler sucks."),
            });
        }

        result
    }

    fn try_parse_object_chain(&mut self, first: ValueExpr) -> Option<ValueExpr> {
        let mut chain = vec![first.clone()];

        while self.is_next_token(Token::Period) {
            let token = self.get_string_token();

            let expr = if let Some(ValueExpr::FnCall {
                name,
                type_args,
                args,
                ..
            }) = self.try_parse_fn_call(token.clone(), NamespaceChain::new())
            {
                ValueExpr::FnCall {
                    namespace: NamespaceChain(Vec::new()),
                    name,
                    type_args,
                    args,
                }
            } else {
                ValueExpr::Variable(token)
            };

            chain.push(expr.clone());
        }

        if chain.len() > 1 {
            Some(ValueExpr::ObjectChain(chain))
        } else {
            None
        }
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

            if self.is_next_token(Token::LeftAngleBracket) {
                let type_args = self.parse_comma_type_exprs(Token::RightAngleBracket);

                return TypeExpr::WithArgs(str, type_args);
            } else {
                return TypeExpr::Name(str);
            };
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
            let items = self.parse_comma_define_exprs(Token::RightParen);
            return VariableDefineExpr::TupleDestruct(items);
        } else {
            panic!("Expected variable define expression. Found '{curr_token}'.");
        }
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
                let (namespace, str) = self.try_parse_namespace(str.clone());
                
                if let Some(macro_expr) = self.try_parse_macro(str.clone(), namespace.clone()) {
                    macro_expr
                } else if let Some(fn_expr) = self.try_parse_fn_call(str.clone(), namespace) {
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

        result
        // if let Some(chain_expr) = self.try_parse_object_chain(result.clone()) {
        //     return chain_expr;
        // } else if let Some(infix_expr) = self.try_parse_infix_value_exprs(result.clone()) {
        //     return infix_expr;
        // } else {
        //     return result;
        // }
    }

    fn parse_codeline(&mut self) -> Expr {
        let Some(curr_token) = self.token_stream.peek() else {
            panic!("Expected code line. But file is ended.");
        };

        match curr_token {
            Token::String(str) => match str.as_str() {
                "if" => {
                    self.token_stream.next();
                    let condition = self.parse_value_expr();
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
                "for" => {
                    self.token_stream.next();
                    let iter_item = self.parse_variable_define_expr();

                    self.assert_next_token(Token::String(s!("in")));

                    let iter = self.parse_value_expr();
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
                "parallelFor" => {
                    self.token_stream.next();
                    self.result.is_threading_used = true;
                    
                    let iter_item = self.parse_variable_define_expr();

                    self.assert_next_token(Token::String(s!("in")));

                    let iter = self.parse_value_expr();
                    let iter_body = self.parse_codeblock();

                    let remain_body = self
                        .is_next_token(Token::String(s!("remain")))
                        .then(|| self.parse_codeblock());

                    Expr::ParallelForIn {
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
                    
                    let value = self.parse_value_expr();
                    self.assert_next_token(Token::Semicolon);

                    Expr::VariableLet { define_expr, value }
                }
                "var" => {
                    self.token_stream.next();
                    let define_expr = self.parse_variable_define_expr();
                    self.assert_next_token(Token::Equal);

                    let value = self.parse_value_expr();
                    self.assert_next_token(Token::Semicolon);

                    Expr::VariableVar { define_expr, value }
                }
                "fn" => {
                    self.token_stream.next();
                    let name = self.get_string_token();

                    let type_args = self
                        .is_next_token(Token::LeftAngleBracket)
                        .then(|| self.parse_comma_type_exprs(Token::RightAngleBracket));

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
                    let value = self.parse_value_expr();

                    self.assert_next_token(Token::Semicolon);

                    Expr::Return(value)
                }
                _ => {
                    let result = Expr::ValueExpr(self.parse_value_expr());
                    self.assert_next_token(Token::Semicolon);

                    result
                }
            },
            Token::Semicolon => {
                self.token_stream.next();
                self.parse_codeline()
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
