use crate::{
    s,
    token_stream::{Token, TokenStream},
};

enum Node<'a> {
    EqualAssign {
        variable: String,
        value: ValueExpr<'a>,
    },
    If {
        condition: ValueExpr<'a>,
        if_body: CodeBlock<'a>,
        else_body: Option<CodeBlock<'a>>,
    },
    ParallelForIn {
        iter_item: VariableDefineExpr,
        iter: ValueExpr<'a>,
        iter_body: CodeBlock<'a>,
        remain_body: Option<CodeBlock<'a>>,
    },
    ForIn {
        iter_item: VariableDefineExpr,
        iter: ValueExpr<'a>,
        iter_body: CodeBlock<'a>,
        remain_body: Option<CodeBlock<'a>>,
    },
    VariableLet {
        define_expr: VariableDefineExpr,
        type_: Option<TypeExpr>,
        value: ValueExpr<'a>,
    },
    VariableVar {
        define_expr: VariableDefineExpr,
        type_: Option<TypeExpr>,
        value: ValueExpr<'a>,
    },
    Namespace {
        parent: String,
        child: String,
    },
    FnDefine {
        name: String,
        type_args: Vec<TypeExpr>,
        return_type: Option<TypeExpr>,
        args: Vec<VariableDefineWithType>,
        body: CodeBlock<'a>,
    },
    Return(ValueExpr<'a>),
    ValueExpr(ValueExpr<'a>),
    CodeBlock(CodeBlock<'a>),
    VariableDefineExpr(VariableDefineExpr),
    TypeExpr(TypeExpr),
}

enum ValueExpr<'a> {
    Add(&'a ValueExpr<'a>, &'a ValueExpr<'a>),
    Sub(&'a ValueExpr<'a>, &'a ValueExpr<'a>),
    Mul(&'a ValueExpr<'a>, &'a ValueExpr<'a>),
    Div(&'a ValueExpr<'a>, &'a ValueExpr<'a>),
    LessThan(&'a ValueExpr<'a>, &'a ValueExpr<'a>),
    GreaterThan(&'a ValueExpr<'a>, &'a ValueExpr<'a>),
    Tuple(Vec<&'a ValueExpr<'a>>),
    Variable(String),
    FnCall {
        name: String,
        type_args: Vec<String>,
        args: Vec<&'a ValueExpr<'a>>,
    },
    ObjectField {
        objcet: String,
        field: String,
    },
    MethodCall {
        object: String,
        method: String,
        type_args: Vec<String>,
        args: Vec<&'a ValueExpr<'a>>,
    },
}

enum VariableDefineExpr {
    Name(String),
    WithType(VariableDefineWithType),
    TupleDestruct(Vec<VariableDefineExpr>),
}

struct VariableDefineWithType(String, TypeExpr);

enum TypeExpr {
    Name(String),
    WithArgs(String, Vec<TypeExpr>),
}

struct CodeBlock<'a>(Vec<Node<'a>>);

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
        let is_next_token = if let Some(curr_token) = self.token_stream.next() {
            curr_token == token
        } else {
            false
        };

        if !is_next_token {
            panic!();
        }
    }

    fn is_next_token(&mut self, token: Token) -> bool {
        let curr_token = self.token_stream.peek();

        if let Some(curr_token) = curr_token {
            if curr_token == token {
                self.token_stream.next();
                true
            } else {
                false
            }
        } else {
            false
        }
    }

    fn parse_value_expr(&mut self) -> ValueExpr {}

    fn parse_variable_define_expr(&mut self) -> VariableDefineExpr {}

    fn parse_codeline(&mut self) -> Node {
        let curr_token = self.token_stream.next();

        let Some(curr_token) = curr_token else {
            panic!();
        };

        match curr_token {
            Token::String(str) => {
                let str = str.as_str();
                match str {
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
                }
            }
            thing => {
                todo!();
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
