// pub trait ExprVisitor {
//     fn visit_expr(&mut self, expr: &Expr);
//     fn visit_value_expr(&mut self, expr: &ValueExpr);
//     fn visit_var_def_expr(&mut self, expr: &VariableDefineExpr);
//     fn visit_codeblock(&mut self, expr: &CodeBlock);
//     fn visit_type_expr(&mut self, expr: &TypeExpr);
//     fn visit_namespace_tree(&mut self, expr: &NamespaceTree);
// }

use crate::compiler::{codegen::ToRust, parser::SyntaxTree};

#[derive(Clone, Copy)]
pub struct ExprId(pub usize);



#[derive(Clone, Copy)]
pub struct ValueExprId(pub usize);



#[derive(Clone, Copy)]
pub struct CodeBlockId(pub usize);



#[derive(Clone)]
pub enum Expr {
    EqualAssign {
        variable: ValueExprId,
        value: ValueExprId,
    },
    AddAssign {
        variable: ValueExprId,
        value: ValueExprId,
    },
    SubAssign {
        variable: ValueExprId,
        value: ValueExprId,
    },
    MulAssign {
        variable: ValueExprId,
        value: ValueExprId,
    },
    DivAssign {
        variable: ValueExprId,
        value: ValueExprId,
    },
    If {
        condition: ValueExprId,
        if_body: CodeBlockId,
        else_body: Option<CodeBlockId>,
    },
    ForIn {
        iter_item: VariableDefineExpr,
        iter: ValueExprId,
        iter_body: CodeBlockId,
        remain_body: Option<CodeBlockId>,
    },
    ParalForIn {
        iter_item: VariableDefineExpr,
        iter: ValueExprId,
        iter_body: CodeBlockId,
        remain_body: Option<CodeBlockId>,
    },
    While {
        condition: ValueExprId,
        body: CodeBlockId,
    },
    VariableLet {
        define_expr: VariableDefineExpr,
        value: ValueExprId,
    },
    VariableVar {
        define_expr: VariableDefineExpr,
        value: ValueExprId,
    },
    FnDefine {
        name: String,
        type_args: Option<Vec<String>>,
        return_type: TypeExpr,
        args: Vec<VariableDefineExpr>,
        body: CodeBlockId,
    },
    Return(ValueExprId),
    ReturnExpr(ExprId),
    ValueExpr(ValueExprId),
    CodeBlock(CodeBlockId),
    VariableDefineExpr(VariableDefineExpr),
    TypeExpr(TypeExpr),
    NamespaceChain(NamespaceChain),
    NamespaceTree(NamespaceTree),
    NamespaceUse(NamespaceTree),
}

#[derive(Clone)]
pub enum ValueExpr {
    IntagerLiteral(i64),
    FloatLiteral(f64),
    StringLiteral(String),
    Add(ValueExprId, ValueExprId),
    Sub(ValueExprId, ValueExprId),
    Mul(ValueExprId, ValueExprId),
    Div(ValueExprId, ValueExprId),
    Remainder(ValueExprId, ValueExprId),
    LessThan(ValueExprId, ValueExprId),
    GreaterThan(ValueExprId, ValueExprId),
    Reference {
        value: ValueExprId,
        is_mut: bool,
    },
    Dereference(ValueExprId),
    BoolAnd(ValueExprId, ValueExprId),
    BoolOr(ValueExprId, ValueExprId),
    BoolEqual(ValueExprId, ValueExprId),
    BoolNotEqual(ValueExprId, ValueExprId),
    Tuple(Vec<ValueExprId>),
    Variable(String),
    Indexing {
        value: ValueExprId,
        index: ValueExprId,
    },
    As {
        value: ValueExprId,
        type_: Box<TypeExpr>,
    },
    Range {
        start: ValueExprId,
        end: ValueExprId,
        is_inclusive: bool,
    },
    FnCall {
        namespace: NamespaceChain,
        name: String,
        type_args: Option<Vec<TypeExpr>>,
        args: Vec<ValueExprId>,
    },
    ObjectField {
        objcet: ValueExprId,
        field: String,
    },
    MethodCall {
        object: ValueExprId,
        method: String,
        type_args: Option<Vec<TypeExpr>>,
        args: Vec<ValueExprId>,
    },
    MacroCall {
        namespace: NamespaceChain,
        name: String,
        args: Vec<ValueExprId>,
    },
}

#[derive(Clone)]
pub enum VariableDefineExpr {
    One {
        is_mut: bool,
        name: String,
        type_: Option<TypeExpr>,
    },
    TupleDestruct(Vec<VariableDefineExpr>),
}

#[derive(Clone, PartialEq)]
pub enum TypeExpr {
    Name(String),
    Tuple(Vec<TypeExpr>),
    WithArgs(String, Vec<TypeExpr>),
}

#[derive(Clone)]
pub struct NamespaceTree(pub Vec<String>, pub Option<Vec<NamespaceTree>>);

#[derive(Clone)]
pub struct NamespaceChain(pub Vec<String>);

impl NamespaceChain {
    pub fn new() -> Self {
        Self(Vec::new())
    }
}

#[derive(Clone)]
pub struct CodeBlock(pub Vec<ExprId>);
