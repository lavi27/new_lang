// pub trait ExprVisitor {
//     fn visit_expr(&mut self, expr: &Expr);
//     fn visit_value_expr(&mut self, expr: &ValueExpr);
//     fn visit_var_def_expr(&mut self, expr: &VariableDefineExpr);
//     fn visit_codeblock(&mut self, expr: &CodeBlock);
//     fn visit_type_expr(&mut self, expr: &TypeExpr);
//     fn visit_namespace_tree(&mut self, expr: &NamespaceTree);
// }

use slotmap::new_key_type;

new_key_type! { pub struct ExprKey; }
new_key_type! { pub struct ValueExprKey; }
new_key_type! { pub struct CodeBlockKey; }

#[derive(Clone)]
pub enum Expr {
    EqualAssign {
        variable: ValueExprKey,
        value: ValueExprKey,
    },
    AddAssign {
        variable: ValueExprKey,
        value: ValueExprKey,
    },
    SubAssign {
        variable: ValueExprKey,
        value: ValueExprKey,
    },
    MulAssign {
        variable: ValueExprKey,
        value: ValueExprKey,
    },
    DivAssign {
        variable: ValueExprKey,
        value: ValueExprKey,
    },
    If {
        condition: ValueExprKey,
        if_body: CodeBlockKey,
        else_body: Option<CodeBlockKey>,
    },
    ForIn {
        iter_item: VariableDefineExpr,
        iter: ValueExprKey,
        iter_body: CodeBlockKey,
        remain_body: Option<CodeBlockKey>,
    },
    ParalForIn {
        iter_item: VariableDefineExpr,
        iter: ValueExprKey,
        iter_body: CodeBlockKey,
        remain_body: Option<CodeBlockKey>,
    },
    While {
        condition: ValueExprKey,
        body: CodeBlockKey,
    },
    VariableLet {
        define_expr: VariableDefineExpr,
        value: ValueExprKey,
    },
    VariableVar {
        define_expr: VariableDefineExpr,
        value: ValueExprKey,
    },
    FnDefine {
        name: String,
        type_args: Option<Vec<String>>,
        return_type: TypeExpr,
        args: Vec<VariableDefineExpr>,
        body: CodeBlockKey,
    },
    Return(ValueExprKey),
    ReturnExpr(ExprKey),
    ValueExpr(ValueExprKey),
    CodeBlock(CodeBlockKey),
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
    Add(ValueExprKey, ValueExprKey),
    Sub(ValueExprKey, ValueExprKey),
    Mul(ValueExprKey, ValueExprKey),
    Div(ValueExprKey, ValueExprKey),
    Remainder(ValueExprKey, ValueExprKey),
    LessThan(ValueExprKey, ValueExprKey),
    GreaterThan(ValueExprKey, ValueExprKey),
    Reference {
        value: ValueExprKey,
        is_mut: bool,
    },
    Dereference(ValueExprKey),
    BoolAnd(ValueExprKey, ValueExprKey),
    BoolOr(ValueExprKey, ValueExprKey),
    BoolEqual(ValueExprKey, ValueExprKey),
    BoolNotEqual(ValueExprKey, ValueExprKey),
    Tuple(Vec<ValueExprKey>),
    GroupingParen(ValueExprKey),
    Variable(String),
    Indexing {
        value: ValueExprKey,
        index: ValueExprKey,
    },
    As {
        value: ValueExprKey,
        type_: Box<TypeExpr>,
    },
    Range {
        start: ValueExprKey,
        end: ValueExprKey,
        is_inclusive: bool,
    },
    FnCall {
        namespace: NamespaceChain,
        name: String,
        type_args: Option<Vec<TypeExpr>>,
        args: Vec<ValueExprKey>,
    },
    ObjectField {
        objcet: ValueExprKey,
        field: String,
    },
    MethodCall {
        object: ValueExprKey,
        method: String,
        type_args: Option<Vec<TypeExpr>>,
        args: Vec<ValueExprKey>,
    },
    MacroCall {
        namespace: NamespaceChain,
        name: String,
        args: Vec<ValueExprKey>,
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
pub struct CodeBlock(pub Vec<ExprKey>);
