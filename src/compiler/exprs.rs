pub trait ExprVisitor {
    fn visit_expr(&mut self, expr: &Expr);
    fn visit_value_expr(&mut self, expr: &ValueExpr);
    fn visit_var_def_expr(&mut self, expr: &VariableDefineExpr);
    fn visit_codeblock(&mut self, expr: &CodeBlock);
    fn visit_type_expr(&mut self, expr: &TypeExpr);
    fn visit_namespace_tree(&mut self, expr: &NamespaceTree);
}

#[derive(Clone)]
pub enum Expr {
    EqualAssign {
        variable: ValueExpr,
        value: ValueExpr,
    },
    AddAssign {
        variable: ValueExpr,
        value: ValueExpr,
    },
    SubAssign {
        variable: ValueExpr,
        value: ValueExpr,
    },
    MulAssign {
        variable: ValueExpr,
        value: ValueExpr,
    },
    DivAssign {
        variable: ValueExpr,
        value: ValueExpr,
    },
    If {
        condition: ValueExpr,
        if_body: CodeBlock,
        else_body: Option<CodeBlock>,
    },
    ForIn {
        iter_item: VariableDefineExpr,
        iter: ValueExpr,
        iter_body: CodeBlock,
        remain_body: Option<CodeBlock>,
    },
    While {
        condition: ValueExpr,
        body: CodeBlock,
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
    ReturnExpr(Box<Expr>),
    ValueExpr(ValueExpr),
    CodeBlock(CodeBlock),
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
    Add(Box<ValueExpr>, Box<ValueExpr>),
    Sub(Box<ValueExpr>, Box<ValueExpr>),
    Mul(Box<ValueExpr>, Box<ValueExpr>),
    Div(Box<ValueExpr>, Box<ValueExpr>),
    Remainder(Box<ValueExpr>, Box<ValueExpr>),
    LessThan(Box<ValueExpr>, Box<ValueExpr>),
    GreaterThan(Box<ValueExpr>, Box<ValueExpr>),
    Reference {
        value: Box<ValueExpr>,
        is_mut: bool,
    },
    Dereference(Box<ValueExpr>),
    BoolAnd(Box<ValueExpr>, Box<ValueExpr>),
    BoolOr(Box<ValueExpr>, Box<ValueExpr>),
    BoolEqual(Box<ValueExpr>, Box<ValueExpr>),
    BoolNotEqual(Box<ValueExpr>, Box<ValueExpr>),
    Tuple(Vec<ValueExpr>),
    Variable(String),
    Indexing {
        value: Box<ValueExpr>,
        index: Box<ValueExpr>,
    },
    As {
        value: Box<ValueExpr>,
        type_: Box<TypeExpr>,
    },
    Range {
        start: Box<ValueExpr>,
        end: Box<ValueExpr>,
        is_inclusive: bool,
    },
    FnCall {
        namespace: NamespaceChain,
        name: String,
        type_args: Option<Vec<TypeExpr>>,
        args: Vec<ValueExpr>,
    },
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

#[derive(Clone)]
pub enum VariableDefineExpr {
    One {
        is_mut: bool,
        name: String,
        type_: Option<TypeExpr>,
    },
    TupleDestruct(Vec<VariableDefineExpr>),
}

#[derive(Clone)]
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
pub struct CodeBlock(pub Vec<Expr>);
