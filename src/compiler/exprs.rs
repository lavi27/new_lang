
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

#[derive(Clone)]
pub enum VariableDefineExpr {
    Name(String),
    WithType(String, TypeExpr),
    TupleDestruct(Vec<VariableDefineExpr>),
}

#[derive(Clone)]
pub enum TypeExpr {
    Name(String),
    Tuple(Vec<TypeExpr>),
    WithArgs(String, Vec<TypeExpr>),
}

#[derive(Clone)]
pub struct NamespaceChain(pub Vec<String>);

impl NamespaceChain {
    pub fn new() -> Self {
        Self(Vec::new())
    }
}

#[derive(Clone)]
pub struct CodeBlock(pub Vec<Expr>);
