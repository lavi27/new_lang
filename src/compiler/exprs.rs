// pub trait ExprVisitor {
//     fn visit_expr(&mut self, expr: &Expr);
//     fn visit_value_expr(&mut self, expr: &ValueExpr);
//     fn visit_var_def_expr(&mut self, expr: &VariableDefineExpr);
//     fn visit_codeblock(&mut self, expr: &CodeBlock);
//     fn visit_type_expr(&mut self, expr: &TypeExpr);
//     fn visit_namespace_tree(&mut self, expr: &NamespaceTree);
// }

use std::default;

use slotmap::{new_key_type, SlotMap};
use strum_macros::{Display, EnumString};

new_key_type! { pub struct ExprKey; }

impl ExprKey {
    pub fn get(&self, slotmap: &SlotMap<ExprKey, Expr>) -> Expr {
        slotmap.get(*self).unwrap().clone()
    }
}

new_key_type! { pub struct ValueExprKey; }

impl ValueExprKey {
    pub fn get(&self, ctx: &SlotMap<ValueExprKey, ValueExpr>) -> ValueExpr {
        ctx.get(*self).unwrap().clone()
    }

    pub fn is_same_value(
        &self,
        other: ValueExprKey,
        ctx: &SlotMap<ValueExprKey, ValueExpr>,
    ) -> bool {
        self.get(ctx).is_same_value(other.get(ctx), ctx)
    }
}

new_key_type! { pub struct CodeBlockKey; }

impl CodeBlockKey {
    pub fn get(&self, slotmap: &SlotMap<CodeBlockKey, CodeBlock>) -> CodeBlock {
        slotmap.get(*self).unwrap().clone()
    }
}

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

#[derive(Clone, PartialEq, Display)]
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

impl ValueExpr {
    pub fn is_same_value(&self, other: ValueExpr, ctx: &SlotMap<ValueExprKey, ValueExpr>) -> bool {
        match (self, other) {
            (Self::Variable(var_me), Self::Variable(var_other)) => *var_me == var_other,
            (Self::Sub(var_me_l, var_me_r), Self::Sub(var_other_l, var_other_r)) => {
                var_me_l.is_same_value(var_other_l, ctx) && var_me_r.is_same_value(var_other_r, ctx)
            }
            (Self::IntagerLiteral(int_l), Self::IntagerLiteral(int_r)) => *int_l == int_r,
            _ => false,
        }
    }
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

impl Default for TypeExpr {
    fn default() -> Self {
        Self::Name("".into())
    }
}

#[derive(Clone)]
pub struct NamespaceTree(pub Vec<String>, pub Option<Vec<NamespaceTree>>);

#[derive(Clone, PartialEq)]
pub struct NamespaceChain(pub Vec<String>);

impl NamespaceChain {
    pub fn new() -> Self {
        Self(Vec::new())
    }
}

impl Default for NamespaceChain {
    fn default() -> Self {
        Self(Vec::new())
    }
}

#[derive(Clone)]
pub struct CodeBlock(pub Vec<ExprKey>);
