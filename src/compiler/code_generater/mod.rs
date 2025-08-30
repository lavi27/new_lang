mod base_rust;

use ahash;
use std::hash::{BuildHasher, Hasher};
use std::sync::LazyLock;

use crate::{
    compiler::{ast_parser::*, exprs::*, CompileOption},
    s,
};

// Anti pattern lol
static mut skill_issue: LazyLock<ahash::RandomState> = LazyLock::new(|| ahash::RandomState::new());

pub fn get_static_var_hash(name: &str) -> String {
    let mut hasher = unsafe { skill_issue.build_hasher() };

    hasher.write(name.as_bytes());
    let hash = hasher.finish();

    format!("_{}_{:016x}", name, hash)
}

pub struct CodeGenerater<'a> {
    ast: &'a AbstractSyntaxTree,
    option: CompileOption,
    result: String,
    // static_var_hasher: FxHasher,
}

impl<'a> CodeGenerater<'a> {
    pub fn generate_static(ast: &'a AbstractSyntaxTree, opt: CompileOption) -> (String, String) {
        let mut generater = Self::new(ast, opt);
        generater.generate_rust()
    }

    pub fn new(ast: &'a AbstractSyntaxTree, option: CompileOption) -> Self {
        Self {
            ast,
            option,
            result: String::new(),
            // static_var_hasher: FxBuildHasher::default().build_hasher(),
        }
    }

    // fn get_static_var_hash(&mut self, name: String) -> String {
    //     self.static_var_hasher.write(name.as_str().as_bytes());
    //     let hash = self.static_var_hasher.finish();

    //     format!("{:016x}", hash)
    // }

    pub fn generate_rust(&self) -> (String, String) {
        // Find a way to format const str

        let namespace_newlang = get_static_var_hash("newlang");
        let mut result = format!("mod _newlang_base;\nuse _newlang_base as {namespace_newlang};\n");

        result += self.ast.to_rust().as_str();

        let mut base = s!("");

        if self.ast.is_threading_used {
            base += base_rust::THREADING_BASE;
        }

        (result, base)
    }
}

fn expr_vec_to_rust(vec: &Vec<impl ToRust>, sep: &str) -> String {
    vec.iter()
        .map(|i| i.to_rust())
        .collect::<Vec<String>>()
        .join(sep)
}

pub trait ToRust {
    fn to_rust(&self) -> String;
}

// move logic to CodeGen. Implement Visitor.
impl ToRust for Expr {
    fn to_rust(&self) -> String {
        match self {
            Self::CodeBlock(expr) => expr.to_rust() + ";",
            Self::ValueExpr(expr) => expr.to_rust() + ";",
            Self::VariableDefineExpr(expr) => expr.to_rust() + ";",
            Self::TypeExpr(expr) => expr.to_rust() + ";",
            Self::NamespaceChain(expr) => expr.to_rust() + ";",
            Self::NamespaceTree(expr) => expr.to_rust() + ";",
            Self::NamespaceUse(expr) => format!("use {};", expr.to_rust()),
            Self::EqualAssign { variable, value } => {
                format!("{} = {};", variable.to_rust(), value.to_rust())
            }
            Self::AddAssign { variable, value } => {
                format!("{} += {};", variable.to_rust(), value.to_rust())
            }
            Self::SubAssign { variable, value } => {
                format!("{} -= {};", variable.to_rust(), value.to_rust())
            }
            Self::MulAssign { variable, value } => {
                format!("{} *= {};", variable.to_rust(), value.to_rust())
            }
            Self::DivAssign { variable, value } => {
                format!("{} /= {};", variable.to_rust(), value.to_rust())
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
            Self::While { condition, body } => {
                format!("while {} {}", condition.to_rust(), body.to_rust())
            }
            Self::ForIn {
                iter_item,
                iter,
                iter_body,
                remain_body,
            } => for_in_to_rust(iter_item, iter, iter_body, remain_body),
            Self::ParallelForIn {
                iter_item,
                iter,
                iter_body,
                remain_body,
            } => parallel_for_in_to_rust(iter_item, iter, iter_body, remain_body),
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
            Self::BoolAnd(lvel, rvel) => format!("{}&&{}", lvel.to_rust(), rvel.to_rust()),
            Self::BoolOr(lvel, rvel) => format!("{}||{}", lvel.to_rust(), rvel.to_rust()),
            Self::BoolEqual(lvel, rvel) => format!("{}=={}", lvel.to_rust(), rvel.to_rust()),
            Self::BoolNotEqual(lvel, rvel) => format!("{}!={}", lvel.to_rust(), rvel.to_rust()),
            Self::Variable(var) => var.to_string(),
            Self::Reference { value, is_mut } => {
                let mut_ = if *is_mut { "mut " } else { "" };

                format!("&{mut_}{}", value.to_rust())
            }
            Self::Dereference(expr) => format!("*{}", expr.to_rust()),
            Self::As { value, type_ } => format!("{} as {}", value.to_rust(), type_.to_rust()),
            Self::Indexing { value, index } => format!("{}[{}]", value.to_rust(), index.to_rust()),
            Self::Range {
                start,
                end,
                is_inclusive,
            } => {
                if is_inclusive.clone() {
                    format!("{}..={}", start.to_rust(), end.to_rust())
                } else {
                    format!("{}..{}", start.to_rust(), end.to_rust())
                }
            }
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

impl ToRust for VariableDefineExpr {
    fn to_rust(&self) -> String {
        match self {
            Self::One {
                is_mut,
                name,
                type_,
            } => {
                let mut_ = if *is_mut { "mut " } else { "" };
                let type_ = if let Some(type_) = type_ {
                    format!(": {}", type_.to_rust())
                } else {
                    "".to_string()
                };

                format!("{mut_}{name}{type_}")
            }
            Self::TupleDestruct(items) => format!("({})", expr_vec_to_rust(items, ", ")),
        }
    }
}

impl ToRust for TypeExpr {
    fn to_rust(&self) -> String {
        match self {
            Self::Name(name) => name.to_owned(),
            Self::Tuple(types) => {
                format!(
                    "({})",
                    types
                        .iter()
                        .map(|i| i.to_rust())
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
            Self::WithArgs(name, args) => {
                format!("{name}<{}>", expr_vec_to_rust(args, ", "))
            }
        }
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

impl ToRust for NamespaceTree {
    fn to_rust(&self) -> String {
        let body = self
            .0
            .iter()
            .map(|i| i.clone())
            .collect::<Vec<String>>()
            .join("::");

        let tail = if let Some(tail) = &self.1 {
            let items = tail
                .iter()
                .map(|i| i.to_rust())
                .collect::<Vec<String>>()
                .join(", ");

            format!("::{{{}}}", items)
        } else {
            s!("")
        };

        format!("{}{}", body, tail)
    }
}

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

impl ToRust for AbstractSyntaxTree {
    fn to_rust(&self) -> String {
        expr_vec_to_rust(&self.main_routine, "\n")
    }
}

macro_rules! put {
    ($target:expr, $($items:expr),+) => {
        $target.push_str(&format!($($items),+));
    };
}

pub fn parallel_for_in_to_rust(
    iter_item: &VariableDefineExpr,
    iter: &ValueExpr,
    iter_body: &CodeBlock,
    remain_body: &Option<CodeBlock>,
) -> String {
    let mut res = String::with_capacity(128);
    let namespace_newlang = get_static_var_hash("newlang");

    if let ValueExpr::Tuple(items) = iter {
        todo!();
    } else {
        put!(res, "unsafe {{\n");
        {
            let var_thr_pool = get_static_var_hash("thr_pool");
            put!(
                res,
                "let {var_thr_pool} = {namespace_newlang}::get_thread_pool();\n"
            );

            let var_s = get_static_var_hash("s");
            put!(res, "{var_thr_pool}.scope(|{var_s}| {{\n");
            {
                let var_range = get_static_var_hash("range");
                put!(res, "for {var_range} in {namespace_newlang}::range_chunks(0..{}.len(), {var_thr_pool}.size(), 128) {{\n", iter.to_rust());
                {
                    let var_slice = get_static_var_hash("slice");
                    put!(res, "let mut {var_slice} = std::mem::ManuallyDrop::new");
                    put!(res, "(std::slice::from_raw_parts_mut({}.as_mut_ptr().add({var_range}.start), {var_range}.len()));\n", iter.to_rust());

                    put!(res, "{var_s}.execute(move || {{\n");
                    {
                        let var_i = get_static_var_hash("i");
                        put!(res, "for {var_i} in 0..{var_slice}.len() {{\n");
                        put!(
                            res,
                            "let {} = &mut {var_slice}[{var_i}];\n",
                            iter_item.to_rust()
                        );
                        put!(res, "{}", iter_body.to_rust());
                        put!(res, "}}\n");
                    }
                    put!(res, "}});\n");
                }
                put!(res, "}};\n");
            }
            put!(res, "}});\n");
        }
        put!(res, "}}\n");
    };

    res
}

pub fn for_in_to_rust(
    iter_item: &VariableDefineExpr,
    iter: &ValueExpr,
    iter_body: &CodeBlock,
    remain_body: &Option<CodeBlock>,
) -> String {
    let mut res = String::with_capacity(128);

    if let ValueExpr::Tuple(iters) = iter {
        let VariableDefineExpr::TupleDestruct(iter_items) = iter_item else {
            panic!("");
        };

        put!(res, "{{\n");
        {
            let var_iters = get_static_var_hash("iters");
            put!(
                res,
                "let mut {var_iters} = ({});\n",
                iters
                    .iter()
                    .map(|i| i.to_rust())
                    .collect::<Vec<String>>()
                    .join(", ")
            );

            let mut while_let_item = Vec::new();
            let mut while_let_iter = Vec::new();
            for (idx, item) in iter_items.iter().enumerate() {
                while_let_item.push(format!("Some({})", item.to_rust()));
                while_let_iter.push(format!("{var_iters}[{}].next()", idx));
            }
            let while_let_item = while_let_item.join(", ");
            let while_let_iter = while_let_iter.join(", ");

            put!(
                res,
                "while let ({}) = ({}) {}\n",
                while_let_item,
                while_let_iter,
                iter_body.to_rust()
            );

            if let Some(remain_body) = remain_body {
                put!(res, "loop {{\n");
                {
                    let mut end_cond = Vec::new();

                    for (idx, item) in iter_items.iter().enumerate() {
                        put!(
                            res,
                            "let {} = {var_iters}.{}.next();\n",
                            item.to_rust(),
                            idx
                        );
                        end_cond.push(format!("{}.is_none()", item.to_rust()));
                    }

                    let end_cond = end_cond.join("&&");

                    put!(res, "if {} {{break}};\n", end_cond);
                    put!(res, "{}\n", remain_body.to_rust());
                }
                put!(res, "}}\n");
            }
        }
        put!(res, "}}\n");
    } else {
        put!(
            res,
            "for {} in {} {}\n",
            iter_item.to_rust(),
            iter.to_rust(),
            iter_body.to_rust()
        );
    };

    res
}
