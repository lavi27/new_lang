use crate::compiler::{
    ast_parser::*, gen_code_base, CompileOption
};

pub struct CodeGenerater<'a> {
    ast: &'a AbstractSyntaxTree,
    option: CompileOption,
    result: String,
}

impl<'a> CodeGenerater<'a> {
    pub fn generate_static(ast: &'a AbstractSyntaxTree, opt: CompileOption) -> String {
        let mut generater = Self::new(ast, opt);
        generater.generate_rust()
    }

    pub fn new(ast: &'a AbstractSyntaxTree, option: CompileOption) -> Self {
        Self {
            ast,
            option,
            result: String::new(),
        }
    }

    pub fn generate_rust(&mut self) -> String {
        let mut result = gen_code_base::rust::BASE.to_string();

        if self.ast.is_threading_used {
            result += gen_code_base::rust::THREADING_BASE;
        }

        result += self.ast.to_rust().as_str();

        result
    }
}

pub trait ToRust {
    fn to_rust(&self) -> String;
}

impl ToRust for Expr {
    fn to_rust(&self) -> String {
        match self {
            todo!()
        }
    }
}

macro_rules! putln {
    ($target:expr, $($items:expr),+) => {
        $target.push_str(&format!($($items),+));
        $target.push('\n');
    };
}

pub fn parallel_for_in_to_rust(iter_item: &VariableDefineExpr, iter: &ValueExpr, iter_body: &CodeBlock, remain_body: &Option<CodeBlock>) -> String {
let mut res = String::with_capacity(128);

    if let ValueExpr::Variable(var) = iter {
        let VariableDefineExpr::Name(iter_item) = iter_item else {
            panic!("");
        };

        putln!(res, "let __thr_pool = get_thread_pool();");
        putln!(res, "let __iter = {};", var);
        putln!(res, "let __iter_chunks: Vec<Vec<_>> = __iter.chunks(__thr_pool.size).map(|c| c.to_vec()).collect();");

        putln!(res, "for __chunk in __iter_chunks {{");
        putln!(res, "__thr_pool.execute(move || {{");

        putln!(res, "for {} in __chunk {{", iter_item);
        putln!(res, "{}", iter_body.to_rust());
        putln!(res, "}};");

        putln!(res, "}});");
        putln!(res, "}};");
    } else {
        panic!("");
    };

    res
}

pub fn for_in_to_rust(iter_item: &VariableDefineExpr, iter: &ValueExpr, iter_body: &CodeBlock, remain_body: &Option<CodeBlock>) -> String {
    let mut res = String::with_capacity(128);

    if let ValueExpr::Variable(var) = iter {
        let VariableDefineExpr::Name(iter_item) = iter_item else {
            panic!("");
        };

        putln!(res, "for {} in {} {}", iter_item, var, iter_body.to_rust());
    } else if let ValueExpr::Tuple(iters) = iter {
        let VariableDefineExpr::TupleDestruct(iter_items) = iter_item else {
            panic!("");
        };

        putln!(res, "let __iters = [{}];", iters.iter().map(|i| i.to_rust()).collect::<Vec<String>>().join(", "));
        putln!(res, "loop {{");
        for (idx, item) in iter_items.iter().enumerate() {
            putln!(res, "let Some({}) = __iters[{}].next() else {{break}};", item.to_rust(), idx);
        }
        putln!(res, "{}", iter_body.to_rust());
        putln!(res, "}}");

        let Some(remain_body) = remain_body else {
            return res;
        };

        putln!(res, "loop {{");
        putln!(res, "let mut __none_cnt = 0usize;");
        for (idx, item) in iter_items.iter().enumerate() {
            putln!(res, "let {} = __iters[{}].next();", item.to_rust(), idx);
            putln!(res, "if {}.is_none() {{__none_cnt += 1}};", item.to_rust());
        }
        putln!(res, "if __none_cnt == {} {{break}};", iter_items.len());
        putln!(res, "{}", remain_body.to_rust());
        putln!(res, "}}");
    } else {
        panic!("");
    };

    res
}