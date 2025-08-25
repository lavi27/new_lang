use crate::compiler::{
    ast_parser::{AbstractSyntaxTree, ToRust},
    CompileOption,
    gen_code_base,
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
