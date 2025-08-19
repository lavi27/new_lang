use crate::compiler::ast_parser::AbstractSyntaxTree;

pub struct CodeGenerater<'a> {
    ast: &'a AbstractSyntaxTree,
    result: String,
}

impl<'a> CodeGenerater<'a> {
    pub fn generate_static(ast: &AbstractSyntaxTree) -> String {
        let mut generater = Self::new(ast);
        generater.generate_rust()
    }

    pub fn new(ast: &'a AbstractSyntaxTree) -> Self {
        Self {
            ast,
            result: String::new(),
        }
    }

    pub fn generate_rust(&mut self) {
        self.ast.main_routine.to_rust()
    }
}
