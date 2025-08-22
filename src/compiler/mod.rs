mod ast_parser;
mod code_generater;
mod token_stream;
#[macro_use]
mod utils;

pub struct Compiler {
    filename: String,
    input_code: String,
}

impl Compiler {
    pub fn compile_static(filename: String, input_code: String) -> String {
        let compiler = Self::new(filename, input_code);
        compiler.compile()
    }

    pub fn new(filename: String, input_code: String) -> Self {
        Self {
            filename,
            input_code,
        }
    }

    pub fn compile(&self) -> String {
        let ast = ast_parser::ASTParser::parse_static(self.filename, self.input_code);
        let ast = ast.unwrap_err()
        let Ok(ast) = ast else {
            panic!("{}", ast.unwrap_err());
        };

        let output_code = code_generater::CodeGenerater::generate_static(&ast);

        output_code
    }
}
