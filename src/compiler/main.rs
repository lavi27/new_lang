mod ast_parser;
mod code_generater;
mod token_stream;
#[macro_use]
mod utils;

pub struct Compiler {
    input_code: String,
}

impl Compiler {
    pub fn compile(input_code: String) -> String {
        let compiler = Self::new(input_code);
        compiler.compile()
    }

    pub fn new(input_code: String) -> Self {
        Self {
            input_code,
        }
    }

    pub fn compile(&self) -> String {
        let ast = AstParser::parse(input_code);
        let Ok(ast) = ast else {
            panic!(ast.unwrap_err());
        }

        let output_code = CodeGenerater::generate(&ast);

        output_code    
    }
}