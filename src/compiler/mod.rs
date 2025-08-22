mod ast_parser;
mod code_generater;
mod token_stream;
#[macro_use]
mod utils;

use ast_parser::*;
use code_generater::*;

pub struct Compiler {
    filename: String,
    input_code: String,
    option: CompileOption,
}

pub struct CompileOption {
    transfile: bool,
}

impl Default for CompileOption {
    fn default() -> Self {
        Self {
            transfile: false,
        }
    }
}

impl Compiler {
    pub fn compile_static(filename: String, input_code: String, opt: CompileOption) -> String {
        let compiler = Self::new(filename, input_code, opt);
        compiler.compile()
    }

    pub fn new(filename: String, input_code: String, option: CompileOption) -> Self {
        Self {
            filename,
            input_code,
            option
        }
    }

    pub fn compile(&self) -> String {
        let ast = ASTParser::parse_static(self.filename, self.input_code);
        let Ok(ast) = ast else {
            panic!("{}", ast.unwrap_err());
        };

        let output_code = CodeGenerater::generate_static(&ast, self.option);

        if !self.option.transfile {
        command::("rustc ...");
            todo!();    
        }

        output_code    
    }
}