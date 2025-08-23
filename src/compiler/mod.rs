mod ast_parser;
mod code_generater;
mod token_stream;
#[macro_use]
mod utils;

use std::mem::take;

use ast_parser::*;
use code_generater::*;

pub struct Compiler {
    filename: String,
    input_code: String,
    option: CompileOption,
}

#[derive(Clone, Copy)]
pub struct CompileOption {
    transfile: bool,
}

impl Default for CompileOption {
    fn default() -> Self {
        Self { transfile: false }
    }
}

impl Compiler {
    pub fn compile_static(
        filename: String,
        input_code: String,
        opt: CompileOption,
    ) -> Result<String, ()> {
        let mut compiler = Self::new(filename, input_code, opt);
        compiler.compile()
    }

    pub fn new(filename: String, input_code: String, option: CompileOption) -> Self {
        Self {
            filename,
            input_code,
            option,
        }
    }

    pub fn compile(&mut self) -> Result<String, ()> {
        let ast = ASTParser::parse_static(self.filename.clone(), take(&mut self.input_code));
        let Ok(ast) = ast else {
            unsafe { println!("{}", ast.unwrap_err_unchecked()) };

            return Err(());
        };

        let output_code = CodeGenerater::generate_static(&ast, self.option.clone());

        if !self.option.transfile {
            // command::("rustc ...");
            todo!();
        }

        Ok(output_code)
    }
}
