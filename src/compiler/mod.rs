mod ast_parser;
mod code_generater;
#[macro_use]
mod utils;
mod exprs;

use std::mem::take;
use std::process::Command;
use std::{
    fs::*,
    io::{Read, Write},
    path::Path,
};

use ast_parser::*;
use code_generater::*;

pub struct Compiler {
    path: String,
    outdir: String,
    option: CompileOption,
}

#[derive(Clone, Copy)]
pub struct CompileOption {
    pub transfile: bool,
}

impl Default for CompileOption {
    fn default() -> Self {
        Self { transfile: false }
    }
}

impl Compiler {
    pub fn compile_static(path: String, outdir: String, opt: CompileOption) -> Result<(), ()> {
        let mut compiler = Self::new(path, outdir, opt);
        compiler.compile()
    }

    pub fn new(path: String, outdir: String, option: CompileOption) -> Self {
        Self {
            path,
            outdir,
            option,
        }
    }

    pub fn compile(&mut self) -> Result<(), ()> {
        let input_code = {
            let mut tmp = String::with_capacity(256);
            let stdin = File::open(self.path.clone()).expect("msg");
            let mut stdin = std::io::BufReader::new(stdin);

            stdin.read_to_string(&mut tmp);
            tmp
        };

        let filename = Path::new(&self.path)
            .file_name()
            .expect("msg")
            .to_str()
            .expect("msg")
            .to_string();

        let (output_code, base_code) = self.compile_code(input_code, filename).unwrap();

        create_dir(self.outdir.clone());

        Command::new("cargo")
            .arg("init")
            .arg(self.outdir.clone())
            .arg("--bin")
            .status()
            .expect("Error during cargo build.");

        let stdout =
            File::create(format!("{}/src/_newlang_base.rs", self.outdir.clone())).expect("msg");
        let mut stdout = std::io::BufWriter::new(stdout);
        stdout.write_all(base_code.as_bytes());

        let stdout = File::create(self.outdir.clone() + "/src/main.rs").expect("msg");
        let mut stdout = std::io::BufWriter::new(stdout);
        stdout.write_all(output_code.as_bytes());

        if !self.option.transfile {
            Command::new("cargo")
                .arg("build")
                .arg("--manifest-path")
                .arg(self.outdir.clone() + "/Cargo.toml")
                .spawn()
                .expect("Error during cargo build.");
        }

        Ok(())
    }

    fn compile_code(
        &mut self,
        mut input_code: String,
        filename: String,
    ) -> Result<(String, String), ()> {
        let ast = ASTParser::parse_static(filename.clone(), take(&mut input_code));
        let Ok(ast) = ast else {
            unsafe { println!("{}", ast.unwrap_err_unchecked()) };

            return Err(());
        };

        Ok(CodeGenerater::generate_static(&ast, self.option))
    }
}
