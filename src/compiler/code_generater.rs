use crate::compiler::{
    ast_parser::{AbstractSyntaxTree, ToRust},
    CompileOption,
};

const RUST_BASE: &str = "use std::cmp;

macro_rules! newlang_forin {
    (($( $iter_item:expr ), +), ($( $iter:expr ), +), $iter_body:block, $remain_body:block) => {
        let shortest_len = ziped_iters.len();
        let longest_len = [($($iter.len()), +)].into_iter().max();


        for _ in 0..shortest_len {
            let (($($iter_item), +)) = (($($iter.next().unwrap()), +));
            $iter_body;
        }


        for _ in shortest_len..longest_len {
            let (($($iter_item), +)) = (($($iter.next()), +));
            $remain_body;
        }
    }
}";

const RUST_THREADING_BASE: &str = "static mut THREAD_POOL = None;

fn get_thread_pool() {
    if THREAD_POOL.is_none() {
        let mut result = Vec::with_capacity(std::thread::available_parallelism().unwarp());
        result.push()

        THREAD_POOL = Some(result);
    }

    THREAD_POOL.unwrap()
}";

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
        let mut result = RUST_BASE.to_string();

        if self.ast.is_threading_used {
            result += RUST_THREADING_BASE;
        }

        result += self.ast.to_rust().as_str();

        result
    }
}
