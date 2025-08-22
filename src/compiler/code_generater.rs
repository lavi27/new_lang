const RUST_BASE: str = "static mut THREAD_POOL: Option<Vec<Handle>> = None;

fn get_thread_pool() {
    if THREAD_POOL.is_none() {
        let mut result = Vec::with_capacity();
        result.push()

        THREAD_POOL = Some(result);
    }

    THREAD_POOL.unwrap()
}

macro_rules! newlang_forin {
    (($( $iter_item:expr ), +), ($( $iter:expr ), +), $iter_body:block, $remain_body:block) => {
        let shortest_len = ziped_iters.len();
        let longest_len = usize::max(($($iter.len()), +));

        ($(let mut $iter_item;)+)

        for _ in 0..shortest_len {
            (($($iter_item), +)) = (($($iter.next().unwrap()), +));
            $iter_body;
        }

        for _ in shortest_len..longest_len {
            (($($iter_item), +)) = (($($iter.next().unwrap()), +));
            $remain_body;
        }
    }
}";

const RUST_THREADING_BASE = "static mut THREAD_POOL = None;";

use crate::compiler::ast_parser::AbstractSyntaxTree;

pub struct CodeGenerater<'a> {
    ast: &'a AbstractSyntaxTree,
    option: CompileOption,
    result: String,
}

impl<'a> CodeGenerater<'a> {
    pub fn generate_static(ast: &'a AbstractSyntaxTree, opt: CompileOption) -> String {
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
    let mut result = s!(RUST_BASE);

    if self.ast.is_threading_used {
      result += RUST_THREADING_BASE;
    }

    result += self.ast.main_routine.to_rust();

    result
  }
}
