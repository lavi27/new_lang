pub struct CodeGenerater {
    ast: AbstractSyntaxTree,
    result: String,
}

impl CodeGenerater {
    pub fn new(ast:AbstractSyntaxTree) {
      Self {
        ast,
        result: String::new(),
      }
    }

    pub fn generate_rust() {
      for line in self.ast.main_routine.to_rust() {
        self.result.push(line);
      }
    }
}