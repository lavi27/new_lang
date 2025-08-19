pub struct CodeGenerater {
    ast: &AbstractSyntaxTree,
    result: String,
}

impl CodeGenerater {
  pub fn generate(ast: &AbstractSyntaxTree) -> String {
    let mut generater = Self::new();
    generater.generate_rust()
  }  
  
  pub fn new(ast: &AbstractSyntaxTree) {
      Self {
        ast,
        result: String::new(),
      }
    }

  pub fn generate_rust(&mut self) {
    self.result = self.ast.main_routine.to_rust()
  }
}