pub struct StaticTester {
    ast
}

impl StaticTester {
    pub fn test(ast: &AbstractSyntaxTree) -> Vec<String> {
        let mut tester = Self::new(ast);
        tester.test()
    }

    pub fn new(ast: &AbstractSyntaxTree) -> Self {
        Self {
            ast
        }
    }

    pub fn test(&self) -> Vec<String> {
        
    }
}