use ahash::{HashMap, HashMapExt};

use crate::compiler::{codegen::ToRust, exprs::*, parser::SyntaxTree};

type Var = String;
type Type_ = String;

pub struct Analyzer<'a> {
    ast: &'a SyntaxTree,
    scope_stack: Vec<HashMap<Var, Type_>>,
}

impl<'a> Analyzer<'a> {
    pub fn analyze_static(ast: &'a SyntaxTree) {
        let mut analyzer = Self::new(ast);
        analyzer.analyze()
    }

    pub fn new(ast: &'a SyntaxTree) -> Self {
        Self {
            ast,
            scope_stack: vec![HashMap::new()],
        }
    }

    pub fn analyze(&mut self) {
        self.analyze_routine(&self.ast.main_routine);
    }

    fn analyze_type(&mut self, expr: &Expr) {}

    fn analyze_value_expr(&mut self, expr: &ValueExpr) {
        match expr {
            _ => {}
        }
    }

    fn analyze_for_in(&mut self) {}

    fn analyze_routine(&mut self, routine: &Vec<Expr>) {
        self.scope_stack.push(HashMap::new());

        let curr_scope = self.scope_stack.len() - 1;
        let curr_scope = self.scope_stack.get_mut(curr_scope).unwrap();

        for expr in routine {
            match expr {
                Expr::FnDefine {
                    name,
                    type_args,
                    return_type,
                    args,
                    body,
                } => {
                    curr_scope.insert(name.clone(), return_type.to_rust());
                }
                Expr::VariableLet { define_expr, value } => {
                    let VariableDefineExpr::One {
                        is_mut,
                        name,
                        type_,
                    } = define_expr
                    else {
                        continue;
                    };
                }
                _ => {}
            }
        }

        self.scope_stack.pop();
    }
}
