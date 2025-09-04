use ahash::{HashMap, HashMapExt};

use crate::compiler::{codegen::ToRust, exprs::*, parser::SyntaxTree};

type Var = String;

pub struct Analyzer<'a> {
    ast: &'a SyntaxTree,
    scope_stack: Vec<HashMap<Var, TypeExpr>>,
}

impl<'a> Analyzer<'a> {
    pub fn analyze_static(ast: &'a SyntaxTree) {
        let mut analyzer = Self::new(ast);
        analyzer.analyze()
    }

    pub fn new(ast: &'a SyntaxTree) -> Self {
        let mut stdlib = HashMap::new();

        stdlib.insert("vec".into(), TypeExpr::WithArgs("".into(), Vec::new()));

        Self {
            ast,
            scope_stack: vec![stdlib],
        }
    }

    pub fn analyze(&mut self) {
        self.analyze_routine(&self.ast.main_routine);
    }

    fn get_type_of_value(&mut self, expr: &ValueExpr) -> TypeExpr {
        match expr {
            ValueExpr::
            _ => {}
        }
    }

    fn analyze_for_in(&mut self, iter_item: &VariableDefineExpr, iter:&ValueExpr, body: &CodeBlock) {}

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
                    self.analyze_routine(&body.0);

                    curr_scope.insert(name.clone(), return_type.clone());
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

                    let type_ = type_.unwrap_or_else(|| self.get_type_of_value(value));

                    curr_scope.insert(name.clone(), type_);
                }
                Expr::VariableVar { define_expr, value } => {
                    let VariableDefineExpr::One {
                        is_mut,
                        name,
                        type_,
                    } = define_expr
                    else {
                        continue;
                    };

                    let type_ = type_.clone().unwrap_or_else(|| self.get_type_of_value(value));

                    curr_scope.insert(name.clone(), type_);
                }
                Expr::ForIn {
                    iter_item,
                    iter,
                    iter_body,
                    remain_body,
                } => self.analyze_for_in(iter_item, iter, iter_body),
                _ => {}
            }
        }

        self.scope_stack.pop();
    }
}

// - 데이터 종속성 파악을 통한 simd 연산
// - 스케줄링 구현
// - 자동 스레딩
