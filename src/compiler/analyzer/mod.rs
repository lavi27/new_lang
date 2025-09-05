use ahash::{HashMap, HashMapExt};

use crate::compiler::{exprs::*, parser::SyntaxTree};

type Var = String;

enum StaticItem {
    Module(Module),
    Func{name: String, type_args: Vec<String>, return_type: TypeExpr},
}

struct Module {
    name: String,
    children: HashMap<String, StaticItem>,
}

pub struct Analyzer<'a> {
    ast: &'a SyntaxTree,
    scope_stack: Vec<HashMap<Var, TypeExpr>>,
    static_items: HashMap<String, StaticItem>,
    is_in_loop: bool,
}

impl<'a> Analyzer<'a> {
    pub fn analyze_static(ast: &'a SyntaxTree) {
        let mut analyzer = Self::new(ast);
        analyzer.analyze()
    }

    pub fn new(ast: &'a SyntaxTree) -> Self {
        let mut children = HashMap::new();
        children.insert("with_capacity".to_string(), StaticItem::Func{
            name: "with_capacity".into(),
            type_args: vec!["T".into()],
            return_type: TypeExpr::WithArgs("Vec".into(), vec![TypeExpr::Name("T".into())])
        });

        children.insert("new".to_string(), StaticItem::Func{
            name: "new".into(),
            type_args: vec!["T".into()],
            return_type: TypeExpr::WithArgs("Vec".into(), vec![TypeExpr::Name("T".into())])
        });

        let mut static_items = HashMap::new();
        static_items.insert("Vec".to_string(),
            StaticItem::Module(Module {
                name: "Vec".into(),
                children,
            })
        );

        Self {
            ast,
            scope_stack: Vec::new(),
            static_items,
            is_in_loop: false,
        }
    }

    pub fn analyze(&mut self) {
        self._analyze_routine(&self.ast.main_routine);
    }

    // ---

    fn find_value_from_scopes(&self, name: &String) -> Option<TypeExpr> {
        for scope in self.scope_stack.iter().rev() {
            if let Some(res) = scope.get(name) {
                return Some(res.clone());
            }
        }

        return None;
    }

    fn get_type_of_value(&mut self, expr: &ValueExpr) -> Option<TypeExpr> {
        match expr {
            ValueExpr::Add(varl, varr) => {
                let Some(typel) = self.get_type_of_value(varl) else {
                    return None;
                };
                let Some(typer) = self.get_type_of_value(varr) else {
                    return None;
                };

                if typel == typer {
                    Some(typel)
                } else {
                    None
                }
            }
            ValueExpr::Sub(varl, varr) => {
                let Some(typel) = self.get_type_of_value(varl) else {
                    return None;
                };
                let Some(typer) = self.get_type_of_value(varr) else {
                    return None;
                };

                if typel == typer {
                    Some(typel)
                } else {
                    None
                }
            }
            ValueExpr::Mul(varl, varr) => {
                let Some(typel) = self.get_type_of_value(varl) else {
                    return None;
                };
                let Some(typer) = self.get_type_of_value(varr) else {
                    return None;
                };

                if typel == typer {
                    Some(typel)
                } else {
                    None
                }
            }
            ValueExpr::Div(varl, varr) => {
                let Some(typel) = self.get_type_of_value(varl) else {
                    return None;
                };
                let Some(typer) = self.get_type_of_value(varr) else {
                    return None;
                };

                if typel == typer {
                    Some(typel)
                } else {
                    None
                }
            }
            ValueExpr::FnCall { name, type_args, .. } => {
                let Some(StaticItem::Func { return_type, type_args: type_args_signature, .. }) = self.static_items.get(name) else {
                    return None;
                };

                let mut return_type = return_type;

                if let TypeExpr::Name(name) = return_type {
                if let Some(type_args) = type_args {
                    for (arg_sig, arg) in type_args_signature.iter().zip(type_args.iter()) {
                        if name == arg_sig {
                            return_type = arg;
                            break;
                        }
                    }
                }
                }

                Some(return_type.clone())
            }
            ValueExpr::Variable(name) => {
                self.find_value_from_scopes(name)
            }
            _ => None
        }
    }

    fn get_define_type(&mut self, expr: &VariableDefineExpr) -> Option<TypeExpr> {
        match expr {
            VariableDefineExpr::One { type_, .. } => {
                type_.clone()
            }
            _ => None
        }
    }

    fn _analyze_routine(&mut self, routine: &Vec<Expr>) {
        for expr in routine {
            match expr {
                Expr::VariableLet { define_expr, value } => {
                    let VariableDefineExpr::One {
                        name,
                        ..
                    } = define_expr
                    else {
                        continue;
                    };

                    let type_ = if let Some(res) = self.get_define_type(define_expr) {
                        res.clone()
                    } else if let Some(res) = self.get_type_of_value(value) {
                        res
                    } else {
                        continue;
                    };

                    self.scope_stack.last_mut().unwrap().insert(name.clone(), type_);
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

                    let type_ = if let Some(res) = type_ {
                        res.clone()
                    } else if let Some(res) = self.get_type_of_value(value) {
                        res
                    } else {
                        continue;
                    };


                    self.scope_stack.last_mut().unwrap().insert(name.clone(), type_);
                }
                Expr::FnDefine {
                    name,
                    type_args,
                    return_type,
                    args,
                    body,
                } => {
                    let mut scope = HashMap::new();
                    
                    for arg in args {
                        let VariableDefineExpr::One {
                            is_mut,
                            name,
                            type_: Some(type_),
                        } = arg else {
                            continue;
                        };     

                        scope.insert(name.clone(), type_.clone());
                    }
                    
                    self.goto_scope(&body.0, scope);

                    self.static_items.insert(name.clone(), StaticItem::Func {
                        name: name.clone(),
                        return_type: return_type.clone(),
                        type_args: type_args.clone().unwrap_or(Vec::new()),
                    });
                }
                Expr::ForIn {
                    iter_item,
                    iter,
                    iter_body,
                    ..
                } if !self.is_in_loop => {
                    let mut scope = HashMap::new();

                    let VariableDefineExpr::One { name: iter_item, .. } = iter_item else {
                        continue;
                    };

                    match iter {
                        ValueExpr::Range {..} => {
                            scope.insert(iter_item.into(), TypeExpr::Name("usize".into()));
                        }
                        ValueExpr::Variable(var) => {
                            let Some(TypeExpr::WithArgs(typename, typeargs)) = self.find_value_from_scopes(var) else {
                                continue;
                            };
                            if typename != "Vec" {
                                continue;
                            };

                            scope.insert(iter_item.into(), typeargs[0].clone());
                        }
                        _ => continue
                    };
                    
                    self.is_in_loop = true;            
                    self.goto_scope(&iter_body.0, scope);
                    self.is_in_loop = false;
                }
                _ => {}
            }
        }
    }

    fn goto_scope(&mut self, routine: &Vec<Expr>, scope: HashMap<String, TypeExpr>) {
        self.scope_stack.push(scope);
        self._analyze_routine(routine);
        self.scope_stack.pop();
    }
}

// - 데이터 종속성 파악을 통한 simd 연산
// - 스케줄링 구현
// - 자동 스레딩
