use std::mem::take;

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

#[derive(Clone)]
struct RangeItemInfo {
    delta: i64,
    min: i64,
    max: i64,
}

pub struct Analyzer {
    ast: SyntaxTree,
    scope_stack: Vec<HashMap<Var, TypeExpr>>,
    static_items: HashMap<String, StaticItem>,
    scope_info: HashMap<CodeBlock, (usize)>,
    range_item_info: HashMap<String, RangeItemInfo>,
    is_in_loop: bool,
}

impl Analyzer {
    pub fn analyze_static(ast: SyntaxTree) -> SyntaxTree {
        let mut analyzer = Self::new(ast);
        analyzer.analyze()
    }

    pub fn new(ast: SyntaxTree) -> Self {
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
            range_item_info: HashMap::new(),
            scope_info: HashMap::new(),
        }
    }

    pub fn analyze(&mut self) -> SyntaxTree {
        self._analyze_routine( &self.ast.main_routine);

        take(&mut self.ast)
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

    fn find_value_scope_floor(&self, name: &String) -> Option<usize> {
        for (i, scope) in self.scope_stack.iter().enumerate().rev() {
            if let Some(..) = scope.get(name) {
                return Some(i);
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

    fn analyze_for_in(&mut self, iter_item: &VariableDefineExpr, iter: &ValueExpr, iter_body: &CodeBlock) {
        if self.is_in_loop {
            return;
        }

        let mut scope = HashMap::new();
        let mut try_chunk = false;

        let VariableDefineExpr::One { name: iter_item, .. } = iter_item else {
            return;
        };

        match iter {
            ValueExpr::Range {start, end, is_inclusive} => {
                scope.insert(iter_item.into(), TypeExpr::Name("usize".into()));

                if let ValueExpr::IntagerLiteral(start) = start.as_ref() {
                if let ValueExpr::IntagerLiteral(end) = end.as_ref() {
                    try_chunk = true;

                    let end = if !*is_inclusive {
                        *end-1
                    } else {
                        *end
                    };

                    self.range_item_info.insert(iter_item.into(), RangeItemInfo {
                        delta: 1,
                        min: *start.min(&end),
                        max: *start.max(&end),
                    });
                }
                }
            }
            ValueExpr::Variable(var) => {
                let Some(TypeExpr::WithArgs(typename, typeargs)) = self.find_value_from_scopes(var) else {
                    return;
                };
                if typename != "Vec" {
                    return;
                };

                scope.insert(iter_item.into(), typeargs[0].clone());
            }
            _ => return
        };
        
        self.is_in_loop = true;            
        self.goto_scope(&iter_body.0, scope);
        self.is_in_loop = false;

        if try_chunk {
            self.range_item_info.remove(iter_item);
        }
    }

    fn analyze_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::VariableLet { define_expr, value } => {
                let VariableDefineExpr::One {
                    name,
                    ..
                } = define_expr
                else {
                    return;
                };

                let type_ = if let Some(res) = self.get_define_type(define_expr) {
                    res.clone()
                } else if let Some(res) = self.get_type_of_value(value) {
                    res
                } else {
                    return;
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
                    return;
                };

                let type_ = if let Some(res) = type_ {
                    res.clone()
                } else if let Some(res) = self.get_type_of_value(value) {
                    res
                } else {
                    return;
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
            } => self.analyze_for_in(iter_item, iter, iter_body),
            _ => {}
        }
    }

    fn _analyze_routine(&mut self, routine: &Vec<Expr>) {
        for expr in routine {
            self.analyze_expr(expr);
        }
    }

    fn goto_scope(&mut self, routine: &Vec<Expr>, scope: HashMap<String, TypeExpr>) {
        self.scope_stack.push(scope);
        self._analyze_routine(routine);
        self.scope_stack.pop();
    }

    fn is_chunkable(&self, expr: &mut ValueExpr) -> bool {
        self.try_const_folding(expr);

        self.is_affine_form(expr)
        && self.is_injective(expr)
    }

    fn is_affine_form(&self, expr: &ValueExpr) -> bool {
        match expr {
            ValueExpr::Add(varl, varr)
            | ValueExpr::Sub(varl, varr) => {
                self.is_affine_form(varl.as_ref())
                && self.is_affine_form(varr.as_ref())
            }
            ValueExpr::Mul(varl, varr) => {
                if let ValueExpr::IntagerLiteral(..) = varr.as_ref() {
                    true
                } else if let ValueExpr::IntagerLiteral(..) = varl.as_ref() {
                    true
                } else {
                    false
                }
            }
            ValueExpr::Div(.., varr) => {
                if let ValueExpr::IntagerLiteral(..) = varr.as_ref() {
                    true
                } else {
                    false
                }
            }
            ValueExpr::Variable(name) => {
                if let Some(..) = self.range_item_info.get(name) {
                    true
                } else {
                    false
                }
            }
            ValueExpr::IntagerLiteral(..) => true,
            _ => false,
        }
    }

    fn is_injective(&self, expr: &ValueExpr) -> bool {
        self._is_injective_inner(expr).is_some()
    }

    fn _is_injective_inner(&self, expr: &ValueExpr) -> Option<RangeItemInfo> {
        fn _add_range(&self, r0: &ValueExpr, r1: &ValueExpr) -> Option<RangeItemInfo> {
            let ValueExpr::Variable(varl) = r0 else {
                unreachable!();
            };
            let ValueExpr::Variable(varr) = r1 else {
                unreachable!();
            };
            
            let varl_floor = self.find_value_scope_floor(varl);
            let varr_floor = self.find_value_scope_floor(varr);

            let (range_parent, range_child) = if varl_floor > varr_floor {
                (varr, varl)
            } else {
                (varl, varr)
            };
            
            let range_parent = self.range_item_info.get(range_parent).unwrap();
            let range_child = self.range_item_info.get(range_child).unwrap();

            if range_parent.delta.abs() as u64 > range_child.max.abs_diff(range_child.min) {
                return Some(RangeItemInfo { delta: 0, min: 0, max: 0 });
            } else {
                return None;
            };
        }
        
        fn aa() {

        }

        match expr {
            ValueExpr::Add(varl, varr) => {
                let (num, valexpr) = match (varl.as_ref(), varr.as_ref()) {
                    (ValueExpr::IntagerLiteral(n), e) => (n, e),
                    (e, ValueExpr::IntagerLiteral(n)) => (n, e),
                    (r0, r1) => return _add_range(&self, r0, r1),
                };

                let Some(mut range_info) = self._is_injective_inner(valexpr.as_ref()) else {
                    return None;
                };

                range_info.min += num;
                range_info.max += num;
                range_info.delta += num;

                Some(range_info)
            }
            ValueExpr::Sub(varl, varr) => {
                let (num, valexpr) = match (varl.as_ref(), varr.as_ref()) {
                    (ValueExpr::IntagerLiteral(n), e) => (n, e),
                    (e, ValueExpr::IntagerLiteral(n)) => (n, e),
                    (r0, r1) => return self._add_range(r0, r1),
                };

                let Some(mut range_info) = self._is_injective_inner(valexpr) else {
                    return None;
                };

                range_info.min -= num;
                range_info.max -= num;
                range_info.delta -= num;

                Some(range_info)
            }
            ValueExpr::Mul(varl, varr) => {
                let (num, valexpr) = match (varl.as_ref(), varr.as_ref()) {
                    (ValueExpr::IntagerLiteral(n), e) => (n, e),
                    (e, ValueExpr::IntagerLiteral(n)) => (n, e),
                    _ => return None,
                };

                let Some(mut range_info) = self._is_injective_inner(valexpr) else {
                    return None;
                };

                range_info.min *= num;
                range_info.max *= num;
                range_info.delta *= num;

                return Some(range_info);
            }
            ValueExpr::Div(varl, varr) => {
                let (num, valexpr) = match (varl.as_ref(), varr.as_ref()) {
                    (ValueExpr::IntagerLiteral(n), e) => (n, e),
                    (e, ValueExpr::IntagerLiteral(n)) => (n, e),
                    _ => return None,
                };

                let Some(mut range_info) = self._is_injective_inner(valexpr) else {
                    return None;
                };

                range_info.min /= num;
                range_info.max /= num;
                range_info.delta /= num;

                return Some(range_info);
            }
            ValueExpr::Variable(name) => {
                self.range_item_info.get(name).map(|r| r.clone())
            }
            _ => None,
        }
    }

    fn try_const_folding(&self, expr: &mut ValueExpr) {
        match expr {
            ValueExpr::Add(varl, varr) => {
                self.try_const_folding(varl);
                self.try_const_folding(varr);

                if let ValueExpr::IntagerLiteral(varl) = varl.as_ref() {
                if let ValueExpr::IntagerLiteral(varr) = varr.as_ref() {
                    *expr = ValueExpr::IntagerLiteral(varl + varr);
                }
                }
            }
            ValueExpr::Sub(varl, varr) => {
                self.try_const_folding(varl);
                self.try_const_folding(varr);

                if let ValueExpr::IntagerLiteral(varl) = varl.as_ref() {
                if let ValueExpr::IntagerLiteral(varr) = varr.as_ref() {
                    *expr = ValueExpr::IntagerLiteral(varl - varr);
                }
                }
            }
            ValueExpr::Mul(varl, varr) => {
                self.try_const_folding(varl);
                self.try_const_folding(varr);

                if let ValueExpr::IntagerLiteral(varl) = varl.as_ref() {
                if let ValueExpr::IntagerLiteral(varr) = varr.as_ref() {
                    *expr = ValueExpr::IntagerLiteral(varl * varr);
                }
                }
            }
            ValueExpr::Div(varl, varr) => {
                self.try_const_folding(varl);
                self.try_const_folding(varr);

                if let ValueExpr::IntagerLiteral(varl) = varl.as_ref() {
                if let ValueExpr::IntagerLiteral(varr) = varr.as_ref() {
                    *expr = ValueExpr::IntagerLiteral(varl / varr);
                }
                }
            }
            _ => {}
        }
    }
}

// - 데이터 종속성 파악을 통한 simd 연산
// - 스케줄링 구현
// - 자동 스레딩
