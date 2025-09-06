use std::mem::take;

use ahash::{HashMap, HashMapExt};

use crate::{
    compiler::{exprs::*, parser::SyntaxTree},
    none_to_err,
};

type Var = String;

enum StaticItem {
    Module(Module),
    Func {
        name: String,
        type_args: Vec<String>,
        return_type: TypeExpr,
    },
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

struct ScopeInfo {
    vars: HashMap<Var, TypeExpr>,
    is_chunkable: bool,
}

pub struct Analyzer {
    ast: SyntaxTree,
    scope_stack: Vec<ScopeInfo>,
    static_items: HashMap<String, StaticItem>,
    range_item_info: HashMap<String, RangeItemInfo>,
}

impl Analyzer {
    pub fn analyze_static(ast: SyntaxTree) -> SyntaxTree {
        let mut analyzer = Self::new(ast);
        analyzer.analyze()
    }

    pub fn new(ast: SyntaxTree) -> Self {
        let mut children = HashMap::new();
        children.insert(
            "with_capacity".to_string(),
            StaticItem::Func {
                name: "with_capacity".into(),
                type_args: vec!["T".into()],
                return_type: TypeExpr::WithArgs("Vec".into(), vec![TypeExpr::Name("T".into())]),
            },
        );

        children.insert(
            "new".to_string(),
            StaticItem::Func {
                name: "new".into(),
                type_args: vec!["T".into()],
                return_type: TypeExpr::WithArgs("Vec".into(), vec![TypeExpr::Name("T".into())]),
            },
        );

        let mut static_items = HashMap::new();
        static_items.insert(
            "Vec".to_string(),
            StaticItem::Module(Module {
                name: "Vec".into(),
                children,
            }),
        );

        Self {
            scope_stack: Vec::new(),
            static_items,
            range_item_info: HashMap::new(),
            ast,
        }
    }

    pub fn analyze(&mut self) -> SyntaxTree {
        self._analyze_routine(&self.ast.main_routine.clone());

        take(&mut self.ast)
    }

    // ---

    fn get_expr_mut(&mut self, id: ExprId) -> &mut Expr {
        &mut self.ast.exprs[id.0]
    }

    fn get_value_expr_mut(&mut self, id: ValueExprId) -> &mut ValueExpr {
        &mut self.ast.value_exprs[id.0]
    }

    fn get_code_block_mut(&mut self, id: CodeBlockId) -> &mut CodeBlock {
        &mut self.ast.code_blocks[id.0]
    }

    fn find_value_from_scopes(&self, name: &String) -> Option<TypeExpr> {
        for scope in self.scope_stack.iter().rev() {
            if let Some(res) = scope.vars.get(name) {
                return Some(res.clone());
            }
        }

        return None;
    }

    fn find_value_scope_floor(&self, name: &String) -> Option<usize> {
        for (i, scope) in self.scope_stack.iter().enumerate().rev() {
            if matches!(scope.vars.get(name), Some(..)) {
                return Some(i);
            }
        }

        return None;
    }

    fn get_type_of_value(&self, expr_id: ValueExprId) -> Option<TypeExpr> {
        let expr = self.ast.get_value_expr(expr_id);

        match expr {
            ValueExpr::Add(varl, varr) => {
                let typel = self.get_type_of_value(*varl)?;
                let typer = self.get_type_of_value(*varr)?;

                if typel == typer {
                    Some(typel)
                } else {
                    None
                }
            }
            ValueExpr::Sub(varl, varr) => {
                let typel = self.get_type_of_value(*varl)?;
                let typer = self.get_type_of_value(*varr)?;

                if typel == typer {
                    Some(typel)
                } else {
                    None
                }
            }
            ValueExpr::Mul(varl, varr) => {
                let typel = self.get_type_of_value(*varl)?;
                let typer = self.get_type_of_value(*varr)?;

                if typel == typer {
                    Some(typel)
                } else {
                    None
                }
            }
            ValueExpr::Div(varl, varr) => {
                let typel = self.get_type_of_value(*varl)?;
                let typer = self.get_type_of_value(*varr)?;

                if typel == typer {
                    Some(typel)
                } else {
                    None
                }
            }
            ValueExpr::FnCall {
                name, type_args, ..
            } => {
                let Some(StaticItem::Func {
                    return_type,
                    type_args: type_args_signature,
                    ..
                }) = self.static_items.get(name)
                else {
                    return None;
                };

                let mut return_type = return_type;

                if let (TypeExpr::Name(name), Some(type_args)) = (return_type, type_args) {
                    for (arg_sig, arg) in type_args_signature.iter().zip(type_args.iter()) {
                        if name == arg_sig {
                            return_type = arg;
                            break;
                        }
                    }
                }

                Some(return_type.clone())
            }
            ValueExpr::Variable(name) => self.find_value_from_scopes(name),
            _ => None,
        }
    }

    fn analyze_for_in(&mut self, expr_id: ExprId) -> Result<(), ()> {
        let Expr::ForIn {
            iter_item,
            iter,
            iter_body,
            remain_body,
        } = self.ast.get_expr(expr_id).clone()
        else {
            unreachable!();
        };

        let mut scope = HashMap::new();
        let mut is_paralable = false;
        let mut is_range_info_inserted = false;

        let VariableDefineExpr::One {
            name: iter_item_name,
            ..
        } = iter_item.clone()
        else {
            return Err(());
        };

        match self.ast.get_value_expr(iter) {
            ValueExpr::Range {
                start,
                end,
                is_inclusive,
            } => {
                scope.insert(iter_item_name.clone(), TypeExpr::Name("usize".into()));

                if let (ValueExpr::IntagerLiteral(start), ValueExpr::IntagerLiteral(end)) = (
                    self.ast.get_value_expr(*start),
                    self.ast.get_value_expr(*end),
                ) {
                    is_paralable = true;
                    is_range_info_inserted = true;

                    let mut end = *end;
                    if !*is_inclusive {
                        end -= 1
                    };

                    self.range_item_info.insert(
                        iter_item_name.clone(),
                        RangeItemInfo {
                            delta: 1,
                            min: *start.min(&end),
                            max: *start.max(&end),
                        },
                    );
                }
            }
            ValueExpr::Variable(var) => {
                let Some(TypeExpr::WithArgs(typename, typeargs)) = self.find_value_from_scopes(var)
                else {
                    return Err(());
                };
                if typename != "Vec" {
                    return Err(());
                };

                scope.insert(iter_item_name.clone(), typeargs[0].clone());
            }
            _ => return Err(()),
        };

        if self.goto_codeblock(iter_body, scope).is_err() {
            is_paralable = false;
        };

        if is_range_info_inserted {
            self.range_item_info.remove(&iter_item_name);
        }

        if is_paralable {
            #[cfg(debug_assertions)]
            {
                eprintln!("optimized for-in");
            }

            self.ast.exprs[expr_id.0] = Expr::ParalForIn {
                iter_item: iter_item.clone(),
                iter: iter.clone(),
                iter_body: iter_body.clone(),
                remain_body: remain_body.clone(),
            };
        };

        Ok(())
    }

    fn analyze_value_expr(&mut self, expr_id: ValueExprId) -> Result<(), ()> {
        let expr = self.ast.get_value_expr(expr_id).clone();

        match expr {
            ValueExpr::FnCall { name, args, .. } => {
                for arg in args {
                    self.analyze_value_expr(arg)?;
                }

                if self.static_items.get(&name).is_none() {
                    return Err(());
                };
            }
            ValueExpr::Variable(var) => {
                let scope_floor = none_to_err!(self.find_value_scope_floor(&var), ());
                let var_type = none_to_err!(self.find_value_from_scopes(&var), ());

                if scope_floor < self.scope_stack.len() - 1 {
                    if TypeExpr::Name("usize".into()) != var_type {
                        return Err(());
                    };
                }
            }
            ValueExpr::As { value, .. } => {
                self.analyze_value_expr(value)?;
            }
            ValueExpr::Indexing { index, .. } => {
                self.analyze_value_expr(index)?;
            }
            ValueExpr::Add(varl, varr)
            | ValueExpr::Sub(varl, varr)
            | ValueExpr::Mul(varl, varr)
            | ValueExpr::Div(varl, varr)
            | ValueExpr::GreaterThan(varl, varr)
            | ValueExpr::LessThan(varl, varr)
            | ValueExpr::BoolAnd(varl, varr)
            | ValueExpr::BoolOr(varl, varr) => {
                self.analyze_value_expr(varl)?;
                self.analyze_value_expr(varr)?;
            }
            _ => return Err(()),
        };

        Ok(())
    }

    fn analyze_expr(&mut self, expr_id: ExprId) -> Result<(), ()> {
        let expr = self.ast.get_expr(expr_id).clone();

        match expr {
            Expr::VariableLet { define_expr, value } | Expr::VariableVar { define_expr, value } => {
                let VariableDefineExpr::One {
                    type_,
                    name: var_name,
                    ..
                } = define_expr
                else {
                    #[cfg(debug_assertions)]
                    {
                        eprintln!("optimize failed var assign (var define expr)");
                    }
                    return Err(());
                };

                let var_type = if let Some(res) = type_ {
                    res.clone()
                } else if let Some(res) = self.get_type_of_value(value) {
                    res
                } else {
                    #[cfg(debug_assertions)]
                    {
                        eprintln!("optimize failed var assign (unknown type)");
                    }
                    return Err(());
                };

                self.scope_stack
                    .last_mut()
                    .unwrap()
                    .vars
                    .insert(var_name.clone(), var_type);
            }
            Expr::ValueExpr(expr) => {
                if self.analyze_value_expr(expr).is_err() {
                    #[cfg(debug_assertions)]
                    {
                        eprintln!("optimize failed value expr");
                    }
                    return Err(());
                };
            }
            Expr::EqualAssign { variable, value } => {
                if self.analyze_value_expr(value).is_err() {
                    #[cfg(debug_assertions)]
                    {
                        eprintln!("optimize failed equal assign value");
                    }
                    return Err(());
                };

                let var_ref = self.ast.get_value_expr(variable).clone();

                match var_ref {
                    ValueExpr::Indexing { index, .. } => {
                        if self.analyze_value_expr(index).is_err() {
                            #[cfg(debug_assertions)]
                            {
                                eprintln!("optimize failed indexing index");
                            }
                            return Err(());
                        };
                        if !self.is_chunkable(index) {
                            #[cfg(debug_assertions)]
                            {
                                eprintln!("optimize failed indexing chunking");
                            }
                            return Err(());
                        };
                    }
                    ValueExpr::Variable(..) => {
                        if self.analyze_value_expr(variable).is_err() {
                            #[cfg(debug_assertions)]
                            {
                                eprintln!("optimize failed variable");
                            }
                            return Err(());
                        };
                    }
                    _ => {
                        #[cfg(debug_assertions)]
                        {
                            eprintln!("optimize failed equal assign");
                        }
                        return Err(());
                    }
                }
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
                        name,
                        type_: Some(type_),
                        ..
                    } = arg
                    else {
                        continue;
                    };

                    scope.insert(name.clone(), type_.clone());
                }

                self.goto_codeblock(body, scope)?;

                self.static_items.insert(
                    name.clone(),
                    StaticItem::Func {
                        name: name.clone(),
                        return_type: return_type.clone(),
                        type_args: type_args.clone().unwrap_or(Vec::new()),
                    },
                );
            }
            Expr::While { condition, .. } => {
                if self.analyze_value_expr(condition).is_err() {
                    #[cfg(debug_assertions)]
                    {
                        eprintln!("optimize failed while condition");
                    }

                    return Err(());
                };
            }
            Expr::ForIn { .. } => {
                if self.analyze_for_in(expr_id).is_err() {
                    #[cfg(debug_assertions)]
                    {
                        eprintln!("optimize failed for in");
                    }

                    return Err(());
                };
            }
            _ => {}
        }

        Ok(())
    }

    fn _analyze_routine(&mut self, routine: &Vec<ExprId>) -> Result<(), ()> {
        for expr in routine {
            self.analyze_expr(*expr)?;
        }

        Ok(())
    }

    fn goto_scope(
        &mut self,
        routine: &Vec<ExprId>,
        scope: HashMap<String, TypeExpr>,
    ) -> Result<(), ()> {
        self.scope_stack.push(ScopeInfo {
            vars: scope,
            is_chunkable: true,
        });
        let res = self._analyze_routine(routine);
        self.scope_stack.pop();

        res
    }

    fn goto_codeblock(
        &mut self,
        codeblock_id: CodeBlockId,
        scope: HashMap<String, TypeExpr>,
    ) -> Result<(), ()> {
        self.goto_scope(&self.ast.get_code_block(codeblock_id).0.clone(), scope)
    }

    fn is_chunkable(&mut self, expr: ValueExprId) -> bool {
        self.try_const_folding(expr);

        self._is_affine_form(expr) && self._is_injective(expr)
    }

    fn _is_affine_form(&self, expr_id: ValueExprId) -> bool {
        let expr = self.ast.get_value_expr(expr_id).clone();

        match expr {
            ValueExpr::Add(varl, varr) | ValueExpr::Sub(varl, varr) => {
                self._is_affine_form(varl) && self._is_affine_form(varr)
            }
            ValueExpr::Mul(varl, varr) => {
                matches!(self.ast.get_value_expr(varr), ValueExpr::IntagerLiteral(..))
                    || matches!(self.ast.get_value_expr(varl), ValueExpr::IntagerLiteral(..))
            }
            ValueExpr::Div(.., varr) => {
                matches!(self.ast.get_value_expr(varr), ValueExpr::IntagerLiteral(..))
            }
            ValueExpr::Variable(name) => self.range_item_info.get(&name).is_some(),
            ValueExpr::IntagerLiteral(..) => true,
            _ => false,
        }
    }

    fn _is_injective(&self, expr: ValueExprId) -> bool {
        self._is_injective_inner(expr).is_some()
    }

    fn _concat_range(&self, r0: ValueExprId, r1: ValueExprId) -> Option<RangeItemInfo> {
        let ValueExpr::Variable(varl) = self.ast.get_value_expr(r0).clone() else {
            unreachable!();
        };
        let ValueExpr::Variable(varr) = self.ast.get_value_expr(r1).clone() else {
            unreachable!();
        };

        let varl_floor = self.find_value_scope_floor(&varl);
        let varr_floor = self.find_value_scope_floor(&varr);

        let (range_parent, range_child) = if varl_floor > varr_floor {
            (varr, varl)
        } else {
            (varl, varr)
        };

        let range_parent = self.range_item_info.get(&range_parent).unwrap();
        let range_child = self.range_item_info.get(&range_child).unwrap();

        if range_parent.delta.abs() as u64 > range_child.max.abs_diff(range_child.min) {
            let min = range_child.min + range_parent.min;
            let max = range_child.max + range_parent.max;

            return Some(RangeItemInfo { delta: 0, min, max });
        } else {
            return None;
        };
    }

    fn _is_injective_inner(&self, expr_id: ValueExprId) -> Option<RangeItemInfo> {
        let expr = self.ast.get_value_expr(expr_id).clone();

        match expr {
            ValueExpr::Add(varl, varr) => {
                let (num, valexpr) =
                    match (self.ast.get_value_expr(varl), self.ast.get_value_expr(varr)) {
                        (ValueExpr::IntagerLiteral(n), ..) => (n, varr),
                        (.., ValueExpr::IntagerLiteral(n)) => (n, varl),
                        _ => return self._concat_range(varl, varr),
                    };

                let mut range_info = self._is_injective_inner(valexpr)?;

                range_info.min += num;
                range_info.max += num;
                range_info.delta += num;

                Some(range_info)
            }
            ValueExpr::Sub(varl, varr) => {
                let (num, valexpr) =
                    match (self.ast.get_value_expr(varl), self.ast.get_value_expr(varr)) {
                        (ValueExpr::IntagerLiteral(n), ..) => (n, varr),
                        (.., ValueExpr::IntagerLiteral(n)) => (n, varl),
                        _ => return self._concat_range(varl, varr),
                    };

                let mut range_info = self._is_injective_inner(valexpr)?;

                range_info.min -= num;
                range_info.max -= num;
                range_info.delta -= num;

                Some(range_info)
            }
            ValueExpr::Mul(varl, varr) => {
                let (num, valexpr) =
                    match (self.ast.get_value_expr(varl), self.ast.get_value_expr(varr)) {
                        (ValueExpr::IntagerLiteral(n), ..) => (n, varr),
                        (.., ValueExpr::IntagerLiteral(n)) => (n, varl),
                        _ => return self._concat_range(varl, varr),
                    };

                let mut range_info = self._is_injective_inner(valexpr)?;

                if *num > 0 {
                    range_info.min *= num;
                    range_info.max *= num;
                    range_info.delta *= num;
                } else {
                    range_info.min = range_info.max * num;
                    range_info.max = range_info.min * num;
                    range_info.delta *= num;
                };

                return Some(range_info);
            }
            ValueExpr::Div(varl, varr) => {
                let (num, valexpr) =
                    match (self.ast.get_value_expr(varl), self.ast.get_value_expr(varr)) {
                        (ValueExpr::IntagerLiteral(n), ..) => (n, varr),
                        (.., ValueExpr::IntagerLiteral(n)) => (n, varl),
                        _ => return self._concat_range(varl, varr),
                    };

                let mut range_info = self._is_injective_inner(valexpr)?;

                if *num > 0 {
                    range_info.min /= num;
                    range_info.max /= num;
                    range_info.delta /= num;
                } else {
                    range_info.min = range_info.max / num;
                    range_info.max = range_info.min / num;
                    range_info.delta /= num;
                };

                return Some(range_info);
            }
            ValueExpr::Variable(name) => self.range_item_info.get(&name).map(|r| r.clone()),
            _ => None,
        }
    }

    fn try_const_folding(&mut self, expr_id: ValueExprId) {
        let expr = self.ast.get_value_expr(expr_id).clone();

        match expr {
            ValueExpr::Add(varl, varr) => {
                self.try_const_folding(varl);
                self.try_const_folding(varr);

                if let (ValueExpr::IntagerLiteral(varl), ValueExpr::IntagerLiteral(varr)) =
                    (self.ast.get_value_expr(varl), self.ast.get_value_expr(varr))
                {
                    self.ast.value_exprs[expr_id.0] = ValueExpr::IntagerLiteral(varl + varr);
                }
            }
            ValueExpr::Sub(varl, varr) => {
                self.try_const_folding(varl);
                self.try_const_folding(varr);

                if let (ValueExpr::IntagerLiteral(varl), ValueExpr::IntagerLiteral(varr)) =
                    (self.ast.get_value_expr(varl), self.ast.get_value_expr(varr))
                {
                    self.ast.value_exprs[expr_id.0] = ValueExpr::IntagerLiteral(varl - varr);
                }
            }
            ValueExpr::Mul(varl, varr) => {
                self.try_const_folding(varl);
                self.try_const_folding(varr);

                if let (ValueExpr::IntagerLiteral(varl), ValueExpr::IntagerLiteral(varr)) =
                    (self.ast.get_value_expr(varl), self.ast.get_value_expr(varr))
                {
                    self.ast.value_exprs[expr_id.0] = ValueExpr::IntagerLiteral(varl * varr);
                }
            }
            ValueExpr::Div(varl, varr) => {
                self.try_const_folding(varl);
                self.try_const_folding(varr);

                if let (ValueExpr::IntagerLiteral(varl), ValueExpr::IntagerLiteral(varr)) =
                    (self.ast.get_value_expr(varl), self.ast.get_value_expr(varr))
                {
                    self.ast.value_exprs[expr_id.0] = ValueExpr::IntagerLiteral(varl / varr);
                }
            }
            _ => {}
        }
    }
}
