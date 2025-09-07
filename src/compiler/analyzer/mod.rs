use std::{mem::take, sync::Arc};

use ahash::{HashMap, HashMapExt};
use slotmap::SlotMap;

use crate::{
    compiler::{codegen::ToRust, exprs::*, parser::SyntaxTree},
    none_to_err,
};

enum ExprAnalyzeErr {
    ExprErr,
    NonPureScope,
}

use ExprAnalyzeErr::*;

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
    delta: ValueExprKey,
    min: ValueExprKey,
    max: ValueExprKey,
    scope_depth: usize,
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
    tmp_value_exprs: SlotMap<ValueExprKey, ValueExpr>,
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
            tmp_value_exprs: SlotMap::with_key(),
        }
    }

    pub fn analyze(&mut self) -> SyntaxTree {
        self._analyze_routine(&self.ast.main_routine.clone());

        take(&mut self.ast)
    }

    // ---

    fn find_value_from_scopes(&self, name: &String) -> Option<TypeExpr> {
        for scope in self.scope_stack.iter().rev() {
            if let Some(res) = scope.vars.get(name) {
                return Some(res.clone());
            }
        }

        return None;
    }

    fn find_value_scope_depth(&self, name: &String) -> Option<usize> {
        for (i, scope) in self.scope_stack.iter().enumerate().rev() {
            if matches!(scope.vars.get(name), Some(..)) {
                return Some(i);
            }
        }

        return None;
    }

    fn get_type_of_value(&self, expr_id: ValueExprKey) -> Option<TypeExpr> {
        let expr = expr_id.get(&self.ast.value_exprs);

        match expr {
            ValueExpr::Add(varl, varr) => {
                let typel = self.get_type_of_value(varl)?;
                let typer = self.get_type_of_value(varr)?;

                if typel == typer {
                    Some(typel)
                } else {
                    None
                }
            }
            ValueExpr::Sub(varl, varr) => {
                let typel = self.get_type_of_value(varl)?;
                let typer = self.get_type_of_value(varr)?;

                if typel == typer {
                    Some(typel)
                } else {
                    None
                }
            }
            ValueExpr::Mul(varl, varr) => {
                let typel = self.get_type_of_value(varl)?;
                let typer = self.get_type_of_value(varr)?;

                if typel == typer {
                    Some(typel)
                } else {
                    None
                }
            }
            ValueExpr::Div(varl, varr) => {
                let typel = self.get_type_of_value(varl)?;
                let typer = self.get_type_of_value(varr)?;

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
                }) = self.static_items.get(&name)
                else {
                    return None;
                };

                let mut return_type = return_type.clone();

                if let (TypeExpr::Name(name), Some(type_args)) = (return_type.clone(), type_args) {
                    let type_args = type_args.clone();

                    for (arg_sig, arg) in type_args_signature.iter().zip(type_args.into_iter()) {
                        if name == *arg_sig {
                            return_type = arg;
                            break;
                        }
                    }
                }

                Some(return_type.clone())
            }
            ValueExpr::Variable(name) => self.find_value_from_scopes(&name),
            _ => None,
        }
    }

    fn analyze_for_in(&mut self, expr_id: ExprKey) -> Result<(), ()> {
        let Expr::ForIn {
            iter_item,
            iter,
            iter_body,
            remain_body,
        } = expr_id.get(&self.ast.exprs)
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

        match iter.get(&self.ast.value_exprs) {
            ValueExpr::Range {
                start,
                end,
                is_inclusive,
            } => {
                scope.insert(iter_item_name.clone(), TypeExpr::Name("usize".into()));

                is_paralable = true;
                is_range_info_inserted = true;

                let var_expr_l = start.get(&self.ast.value_exprs);
                let var_expr_r = end.get(&self.ast.value_exprs);

                let min = self.tmp_value_exprs.insert(var_expr_l.clone());
                let mut max = self.tmp_value_exprs.insert(var_expr_r.clone());
                let delta = self.tmp_value_exprs.insert(ValueExpr::IntagerLiteral(1));

                if !is_inclusive {
                    let tmp = self.tmp_value_exprs.insert(ValueExpr::IntagerLiteral(1));
                    max = self.tmp_value_exprs.insert(ValueExpr::Sub(max, tmp));
                };

                self.range_item_info.insert(
                    iter_item_name.clone(),
                    RangeItemInfo {
                        delta,
                        min,
                        max,
                        scope_depth: self.scope_stack.len(),
                    },
                );
            }
            ValueExpr::Reference { value, .. } => match value.get(&self.ast.value_exprs) {
                ValueExpr::Variable(var) => {
                    let Some(TypeExpr::WithArgs(typename, typeargs)) =
                        self.find_value_from_scopes(&var)
                    else {
                        return Err(());
                    };
                    if typename != "Vec" {
                        return Err(());
                    };

                    is_paralable = true;
                    scope.insert(iter_item_name.clone(), typeargs[0].clone());
                }
                _ => return Err(()),
            },
            ValueExpr::Variable(var) => {
                let Some(TypeExpr::WithArgs(typename, typeargs)) =
                    self.find_value_from_scopes(&var)
                else {
                    return Err(());
                };
                if typename != "Vec" {
                    return Err(());
                };

                is_paralable = true;
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

        if !is_paralable {
            return Err(());
        };

        #[cfg(debug_assertions)]
        {
            eprintln!("optimized for-in");
        }

        self.ast.exprs[expr_id] = Expr::ParalForIn {
            iter_item: iter_item.clone(),
            iter: iter.clone(),
            iter_body: iter_body.clone(),
            remain_body: remain_body.clone(),
        };

        self.ast.is_threading_used = true;

        Ok(())
    }

    fn analyze_value_expr(&mut self, expr_id: ValueExprKey) -> Result<(), ExprAnalyzeErr> {
        let expr = expr_id.get(&self.ast.value_exprs);

        match expr {
            ValueExpr::FnCall { name, args, .. } => {
                for arg in args {
                    self.analyze_value_expr(arg)?;
                }

                if self.static_items.get(&name).is_none() {
                    return Err(ExprErr);
                };
            }
            ValueExpr::Variable(var) => {
                let scope_floor = none_to_err!(self.find_value_scope_depth(&var), ExprErr);
                let var_type = none_to_err!(self.find_value_from_scopes(&var), ExprErr);

                if scope_floor < self.scope_stack.len() - 1 {
                    if TypeExpr::Name("usize".into()) != var_type {
                        return Err(ExprErr);
                    };
                }
            }
            ValueExpr::Reference { value, .. }
            | ValueExpr::Dereference(value)
            | ValueExpr::GroupingParen(value)
            | ValueExpr::As { value, .. }
            | ValueExpr::Indexing { index: value, .. } => {
                self.analyze_value_expr(value)?;
            }
            ValueExpr::Range { .. } => {}
            ValueExpr::IntagerLiteral(..) => {}
            ValueExpr::FloatLiteral(..) => {}
            ValueExpr::Add(varl, varr)
            | ValueExpr::Sub(varl, varr)
            | ValueExpr::Mul(varl, varr)
            | ValueExpr::Div(varl, varr)
            | ValueExpr::Remainder(varl, varr)
            | ValueExpr::GreaterThan(varl, varr)
            | ValueExpr::LessThan(varl, varr)
            | ValueExpr::BoolAnd(varl, varr)
            | ValueExpr::BoolOr(varl, varr) => {
                self.analyze_value_expr(varl)?;
                self.analyze_value_expr(varr)?;
            }
            _ => {
                return Err(ExprErr);
            }
        };

        Ok(())
    }

    fn analyze_expr(&mut self, expr_id: ExprKey) -> Result<(), ExprAnalyzeErr> {
        let expr = expr_id.get(&self.ast.exprs);

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
                    return Err(ExprErr);
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
                    return Err(ExprErr);
                };

                self.scope_stack
                    .last_mut()
                    .unwrap()
                    .vars
                    .insert(var_name.clone(), var_type);
            }
            Expr::ValueExpr(expr) => {
                self.analyze_value_expr(expr).inspect_err(|e| {
                    #[cfg(debug_assertions)]
                    {
                        eprintln!("optimize failed value expr");
                    }
                })?;
            }
            Expr::AddAssign { variable, value } => {}
            Expr::EqualAssign { variable, value } => {
                self.analyze_value_expr(value).inspect_err(|e| {
                    #[cfg(debug_assertions)]
                    {
                        eprintln!("optimize failed equal assign value");
                    }
                })?;

                let var_ref = variable.get(&self.ast.value_exprs);

                match var_ref {
                    ValueExpr::Indexing { index, .. } => {
                        self.analyze_value_expr(index).inspect_err(|e| {
                            #[cfg(debug_assertions)]
                            {
                                eprintln!("optimize failed indexing index");
                            }
                        })?;

                        if !self.is_chunkable(index) {
                            #[cfg(debug_assertions)]
                            {
                                eprintln!("optimize failed indexing chunking");
                            }
                            return Err(ExprErr);
                        };
                    }
                    ValueExpr::Variable(..) => {
                        self.analyze_value_expr(variable).inspect_err(|e| {
                            #[cfg(debug_assertions)]
                            {
                                eprintln!("optimize failed equal assign value");
                            }
                        })?;
                    }
                    _ => {
                        #[cfg(debug_assertions)]
                        {
                            eprintln!("optimize failed equal assign");
                        }
                        return Err(ExprErr);
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

                if let Err(NonPureScope) = self.goto_codeblock(body, scope) {
                    return Err(ExprErr);
                };

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
                self.analyze_value_expr(condition).inspect_err(|e| {
                    #[cfg(debug_assertions)]
                    {
                        eprintln!("optimize failed while condition");
                    }
                })?;
            }
            Expr::ForIn { .. } => {
                if self
                    .analyze_for_in(expr_id)
                    .inspect_err(|e| {
                        #[cfg(debug_assertions)]
                        {
                            eprintln!("optimize failed for in");
                        }
                    })
                    .is_err()
                {
                    return Err(ExprErr);
                };
            }
            Expr::Return(var) => self.analyze_value_expr(var)?,
            _ => {}
        }

        Ok(())
    }

    fn _analyze_routine(&mut self, routine: &Vec<ExprKey>) -> Result<(), ExprAnalyzeErr> {
        let mut is_erred = false;

        for expr in routine {
            let expr_res = self.analyze_expr(*expr);

            if let Err(res_err) = expr_res {
                match res_err {
                    NonPureScope => {
                        return Err(NonPureScope);
                    }
                    ExprErr => {
                        is_erred = true;
                    }
                }
            };
        }

        if is_erred {
            Err(ExprErr)
        } else {
            Ok(())
        }
    }

    fn goto_scope(
        &mut self,
        routine: &Vec<ExprKey>,
        scope: HashMap<String, TypeExpr>,
    ) -> Result<(), ExprAnalyzeErr> {
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
        codeblock_id: CodeBlockKey,
        scope: HashMap<String, TypeExpr>,
    ) -> Result<(), ExprAnalyzeErr> {
        self.goto_scope(&codeblock_id.get(&self.ast.code_blocks).0, scope)
    }

    fn is_chunkable(&mut self, expr: ValueExprKey) -> bool {
        let Some(expr) = self.dup_to_tmp(expr) else {
            return false;
        };

        self.try_const_folding(expr);

        self._is_affine_form(expr) && self._is_injective(expr)
    }

    fn dup_to_tmp(&mut self, expr_id: ValueExprKey) -> Option<ValueExprKey> {
        let expr = expr_id.get(&self.ast.value_exprs);

        Some(match expr {
            ValueExpr::Add(varl, varr) => {
                let tmp0 = self.dup_to_tmp(varl)?;
                let tmp1 = self.dup_to_tmp(varr)?;
                self.tmp_value_exprs.insert(ValueExpr::Add(tmp0, tmp1))
            }
            ValueExpr::Sub(varl, varr) => {
                let tmp0 = self.dup_to_tmp(varl)?;
                let tmp1 = self.dup_to_tmp(varr)?;
                self.tmp_value_exprs.insert(ValueExpr::Sub(tmp0, tmp1))
            }
            ValueExpr::Mul(varl, varr) => {
                let tmp0 = self.dup_to_tmp(varl)?;
                let tmp1 = self.dup_to_tmp(varr)?;
                self.tmp_value_exprs.insert(ValueExpr::Mul(tmp0, tmp1))
            }
            ValueExpr::Div(varl, varr) => {
                let tmp0 = self.dup_to_tmp(varl)?;
                let tmp1 = self.dup_to_tmp(varr)?;
                self.tmp_value_exprs.insert(ValueExpr::Div(tmp0, tmp1))
            }
            ValueExpr::Variable(var) => self.tmp_var(var.as_str()),
            ValueExpr::IntagerLiteral(int) => self.tmp_int_lit(int),
            _ => return None,
        })
    }

    fn _is_affine_form(&self, expr_id: ValueExprKey) -> bool {
        let expr = expr_id.get(&self.tmp_value_exprs);

        match expr {
            ValueExpr::Add(varl, varr) | ValueExpr::Sub(varl, varr) => {
                self._is_affine_form(varl) && self._is_affine_form(varr)
            }
            ValueExpr::Mul(varl, varr) => {
                match varl.get(&self.tmp_value_exprs) {
                    ValueExpr::IntagerLiteral(..) => {
                        return true;
                    }
                    ValueExpr::Variable(var) => {
                        if self.range_item_info.get(&var).is_none() {
                            return true;
                        }
                    }
                    _ => {}
                };

                match varr.get(&self.tmp_value_exprs) {
                    ValueExpr::IntagerLiteral(..) => {
                        return true;
                    }
                    ValueExpr::Variable(var) => {
                        if self.range_item_info.get(&var).is_none() {
                            return true;
                        }
                    }
                    _ => {}
                };

                return false;
            }
            ValueExpr::Div(.., varr) => {
                matches!(
                    varr.get(&self.tmp_value_exprs),
                    ValueExpr::IntagerLiteral(..)
                )
            }
            ValueExpr::Variable(..) => true,
            ValueExpr::IntagerLiteral(..) => true,
            _ => false,
        }
    }

    fn _is_injective(&mut self, expr: ValueExprKey) -> bool {
        self._is_injective_inner(expr).is_some()
    }

    fn _concat_range(
        &mut self,
        range0: RangeItemInfo,
        range1: RangeItemInfo,
    ) -> Option<RangeItemInfo> {
        let (range_parent, range_child) = if range0.scope_depth > range1.scope_depth {
            (range1, range0)
        } else {
            (range0, range1)
        };

        let tmp = self.tmp_int_lit(1);
        let tmp = self
            .tmp_value_exprs
            .insert(ValueExpr::Sub(range_parent.delta, tmp));

        self.try_const_folding(tmp);
        self.try_const_folding(range_child.max);

        if !tmp.is_same_value(range_child.max, &self.tmp_value_exprs) {
            return None;
        };

        let delta = self.tmp_value_exprs.insert(ValueExpr::IntagerLiteral(0));
        let min = self
            .tmp_value_exprs
            .insert(ValueExpr::Add(range_child.min, range_parent.min));
        let max = self
            .tmp_value_exprs
            .insert(ValueExpr::Add(range_child.max, range_parent.max));

        return Some(RangeItemInfo {
            delta,
            min,
            max,
            scope_depth: 0,
        });
    }

    fn tmp_int_lit(&mut self, n: i64) -> ValueExprKey {
        self.tmp_value_exprs.insert(ValueExpr::IntagerLiteral(n))
    }

    fn tmp_var(&mut self, name: &str) -> ValueExprKey {
        self.tmp_value_exprs
            .insert(ValueExpr::Variable(name.to_string()))
    }

    fn get_var_name(&mut self, expr_id: ValueExprKey) -> Option<String> {
        let var_expr = expr_id.get(&self.tmp_value_exprs);

        let ValueExpr::Variable(name) = var_expr else {
            return None;
        };

        Some(name)
    }

    fn both_range_item(
        &mut self,
        expr0: ValueExprKey,
        expr1: ValueExprKey,
    ) -> Option<(RangeItemInfo, RangeItemInfo)> {
        match (
            self._is_injective_inner(expr0),
            self._is_injective_inner(expr1),
        ) {
            (Some(range_info0), Some(ranege_info1)) => Some((range_info0, ranege_info1)),
            _ => None,
        }
    }

    fn const_and_range_item(
        &mut self,
        expr0: ValueExprKey,
        expr1: ValueExprKey,
    ) -> Option<(RangeItemInfo, i64, u8)> {
        let (const_val, var_expr, which_is_range) = match (
            expr0.get(&self.tmp_value_exprs),
            expr1.get(&self.tmp_value_exprs),
        ) {
            (ValueExpr::IntagerLiteral(n), ..) => (n, expr1, 1),
            (.., ValueExpr::IntagerLiteral(n)) => (n, expr0, 0),
            _ => return None,
        };

        let range_info = self._is_injective_inner(var_expr)?;

        Some((range_info, const_val, which_is_range))
    }

    fn val_expr_and_range_item(
        &mut self,
        expr0: ValueExprKey,
        expr1: ValueExprKey,
    ) -> Option<(RangeItemInfo, String, u8)> {
        let (Some(varl_var_name), Some(varr_var_name)) =
            (self.get_var_name(expr0), self.get_var_name(expr1))
        else {
            return None;
        };

        match (
            self._is_injective_inner(expr0),
            self._is_injective_inner(expr1),
        ) {
            (Some(range_info), None) => Some((range_info.clone(), varr_var_name, 0)),
            (None, Some(range_info)) => Some((range_info.clone(), varl_var_name, 1)),
            _ => None,
        }
    }

    fn _is_injective_inner(&mut self, expr_id: ValueExprKey) -> Option<RangeItemInfo> {
        let expr = expr_id.get(&self.tmp_value_exprs);

        match expr {
            ValueExpr::Add(varl, varr) => {
                if let Some((rangel, ranger)) = self.both_range_item(varl, varr) {
                    return self._concat_range(rangel, ranger);
                };

                if let Some((mut range_info, num_var_name, ..)) =
                    self.val_expr_and_range_item(varl, varr)
                {
                    range_info.min = {
                        let tmp0 = self.tmp_var(&num_var_name);
                        self.tmp_value_exprs
                            .insert(ValueExpr::Add(range_info.min, tmp0))
                    };
                    range_info.max = {
                        let tmp0 = self.tmp_var(&num_var_name);
                        self.tmp_value_exprs
                            .insert(ValueExpr::Add(range_info.max, tmp0))
                    };

                    return Some(range_info);
                };

                if let Some((mut range_info, const_num, ..)) = self.const_and_range_item(varl, varr)
                {
                    range_info.min = {
                        let tmp0 = self
                            .tmp_value_exprs
                            .insert(ValueExpr::IntagerLiteral(const_num));
                        self.tmp_value_exprs
                            .insert(ValueExpr::Add(tmp0, range_info.min))
                    };
                    range_info.max = {
                        let tmp0 = self
                            .tmp_value_exprs
                            .insert(ValueExpr::IntagerLiteral(const_num));
                        self.tmp_value_exprs
                            .insert(ValueExpr::Add(tmp0, range_info.max))
                    };

                    return Some(range_info);
                };

                None
            }
            ValueExpr::Sub(varl, varr) => {
                todo!();
            }
            ValueExpr::Mul(varl, varr) => {
                if self.both_range_item(varl, varr).is_some() {
                    return None;
                };

                if let Some((mut range_info, num_var_name, _)) =
                    self.val_expr_and_range_item(varl, varr)
                {
                    range_info.min = {
                        let tmp0 = self.tmp_var(&num_var_name);
                        self.tmp_value_exprs
                            .insert(ValueExpr::Add(range_info.min, tmp0))
                    };
                    range_info.max = {
                        let tmp0 = self.tmp_var(&num_var_name);
                        self.tmp_value_exprs
                            .insert(ValueExpr::Add(range_info.max, tmp0))
                    };
                    range_info.delta = {
                        let tmp0 = self.tmp_var(&num_var_name);
                        self.tmp_value_exprs
                            .insert(ValueExpr::Mul(range_info.delta, tmp0))
                    };

                    return Some(range_info);
                };

                if let Some((mut range_info, const_num, ..)) = self.const_and_range_item(varl, varr)
                {
                    range_info.min = {
                        let tmp0 = self
                            .tmp_value_exprs
                            .insert(ValueExpr::IntagerLiteral(const_num));
                        self.tmp_value_exprs
                            .insert(ValueExpr::Mul(tmp0, range_info.min))
                    };
                    range_info.max = {
                        let tmp0 = self
                            .tmp_value_exprs
                            .insert(ValueExpr::IntagerLiteral(const_num));
                        self.tmp_value_exprs
                            .insert(ValueExpr::Mul(tmp0, range_info.max))
                    };
                    range_info.delta = {
                        let tmp0 = self
                            .tmp_value_exprs
                            .insert(ValueExpr::IntagerLiteral(const_num));
                        self.tmp_value_exprs
                            .insert(ValueExpr::Mul(range_info.delta, tmp0))
                    };

                    return Some(range_info);
                };

                None
            }
            ValueExpr::Div(varl, varr) => {
                todo!();
            }
            ValueExpr::Variable(name) => self.range_item_info.get(&name).cloned(),
            _ => None,
        }
    }

    fn try_const_folding(&mut self, expr_id: ValueExprKey) {
        let expr = expr_id.get(&self.tmp_value_exprs);

        match expr {
            ValueExpr::Add(varl, varr) => {
                self.try_const_folding(varl);
                self.try_const_folding(varr);

                if let (ValueExpr::IntagerLiteral(varl), ValueExpr::IntagerLiteral(varr)) = (
                    varl.get(&self.tmp_value_exprs),
                    varr.get(&self.tmp_value_exprs),
                ) {
                    self.tmp_value_exprs[expr_id] = ValueExpr::IntagerLiteral(varl + varr);
                }
            }
            ValueExpr::Sub(varl, varr) => {
                self.try_const_folding(varl);
                self.try_const_folding(varr);

                if let (ValueExpr::IntagerLiteral(varl), ValueExpr::IntagerLiteral(varr)) = (
                    varl.get(&self.tmp_value_exprs),
                    varr.get(&self.tmp_value_exprs),
                ) {
                    self.tmp_value_exprs[expr_id] = ValueExpr::IntagerLiteral(varl - varr);
                }
            }
            ValueExpr::Mul(varl, varr) => {
                self.try_const_folding(varl);
                self.try_const_folding(varr);

                match (
                    varl.get(&self.tmp_value_exprs),
                    varr.get(&self.tmp_value_exprs),
                ) {
                    (ValueExpr::IntagerLiteral(varl), ValueExpr::IntagerLiteral(varr)) => {
                        self.tmp_value_exprs[expr_id] = ValueExpr::IntagerLiteral(varl * varr);
                    }
                    (ValueExpr::IntagerLiteral(1), other)
                    | (other, ValueExpr::IntagerLiteral(1)) => {
                        eprintln!("{}", other);

                        self.tmp_value_exprs[expr_id] = other;
                    }
                    (ValueExpr::IntagerLiteral(0), _) | (_, ValueExpr::IntagerLiteral(0)) => {
                        self.tmp_value_exprs[expr_id] = ValueExpr::IntagerLiteral(0);
                    }
                    _ => {}
                }
            }
            ValueExpr::Div(varl, varr) => {
                self.try_const_folding(varl);
                self.try_const_folding(varr);

                if let (ValueExpr::IntagerLiteral(varl), ValueExpr::IntagerLiteral(varr)) = (
                    varl.get(&self.tmp_value_exprs),
                    varr.get(&self.tmp_value_exprs),
                ) {
                    self.tmp_value_exprs[expr_id] = ValueExpr::IntagerLiteral(varl / varr);
                }
            }
            _ => {}
        }
    }
}
