use crate::syntax::{
    ArithmeticExpr, ArrayExpr, AssignStmt, BreakStmt, CallExpr, CastExpr, ClassDeclStmt,
    ClosureExpr, ComparisonExpr, DerefExpr, ElseStmt, Expr, ExprStmt, Field, FnArg, FnDeclStmt,
    ForStmt, IdentExpr, IfStmt, ImportStmt, IndexExpr, InterfaceDeclStmt, KeyValue, KeywordArg,
    LetDeclStmt, LetStmt, LiteralExpr, LogicalExpr, MakeRefExpr, MapExpr, MatchCase, MatchStmt,
    MemberExpr, MixinDeclStmt, ModuleStmt, NewExpr, NotExpr, RaiseStmt, RangeExpr, ReturnStmt,
    Stmt, TemplateExpr, TryExpr, TupleExpr, TypeAssertExpr, TypeOfExpr, Variable,
};

pub trait Visitable<E> {
    fn accept(&mut self, visitor: &mut impl Visitor<Error = E>) -> Result<(), E>;
}

/// Implements a Visitor for the AST which allows rewriting the syntax tree
pub trait Visitor: Sized {
    type Error;

    fn visit_stmt_list(&mut self, stmt_list: &mut [Stmt]) -> Result<(), Self::Error> {
        for stmt in stmt_list.iter_mut() {
            self.visit_stmt(stmt)?;
        }

        Ok(())
    }

    fn visit_stmt(&mut self, stmt: &mut Stmt) -> Result<(), Self::Error> {
        stmt.accept(self)
    }

    fn visit_if_stmt(&mut self, if_stmt: &mut IfStmt) -> Result<(), Self::Error> {
        if_stmt.accept(self)
    }

    fn visit_else_stmt(&mut self, else_stmt: &mut ElseStmt) -> Result<(), Self::Error> {
        else_stmt.accept(self)
    }

    fn visit_match_case(&mut self, match_case: &mut MatchCase) -> Result<(), Self::Error> {
        match_case.accept(self)
    }

    fn visit_match_stmt(&mut self, match_stmt: &mut MatchStmt) -> Result<(), Self::Error> {
        match_stmt.accept(self)
    }

    fn visit_for_stmt(&mut self, for_stmt: &mut ForStmt) -> Result<(), Self::Error> {
        for_stmt.accept(self)
    }

    fn visit_expr(&mut self, expr: &mut Expr) -> Result<(), Self::Error> {
        expr.accept(self)
    }

    fn visit_expr_stmt(&mut self, expr_stmt: &mut ExprStmt) -> Result<(), Self::Error> {
        expr_stmt.accept(self)
    }

    fn visit_let_stmt(&mut self, let_stmt: &mut LetStmt) -> Result<(), Self::Error> {
        let_stmt.accept(self)
    }

    fn visit_break_stmt(&mut self, break_stmt: &mut BreakStmt) -> Result<(), Self::Error> {
        break_stmt.accept(self)
    }

    fn visit_raise_stmt(&mut self, raise_stmt: &mut RaiseStmt) -> Result<(), Self::Error> {
        raise_stmt.accept(self)
    }

    fn visit_let_decl_stmt(&mut self, let_decl_stmt: &mut LetDeclStmt) -> Result<(), Self::Error> {
        let_decl_stmt.accept(self)
    }

    fn visit_fn_arg(&mut self, fn_arg: &mut FnArg) -> Result<(), Self::Error> {
        fn_arg.accept(self)
    }

    fn visit_fn_decl_stmt(&mut self, fn_decl_stmt: &mut FnDeclStmt) -> Result<(), Self::Error> {
        fn_decl_stmt.accept(self)
    }

    fn visit_assign_stmt(&mut self, assign_stmt: &mut AssignStmt) -> Result<(), Self::Error> {
        assign_stmt.accept(self)
    }

    fn visit_return_stmt(&mut self, return_stmt: &mut ReturnStmt) -> Result<(), Self::Error> {
        return_stmt.accept(self)
    }

    fn visit_import_stmt(&mut self, import_stmt: &mut ImportStmt) -> Result<(), Self::Error> {
        import_stmt.accept(self)
    }

    fn visit_module_stmt(&mut self, module_stmt: &mut ModuleStmt) -> Result<(), Self::Error> {
        module_stmt.accept(self)
    }

    fn visit_class_field(&mut self, field: &mut Field) -> Result<(), Self::Error> {
        field.accept(self)
    }

    fn visit_class_decl_stmt(
        &mut self,
        class_decl_stmt: &mut ClassDeclStmt,
    ) -> Result<(), Self::Error> {
        class_decl_stmt.accept(self)
    }

    fn visit_mixin_decl_stmt(
        &mut self,
        mixin_decl_stmt: &mut MixinDeclStmt,
    ) -> Result<(), Self::Error> {
        mixin_decl_stmt.accept(self)
    }

    fn visit_interface_decl_stmt(
        &mut self,
        interface_decl_stmt: &mut InterfaceDeclStmt,
    ) -> Result<(), Self::Error> {
        interface_decl_stmt.accept(self)
    }

    fn visit_name(&mut self, _name: &mut String) -> Result<(), Self::Error> {
        Ok(())
    }

    fn visit_var(&mut self, var: &mut Variable) -> Result<(), Self::Error> {
        var.accept(self)
    }

    fn visit_literal_expr(&mut self, literal_expr: &mut LiteralExpr) -> Result<(), Self::Error> {
        literal_expr.accept(self)
    }

    fn visit_ident_expr(&mut self, ident_expr: &mut IdentExpr) -> Result<(), Self::Error> {
        ident_expr.accept(self)
    }

    fn visit_keyword_arg(&mut self, arg: &mut KeywordArg) -> Result<(), Self::Error> {
        arg.accept(self)
    }

    fn visit_new_expr(&mut self, new_expr: &mut NewExpr) -> Result<(), Self::Error> {
        new_expr.accept(self)
    }

    fn visit_call_expr(&mut self, call_expr: &mut CallExpr) -> Result<(), Self::Error> {
        call_expr.accept(self)
    }

    fn visit_cast_expr(&mut self, cast_expr: &mut CastExpr) -> Result<(), Self::Error> {
        cast_expr.accept(self)
    }

    fn visit_not_expr(&mut self, not_expr: &mut NotExpr) -> Result<(), Self::Error> {
        not_expr.accept(self)
    }

    fn visit_try_expr(&mut self, try_expr: &mut TryExpr) -> Result<(), Self::Error> {
        try_expr.accept(self)
    }

    fn visit_array_expr(&mut self, array_expr: &mut ArrayExpr) -> Result<(), Self::Error> {
        array_expr.accept(self)
    }

    fn visit_tuple_expr(&mut self, tuple_expr: &mut TupleExpr) -> Result<(), Self::Error> {
        tuple_expr.accept(self)
    }

    fn visit_key_value(&mut self, key_value: &mut KeyValue) -> Result<(), Self::Error> {
        key_value.accept(self)
    }

    fn visit_map_expr(&mut self, map_expr: &mut MapExpr) -> Result<(), Self::Error> {
        map_expr.accept(self)
    }

    fn visit_typeof_expr(&mut self, typeof_expr: &mut TypeOfExpr) -> Result<(), Self::Error> {
        typeof_expr.accept(self)
    }

    fn visit_closure_expr(&mut self, closure_expr: &mut ClosureExpr) -> Result<(), Self::Error> {
        closure_expr.accept(self)
    }

    fn visit_member_expr(&mut self, member_expr: &mut MemberExpr) -> Result<(), Self::Error> {
        member_expr.accept(self)
    }

    fn visit_arithmetic_expr(
        &mut self,
        arithmetic_expr: &mut ArithmeticExpr,
    ) -> Result<(), Self::Error> {
        arithmetic_expr.accept(self)
    }

    fn visit_comparison_expr(
        &mut self,
        comparison_expr: &mut ComparisonExpr,
    ) -> Result<(), Self::Error> {
        comparison_expr.accept(self)
    }

    fn visit_logical_expr(&mut self, logical_expr: &mut LogicalExpr) -> Result<(), Self::Error> {
        logical_expr.accept(self)
    }

    fn visit_make_ref_expr(&mut self, make_ref_expr: &mut MakeRefExpr) -> Result<(), Self::Error> {
        make_ref_expr.accept(self)
    }

    fn visit_deref_expr(&mut self, deref_expr: &mut DerefExpr) -> Result<(), Self::Error> {
        deref_expr.accept(self)
    }

    fn visit_index_expr(&mut self, index_expr: &mut IndexExpr) -> Result<(), Self::Error> {
        index_expr.accept(self)
    }

    fn visit_range_expr(&mut self, range_expr: &mut RangeExpr) -> Result<(), Self::Error> {
        range_expr.accept(self)
    }

    fn visit_template_expr(&mut self, template_expr: &mut TemplateExpr) -> Result<(), Self::Error> {
        template_expr.accept(self)
    }

    fn visit_type_assert_expr(
        &mut self,
        type_assert_expr: &mut TypeAssertExpr,
    ) -> Result<(), Self::Error> {
        type_assert_expr.accept(self)
    }
}
