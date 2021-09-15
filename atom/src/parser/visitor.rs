use super::ast::*;

/// Walks the tree calling `visit_{TYPE}` methods when `TYPE` is encountered
pub trait Visitor
where
    Self: Sized,
{
    type Error;

    fn visit_name(&mut self, _name: &str) -> Result<(), Self::Error> {
        Ok(())
    }

    fn visit_var(&mut self, _var: &Variable) -> Result<(), Self::Error> {
        Ok(())
    }

    fn visit_fn_arg(&mut self, _fn_arg: &FnArg) -> Result<(), Self::Error> {
        Ok(())
    }

    fn visit_import_stmt(&mut self, import_stmt: &ImportStmt) -> Result<(), Self::Error> {
        import_stmt.walk(self)
    }

    fn visit_block(&mut self, body: &[Stmt]) -> Result<(), Self::Error> {
        for stmt in body {
            self.visit_stmt(stmt)?;
        }

        Ok(())
    }

    fn visit_expr(&mut self, _expr: &Expr) -> Result<(), Self::Error> {
        Ok(())
    }

    fn visit_unsafe_stmt(&mut self, unsafe_stmt: &UnsafeStmt) -> Result<(), Self::Error> {
        unsafe_stmt.walk(self)
    }

    fn visit_module_stmt(&mut self, module_stmt: &ModuleStmt) -> Result<(), Self::Error> {
        module_stmt.walk(self)
    }

    fn visit_class_decl_stmt(
        &mut self,
        class_decl_stmt: &ClassDeclStmt,
    ) -> Result<(), Self::Error> {
        class_decl_stmt.walk(self)
    }

    fn visit_interface_decl_stmt(
        &mut self,
        interface_decl_stmt: &InterfaceDeclStmt,
    ) -> Result<(), Self::Error> {
        interface_decl_stmt.walk(self)
    }

    fn visit_mixin_decl_stmt(
        &mut self,
        mixin_decl_stmt: &MixinDeclStmt,
    ) -> Result<(), Self::Error> {
        mixin_decl_stmt.walk(self)
    }

    fn visit_expr_stmt(&mut self, expr_stmt: &ExprStmt) -> Result<(), Self::Error> {
        expr_stmt.walk(self)
    }

    fn visit_for_stmt(&mut self, for_stmt: &ForStmt) -> Result<(), Self::Error> {
        for_stmt.walk(self)
    }

    fn visit_let_stmt(&mut self, let_stmt: &LetStmt) -> Result<(), Self::Error> {
        let_stmt.walk(self)
    }

    fn visit_break_stmt(&mut self, break_stmt: &BreakStmt) -> Result<(), Self::Error> {
        break_stmt.walk(self)
    }

    fn visit_raise_stmt(&mut self, raise_stmt: &RaiseStmt) -> Result<(), Self::Error> {
        raise_stmt.walk(self)
    }

    fn visit_let_decl_stmt(&mut self, let_decl_stmt: &LetDeclStmt) -> Result<(), Self::Error> {
        let_decl_stmt.walk(self)
    }

    fn visit_fn_decl_stmt_list(&mut self, fn_decls: &[FnDeclStmt]) -> Result<(), Self::Error> {
        for fn_decl in fn_decls {
            self.visit_fn_decl_stmt(fn_decl)?;
        }

        Ok(())
    }

    fn visit_extern_fn_decl_stmt_list(
        &mut self,
        extern_fn_decls: &[ExternFnDeclStmt],
    ) -> Result<(), Self::Error> {
        for extern_fn_decl in extern_fn_decls {
            self.visit_extern_fn_decl_stmt(extern_fn_decl)?;
        }

        Ok(())
    }

    fn visit_fn_decl_stmt(&mut self, fn_decl_stmt: &FnDeclStmt) -> Result<(), Self::Error> {
        fn_decl_stmt.walk(self)
    }

    fn visit_extern_fn_decl_stmt(
        &mut self,
        extern_fn_decl_stmt: &ExternFnDeclStmt,
    ) -> Result<(), Self::Error> {
        extern_fn_decl_stmt.walk(self)
    }

    fn visit_assign_stmt(&mut self, assign_stmt: &AssignStmt) -> Result<(), Self::Error> {
        assign_stmt.walk(self)
    }

    fn visit_return_stmt(&mut self, return_stmt: &ReturnStmt) -> Result<(), Self::Error> {
        return_stmt.walk(self)
    }

    fn visit_if_stmt(&mut self, if_stmt: &IfStmt) -> Result<(), Self::Error> {
        if_stmt.walk(self)
    }

    fn visit_else_stmt(&mut self, else_stmt: &ElseStmt) -> Result<(), Self::Error> {
        else_stmt.walk(self)
    }

    fn visit_stmt(&mut self, stmt: &Stmt) -> Result<(), Self::Error> {
        stmt.walk(self)
    }
}

pub trait Visitable<E> {
    fn walk(&self, visitor: &mut impl Visitor<Error = E>) -> Result<(), E>;
}
