use super::ast::*;

pub trait Folder {
    fn fold_name(&mut self, name: String) -> String {
        name
    }

    fn fold_expr(&mut self, expr: Expr) -> Expr {
        expr
    }

    fn fold_var(&mut self, var: Variable) -> Variable {
        var
    }

    fn fold_fn_arg(&mut self, fn_arg: FnArg) -> FnArg {
        fn_arg
    }

    fn fold_if_stmt(&mut self, if_stmt: IfStmt) -> IfStmt {
        IfStmt {
            cond: self.fold_expr(if_stmt.cond),
            alt: if let Some(stmt) = if_stmt.alt {
                Some(self.fold_stmt(*stmt).into())
            } else {
                None
            },
            body: self.fold_stmt_list(if_stmt.body),
            pos: if_stmt.pos,
        }
    }

    fn fold_else_stmt(&mut self, else_stmt: ElseStmt) -> ElseStmt {
        ElseStmt {
            body: self.fold_stmt_list(else_stmt.body),
            pos: else_stmt.pos,
        }
    }

    fn fold_for_stmt(&mut self, for_stmt: ForStmt) -> ForStmt {
        ForStmt {
            expr: for_stmt.expr.map(|expr| self.fold_expr(expr)),
            alias: for_stmt.alias.map(|var| self.fold_var(var)),
            body: self.fold_stmt_list(for_stmt.body),
            pos: for_stmt.pos,
        }
    }

    fn fold_expr_stmt(&mut self, expr_stmt: ExprStmt) -> ExprStmt {
        ExprStmt {
            expr: self.fold_expr(expr_stmt.expr),
            pos: expr_stmt.pos,
        }
    }

    fn fold_let_stmt(&mut self, let_stmt: LetStmt) -> LetStmt {
        LetStmt {
            var: self.fold_var(let_stmt.var),
            value: self.fold_expr(let_stmt.value),
            mutable: let_stmt.mutable,
            pos: let_stmt.pos,
        }
    }

    fn fold_break_stmt(&mut self, break_stmt: BreakStmt) -> BreakStmt {
        BreakStmt {
            label: break_stmt.label,
            pos: break_stmt.pos,
        }
    }

    fn fold_raise_stmt(&mut self, raise_stmt: RaiseStmt) -> RaiseStmt {
        RaiseStmt {
            expr: self.fold_expr(raise_stmt.expr),
            pos: raise_stmt.pos,
        }
    }

    fn fold_let_decl_stmt(&mut self, let_decl_stmt: LetDeclStmt) -> LetDeclStmt {
        LetDeclStmt {
            name: self.fold_name(let_decl_stmt.name),
            pos: let_decl_stmt.pos,
        }
    }

    fn fold_fn_decl_stmt(&mut self, fn_decl_stmt: FnDeclStmt) -> FnDeclStmt {
        FnDeclStmt {
            name: self.fold_name(fn_decl_stmt.name),
            public: fn_decl_stmt.public,
            args: fn_decl_stmt
                .args
                .into_iter()
                .map(|fn_arg| self.fold_fn_arg(fn_arg))
                .collect(),
            body: self.fold_stmt_list(fn_decl_stmt.body),
            comments: self.fold_comment_list(fn_decl_stmt.comments),
            pos: Default::default(),
        }
    }

    fn fold_fn_decl_stmt_list(&mut self, fn_decl_stmt_list: Vec<FnDeclStmt>) -> Vec<FnDeclStmt> {
        fn_decl_stmt_list
            .into_iter()
            .map(|fn_decl_stmt| self.fold_fn_decl_stmt(fn_decl_stmt))
            .collect()
    }

    fn fold_extern_fn_decl_stmt(
        &mut self,
        extern_fn_decl_stmt: ExternFnDeclStmt,
    ) -> ExternFnDeclStmt {
        ExternFnDeclStmt {
            name: self.fold_name(extern_fn_decl_stmt.name),
            public: extern_fn_decl_stmt.public,
            args: extern_fn_decl_stmt
                .args
                .into_iter()
                .map(|fn_arg| self.fold_fn_arg(fn_arg))
                .collect(),
            comments: self.fold_comment_list(extern_fn_decl_stmt.comments),
            pos: extern_fn_decl_stmt.pos,
        }
    }

    fn fold_extern_fn_decl_stmt_list(
        &mut self,
        extern_fn_decl_stmt_list: Vec<ExternFnDeclStmt>,
    ) -> Vec<ExternFnDeclStmt> {
        extern_fn_decl_stmt_list
            .into_iter()
            .map(|extern_fn_decl_stmt| self.fold_extern_fn_decl_stmt(extern_fn_decl_stmt))
            .collect()
    }

    fn fold_assign_stmt(&mut self, assign_stmt: AssignStmt) -> AssignStmt {
        AssignStmt {
            left: self.fold_expr(assign_stmt.left),
            right: self.fold_expr(assign_stmt.right),
            op: assign_stmt.op.clone(),
            pos: assign_stmt.pos,
        }
    }

    fn fold_return_stmt(&mut self, return_stmt: ReturnStmt) -> ReturnStmt {
        ReturnStmt {
            expr: self.fold_expr(return_stmt.expr),
            pos: return_stmt.pos,
        }
    }

    fn fold_import_stmt(&mut self, import_stmt: ImportStmt) -> ImportStmt {
        ImportStmt {
            name: self.fold_name(import_stmt.name),
            pos: import_stmt.pos,
        }
    }

    fn fold_unsafe_stmt(&mut self, unsafe_stmt: UnsafeStmt) -> UnsafeStmt {
        UnsafeStmt {
            body: self.fold_stmt_list(unsafe_stmt.body),
            pos: unsafe_stmt.pos,
        }
    }

    fn fold_module_stmt(&mut self, module_stmt: ModuleStmt) -> ModuleStmt {
        ModuleStmt {
            name: self.fold_name(module_stmt.name),
            pos: module_stmt.pos,
        }
    }

    fn fold_field(&mut self, field: Field) -> Field {
        Field {
            name: self.fold_name(field.name),
            mutable: field.mutable,
            public: field.public,
            value: field.value.map(|value| self.fold_expr(value)),
            pos: field.pos,
        }
    }

    fn fold_class_decl_stmt(&mut self, class_decl_stmt: ClassDeclStmt) -> ClassDeclStmt {
        ClassDeclStmt {
            name: self.fold_name(class_decl_stmt.name),
            public: class_decl_stmt.public,
            extends: class_decl_stmt
                .extends
                .into_iter()
                .map(|name| self.fold_name(name))
                .collect(),
            fields: class_decl_stmt
                .fields
                .into_iter()
                .map(|field| self.fold_field(field))
                .collect(),
            funcs: self.fold_fn_decl_stmt_list(class_decl_stmt.funcs),
            extern_funcs: self.fold_extern_fn_decl_stmt_list(class_decl_stmt.extern_funcs),
            comments: self.fold_comment_list(class_decl_stmt.comments),
            pos: class_decl_stmt.pos,
        }
    }

    fn fold_mixin_decl_stmt(&mut self, mixin_decl_stmt: MixinDeclStmt) -> MixinDeclStmt {
        MixinDeclStmt {
            name: self.fold_name(mixin_decl_stmt.name),
            public: mixin_decl_stmt.public,
            funcs: self.fold_fn_decl_stmt_list(mixin_decl_stmt.funcs),
            extern_funcs: self.fold_extern_fn_decl_stmt_list(mixin_decl_stmt.extern_funcs),
            comments: self.fold_comment_list(mixin_decl_stmt.comments),
            pos: mixin_decl_stmt.pos,
        }
    }

    fn fold_interface_decl_stmt(
        &mut self,
        interface_decl_stmt: InterfaceDeclStmt,
    ) -> InterfaceDeclStmt {
        InterfaceDeclStmt {
            name: self.fold_name(interface_decl_stmt.name),
            public: interface_decl_stmt.public,
            functions: vec![],
            comments: self.fold_comment_list(interface_decl_stmt.comments),
            pos: interface_decl_stmt.pos,
        }
    }

    fn fold_stmt(&mut self, stmt: Stmt) -> Stmt {
        match stmt {
            Stmt::If(if_stmt) => Stmt::If(self.fold_if_stmt(if_stmt)),
            Stmt::Else(else_stmt) => Stmt::Else(self.fold_else_stmt(else_stmt)),
            Stmt::For(for_stmt) => Stmt::For(self.fold_for_stmt(for_stmt)),
            Stmt::Expr(expr_stmt) => Stmt::Expr(self.fold_expr_stmt(expr_stmt)),
            Stmt::Let(let_stmt) => Stmt::Let(self.fold_let_stmt(let_stmt)),
            Stmt::Break(break_stmt) => Stmt::Break(self.fold_break_stmt(break_stmt)),
            Stmt::Raise(raise_stmt) => Stmt::Raise(self.fold_raise_stmt(raise_stmt)),
            Stmt::LetDecl(let_decl_stmt) => Stmt::LetDecl(self.fold_let_decl_stmt(let_decl_stmt)),
            Stmt::FnDecl(fn_decl_stmt) => Stmt::FnDecl(self.fold_fn_decl_stmt(fn_decl_stmt)),
            Stmt::ExternFnDecl(extern_fn_decl_stmt) => {
                Stmt::ExternFnDecl(self.fold_extern_fn_decl_stmt(extern_fn_decl_stmt))
            }
            Stmt::Assign(assign_stmt) => Stmt::Assign(self.fold_assign_stmt(assign_stmt)),
            Stmt::Return(return_stmt) => Stmt::Return(self.fold_return_stmt(return_stmt)),
            Stmt::Import(import_stmt) => Stmt::Import(self.fold_import_stmt(import_stmt)),
            Stmt::Unsafe(unsafe_stmt) => Stmt::Unsafe(self.fold_unsafe_stmt(unsafe_stmt)),
            Stmt::Module(module_stmt) => Stmt::Module(self.fold_module_stmt(module_stmt)),
            Stmt::ClassDecl(class_decl_stmt) => {
                Stmt::ClassDecl(self.fold_class_decl_stmt(class_decl_stmt))
            }
            Stmt::MixinDecl(mixin_decl_stmt) => {
                Stmt::MixinDecl(self.fold_mixin_decl_stmt(mixin_decl_stmt))
            }
            Stmt::InterfaceDecl(interface_decl_stmt) => {
                Stmt::InterfaceDecl(self.fold_interface_decl_stmt(interface_decl_stmt))
            }
        }
    }

    fn fold_stmt_list(&mut self, stmt_list: Vec<Stmt>) -> Vec<Stmt> {
        stmt_list
            .into_iter()
            .map(|stmt| self.fold_stmt(stmt))
            .collect()
    }

    fn fold_comment(&mut self, comment: Comment) -> Comment {
        comment
    }

    fn fold_comment_list(&mut self, comments: Vec<Comment>) -> Vec<Comment> {
        comments
            .into_iter()
            .map(|comment| self.fold_comment(comment))
            .collect()
    }
}
