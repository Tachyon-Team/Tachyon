//=============================================================================

// File: "ast-passes.js", Time-stamp: <2010-06-23 22:00:14 feeley>

// Copyright (c) 2010 by Marc Feeley, All Rights Reserved.

//=============================================================================

// Generic AST walker.

function ast_walk_statement(ast, ctx)
{
    if (ast == null)
    {
        // no transformation
        return ast;
    }
    else if (ast instanceof Program)
    {
        ast.block = ctx.walk_statement(ast.block);
        return ast;
    }
    else if (ast instanceof FunctionDeclaration)
    {
        ast.funct = ctx.walk_statement(ast.funct);
        return ast;
    }
    else if (ast instanceof BlockStatement)
    {
        ast.statements = ast_walk_statements(ast.statements, ctx);
        return ast;
    }
    /*
     * The VariableStatement case is eliminated in pass1.
     *
    else if (ast instanceof VariableStatement)
    {
        ast.decls.forEach(function (decl, i, self)
                          {
                              decl.initializer = ctx.walk_expr(decl.initializer);
                          });
        return ast;
    }
    */
    else if (ast instanceof ConstStatement)
    {
        // TODO
        error("ConstStatement not implemented");
        return ast;
    }
    else if (ast instanceof ExprStatement)
    {
        ast.expr = ctx.walk_expr(ast.expr);
        return ast;
    }
    else if (ast instanceof IfStatement)
    {
        ast.expr = ctx.walk_expr(ast.expr);
        ast.statements = ast_walk_statements(ast.statements, ctx);
        return ast;
    }
    else if (ast instanceof DoWhileStatement)
    {
        ast.statement = ctx.walk_statement(ast.statement);
        ast.expr = ctx.walk_expr(ast.expr);
        return ast;
    }
    else if (ast instanceof WhileStatement)
    {
        ast.expr = ctx.walk_expr(ast.expr);
        ast.statement = ctx.walk_statement(ast.statement);
        return ast;
    }
    else if (ast instanceof ForStatement)
    {
        ast.expr1 = ctx.walk_expr(ast.expr1);
        ast.expr2 = ctx.walk_expr(ast.expr2);
        ast.expr3 = ctx.walk_expr(ast.expr3);
        ast.statement = ctx.walk_statement(ast.statement);
        return ast;
    }
    /*
     * The ForVarStatement case is eliminated in pass1.
     *
    else if (ast instanceof ForVarStatement)
    {
        for (var i=ast.decls.length-1; i>=0; i--)
        {
            var decl = ast.decls[i];
            decl.initializer = ctx.walk_expr(decl.initializer);
        }
        ast.expr2 = ctx.walk_expr(ast.expr2);
        ast.expr3 = ctx.walk_expr(ast.expr3);
        ast.statement = ctx.walk_statement(ast.statement);
        return ast;
    }
    */
    else if (ast instanceof ForInStatement)
    {
        ast.lhs_expr = ctx.walk_expr(ast.lhs_expr);
        ast.set_expr = ctx.walk_expr(ast.set_expr);
        ast.statement = ctx.walk_statement(ast.statement);
        return ast;
    }
    /*
     * The ForVarInStatement case is eliminated in pass1.
     *
    else if (ast instanceof ForVarInStatement)
    {
        ast.initializer = ctx.walk_expr(ast.initializer);
        ast.set_expr = ctx.walk_expr(ast.set_expr);
        ast.statement = ctx.walk_statement(ast.statement);
        return ast;
    }
    */
    else if (ast instanceof ContinueStatement)
    {
        return ast;
    }
    else if (ast instanceof BreakStatement)
    {
        return ast;
    }
    else if (ast instanceof ReturnStatement)
    {
        ast.expr = ctx.walk_expr(ast.expr);
        return ast;
    }
    else if (ast instanceof WithStatement)
    {
        ast.expr = ctx.walk_expr(ast.expr);
        ast.statement = ctx.walk_statement(ast.statement);
        return ast;
    }
    else if (ast instanceof SwitchStatement)
    {
        ast.expr = ctx.walk_expr(ast.expr);
        ast.clauses.forEach(function (c, i, asts)
                            {
                                c.expr = ctx.walk_expr(c.expr);
                                c.statements = ast_walk_statements(c.statements, ctx);
                            });
        return ast;
    }
    else if (ast instanceof LabelledStatement)
    {
        ast.statement = ctx.walk_statement(ast.statement);
        return ast;
    }
    else if (ast instanceof ThrowStatement)
    {
        ast.expr = ctx.walk_expr(ast.expr);
        return ast;
    }
    else if (ast instanceof TryStatement)
    {
        ast.statement = ctx.walk_statement(ast.statement);
        ast.catch_part = ctx.walk_statement(ast.catch_part);
        ast.finally_part = ctx.walk_statement(ast.finally_part);
        return ast;
    }
    else if (ast instanceof DebuggerStatement)
    {
        return ast;
    }
    else
    {
        //pp(ast);
        error("UNKNOWN AST");
    }
}

function ast_walk_statements(asts, ctx)
{
    asts.forEach(function (ast, i, asts)
                 {
                     asts[i] = ctx.walk_statement(ast);
                 });
    return asts;
}

function ast_walk_expr(ast, ctx)
{
    if (ast == null)
    {
        // no transformation
        return ast;
    }
    else if (ast instanceof OpExpr)
    {
        ast.exprs = ast_walk_exprs(ast.exprs, ctx);
        return ast;
    }
    else if (ast instanceof NewExpr)
    {
        ast.expr = ctx.walk_expr(ast.expr);
        ast.args = ast_walk_exprs(ast.args, ctx);
        return ast;
    }
    else if (ast instanceof CallExpr)
    {
        ast.fn = ctx.walk_expr(ast.fn);
        ast.args = ast_walk_exprs(ast.args, ctx);
        return ast;
    }
    else if (ast instanceof FunctionExpr)
    {
        ast.body = ast_walk_statements(ast.body, ctx);
        return ast;
    }
    else if (ast instanceof Literal)
    {
        return ast;
    }
    else if (ast instanceof ArrayLiteral)
    {
        ast.exprs = ast_walk_exprs(ast.exprs, ctx);
        return ast;
    }
    else if (ast instanceof ObjectLiteral)
    {
        ast.properties.forEach(function (prop, i, self)
                               {
                                   prop.name = ctx.walk_expr(prop.name);
                                   prop.value = ctx.walk_expr(prop.value);
                               });
        return ast;
    }
    else if (ast instanceof Ref)
    {
        return ast;
    }
    else if (ast instanceof This)
    {
        return ast;
    }
    else
    {
        //pp(ast);
        error("UNKNOWN AST");
    }
}

function ast_walk_exprs(asts, ctx)
{
    asts.forEach(function (ast, i, asts)
                 {
                     asts[i] = ctx.walk_expr(ast);
                 });
    return asts;
}

//-----------------------------------------------------------------------------

// Pass 1.
//
// Transforms an AST into a simpler AST.
//
//   - transform "VariableStatement" into assignment
//   - transform "ForVarStatement" into "ForStatement"
//   - transform "ForVarInStatement" into "ForInStatement"
//   - flattening of nested block statements
//   - elimination of composite assignment operators (e.g.: +=, *=, ...)

function ast_pass1_ctx(vars, scope)
{
    this.vars = vars;
    this.scope = scope;
}

function ast_pass1_empty_ctx(scope)
{
    return new ast_pass1_ctx({}, scope);
}

function ast_Variable(id, is_param, scope)
{
    this.id       = id;
    this.is_param = is_param;
    this.scope    = scope;
}

ast_Variable.prototype.toString = function ()
{
    return this.id.toString();
};

ast_pass1_ctx.prototype.function_ctx = function (ast)
{
    var new_ctx = ast_pass1_empty_ctx(ast);
    ast.params.forEach(function (param, i, self)
                       {
                           new_ctx.add_variable(param, true);
                       });
    return new_ctx;
}

ast_pass1_ctx.prototype.add_variable = function (id, is_param)
{
    var id_str = id.value;
    var v = this.vars[id_str];
    if (typeof v == "undefined")
    {
        v = new ast_Variable(id, is_param, this.scope);
        this.vars[id_str] = v;
    }
    return v;
}

ast_pass1_ctx.prototype.walk_statement = function (ast)
{
    if (ast == null)
    {
        // no transformation
        return ast;
    }
    else if (ast instanceof Program)
    {
        var new_ctx = ast_pass1_empty_ctx(ast);
        ast.block = new_ctx.walk_statement(ast.block);
        ast.vars = new_ctx.vars;
        return ast;
    }
    else if (ast instanceof FunctionDeclaration)
    {
        this.add_variable(ast.id, false);
        ast.funct = this.walk_expr(ast.funct);
        return ast;
    }
    else if (ast instanceof VariableStatement)
    {
        var ctx = this;
        var accum = [];
        ast.decls.forEach(function (decl, i, self)
                          {
                              ctx.add_variable(decl.id, false);
                              if (decl.initializer != null)
                              {
                                  decl.initializer = ctx.walk_expr(decl.initializer);
                                  accum.push(new ExprStatement(
                                               decl.loc,
                                               new OpExpr(decl.loc,
                                                          op2_table[EQUAL_CAT],
                                                          [new Ref(decl.id.loc,
                                                                   decl.id),
                                                           decl.initializer])));
                              }
                          });
        if (accum.length == 1)
            return accum[0];
        else
            return new BlockStatement(ast.loc,
                                      accum);
    }
    else if (ast instanceof ForVarStatement)
    {
        var accum = new Literal(ast.loc, null);
        for (var i=ast.decls.length-1; i>=0; i--)
        {
            var decl = ast.decls[i];
            this.add_variable(decl.id, false);
            if (decl.initializer != null)
            {
                decl.initializer = this.walk_expr(decl.initializer);
                accum = new OpExpr(decl.loc,
                                   op2_table[COMMA_CAT],
                                   [new OpExpr(decl.loc,
                                               op2_table[EQUAL_CAT],
                                               [new Ref(decl.id.loc,
                                                        decl.id),
                                                decl.initializer]),
                                    accum]);
            }
        }
        ast.expr2 = this.walk_expr(ast.expr2);
        ast.expr3 = this.walk_expr(ast.expr3);
        ast.statement = this.walk_statement(ast.statement);
        return new ForStatement(ast.loc,
                                accum,
                                ast.expr2,
                                ast.expr3,
                                ast.statement);
    }
    else if (ast instanceof ForVarInStatement)
    {
        this.add_variable(ast.id, false);
        var initializer = this.walk_expr(ast.initializer);
        var set_expr = this.walk_expr(ast.set_expr);
        var statement = this.walk_statement(ast.statement);
        var for_stat = new ForInStatement(ast.loc,
                                          new Ref(ast.id.loc,
                                                  ast.id),
                                          set_expr,
                                          statement);
        if (initializer == null)
            return for_stat;
        else
            return new BlockStatement(ast.loc,
                                      [new ExprStatement(
                                         decl.loc,
                                         new OpExpr(ast.loc,
                                                    op2_table[EQUAL_CAT],
                                                    [new Ref(ast.id.loc,
                                                             ast.id),
                                                     initializer])),
                                       for_stat]);
    }
    else if (ast instanceof TryStatement)
    {
        if (ast.id != null)
            this.add_variable(ast.id, false);
        ast.statement = this.walk_statement(ast.statement);
        ast.catch_part = this.walk_statement(ast.catch_part);
        ast.finally_part = this.walk_statement(ast.finally_part);
        return ast;
    }
    else
        return ast_walk_statement(ast, this);
};

ast_pass1_ctx.prototype.walk_expr = function (ast)
{
    if (ast == null)
    {
        // no transformation
        return ast;
    }
    else if (ast instanceof FunctionExpr)
    {
        var new_ctx = this.function_ctx(ast);
        if (ast.id != null)
            new_ctx.add_variable(ast.id, false);
        ast.body = new_ctx.walk_statements(ast.body);
        ast.vars = new_ctx.vars;
        ast.parent = this.scope;
        return ast;
    }
    else if (ast instanceof OpExpr)
    {
        function assgToOp(op)
        {
            return new OpExpr(
                ast.loc,
                'x = y',
                [ 
                    ast.exprs[0],
                    new OpExpr(
                        ast.loc,
                        op, 
                        [ast.exprs[0], ast.exprs[1]]
                    )
                ]
            );
        }

        switch (ast.op)
        {
            case 'x += y':      return assgToOp('x + y');   break;
            case 'x -= y':      return assgToOp('x - y');   break;
            case 'x *= y':      return assgToOp('x * y');   break;
            case 'x /= y':      return assgToOp('x / y');   break;
            case 'x %= y':      return assgToOp('x % y');   break;

            case 'x &= y':      return assgToOp('x & y');   break;
            case 'x |= y':      return assgToOp('x | y');   break;
            case 'x ^= y':      return assgToOp('x ^ y');   break;

            case 'x <<= y':     return assgToOp('x << y');  break;
            case 'x >>= y':     return assgToOp('x >> y');  break;
            case 'x >>>= y':    return assgToOp('x >>> y'); break;

            default:
            return ast_walk_expr(ast, this);
        }
    }
    else
    {
        return ast_walk_expr(ast, this);
    }
};

ast_pass1_ctx.prototype.walk_statements = function (asts)
{
    var ctx = this;
    var accum = [];
    asts.forEach(function (ast, i, asts)
                 {
                     var a = ctx.walk_statement(ast);
                     if (a instanceof BlockStatement)
                         accum.push(a.statements); // merge embedded blocks
                     else
                         accum.push([a]);
                 });
    return Array.prototype.concat.apply([], accum);
};

function ast_pass1(ast)
{
    var ctx = ast_pass1_empty_ctx(null);
    ctx.walk_statement(ast);
}

//-----------------------------------------------------------------------------

// Pass 2.
//
// Transforms an AST into an AST in which
//
//   - variables are resolved according to their scope
//   - a map of escaping variables is added to functions
//   - a map of closure-provided variables is added to functions
//   - a list of all nested functions is added to functions

function ast_pass2_ctx(scope)
{
    this.scope = scope;
}

ast_pass2_ctx.prototype.function_ctx = function (ast)
{
    var new_ctx = new ast_pass2_ctx(ast);

    ast.params.forEach(
        function (param, i, self)
        {
            param[i] = new_ctx.resolve_variable(param);
        }
    );

    return new_ctx;
}

ast_pass2_ctx.prototype.resolve_variable = function (id)
{
    // Where is this id declared???

    function resolve(scope)
    {
        var id_str = id.value;

        // If the id is a local variable of the current scope
        var v = scope.vars[id_str];
        if (typeof v != "undefined")
            return v;

        // If the id is a free variable of the current scope
        v = scope.free_vars[id_str];
        if (typeof v != "undefined")
            return v;

        // If the current scope is global
        if (scope instanceof Program)
            v = new ast_Variable(id, false, scope);
        else
            v = resolve(scope.parent);

        // This variable is not a local, add it to the free variable list of the scope
        scope.free_vars[id_str] = v;

        // If this is not a global variable, add it to the closure variable list of the scope
        if (!(v.scope instanceof Program))
            scope.clos_vars[id_str] = v;

        // If the variable's scope is a function and does not match the current scope, mark
        // the variable as escaping in its scope of origin
        if (v.scope instanceof FunctionExpr && v.scope !== scope)
            v.scope.esc_vars[id_str] = v;

        return v;
    }

    return resolve(this.scope);
}

ast_pass2_ctx.prototype.walk_statement = function (ast)
{
    if (ast == null)
    {
        // no transformation
        return ast;
    }
    else if (ast instanceof Program)
    {
        ast.free_vars = {};
        ast.funcs = [];

        var new_ctx = new ast_pass2_ctx(ast);
        ast.block = new_ctx.walk_statement(ast.block);
        return ast;
    }
    else if (ast instanceof FunctionDeclaration)
    {
        // Set the current function declaration
        this.func_decl = ast;

        ast.id = this.resolve_variable(ast.id);
        ast.funct = this.walk_expr(ast.funct);

        return ast;
    }
    else if (ast instanceof TryStatement)
    {
        if (ast.id != null)
            ast.id = this.resolve_variable(ast.id);
        ast.statement = this.walk_statement(ast.statement);
        ast.catch_part = this.walk_statement(ast.catch_part);
        ast.finally_part = this.walk_statement(ast.finally_part);
        return ast;
    }
    else
    {
        return ast_walk_statement(ast, this);
    }
};

ast_pass2_ctx.prototype.walk_expr = function (ast)
{
    if (ast == null)
    {
        // no transformation
        return ast;
    }
    else if (ast instanceof FunctionExpr)
    {
        ast.free_vars = {};
        ast.clos_vars = {};
        ast.esc_vars = {};
        ast.funcs = [];

        // Add this function to the scope's nested function list
        // If this function is part of a function declaration, add the declaration instead
        if (this.func_decl != undefined && this.func_decl.funct === ast)
            this.scope.funcs.push(this.func_decl);
        else
            this.scope.funcs.push(ast);

        var new_ctx = this.function_ctx(ast);

        if (ast.id != null)
            ast.id = new_ctx.resolve_variable(ast.id);

        ast.body = ast_walk_statements(ast.body, new_ctx);

        return ast;
    }
    else if (ast instanceof Ref)
    {
        ast.id = this.resolve_variable(ast.id);
        return ast;
    }
    else
    {
        return ast_walk_expr(ast, this);
    }
};

function ast_pass2(ast)
{
    var ctx = new ast_pass2_ctx(ast);
    ctx.walk_statement(ast);
}

//-----------------------------------------------------------------------------

function ast_normalize(ast)
{
    ast_pass1(ast);
    ast_pass2(ast);

    return ast;
}

//=============================================================================
