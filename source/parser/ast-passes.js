//=============================================================================

// File: "ast-passes.js", Time-stamp: <2010-06-08 21:53:22 feeley>

// Copyright (c) 2010 by Marc Feeley, All Rights Reserved.

//=============================================================================

// Pass 1.
//
// Transforms an AST into a simpler AST.
//
//   - elimination of "var" statements
//   - flattening of nested block statements

function ast_pass1_ctx(vars, scope)
{
    this.vars = vars;
    this.scope = scope;
}

function ast_pass1_empty_ctx(scope)
{
    return new ast_pass1_ctx({}, scope);
}

function ast_pass1_global_ctx(scope)
{
    return ast_pass1_empty_ctx(scope);
}

function variable(id, is_local)
{
    this.id = id;
    this.is_local = is_local;
}

function ast_pass1_add_variable(id, ctx, is_local)
{
    var id_str = id.value;
    var v = ctx.vars[id_str];
    if (typeof v == "undefined")
    {
        v = new variable(id, is_local);
        ctx.vars[id_str] = v;
    }
    return v;
}

function ast_pass1_function_ctx(ast)
{
    var ctx = ast_pass1_empty_ctx(ast);
    ast.params.forEach(function (param, i, self)
                       {
                           ast_pass1_add_variable(param, ctx, false);
                       });
    return ctx;
}

function ast_pass1(ast, ctx)
{
    if (ast == null)
    {
        // no transformation
        return ast;
    }
    else if (ast instanceof Program)
    {
        var new_ctx = ast_pass1_empty_ctx(ast);
        ast.block = ast_pass1(ast.block, new_ctx);
        ast.vars = new_ctx.vars;
        return ast;
    }
    else if (ast instanceof BlockStatement)
    {
        ast.statements = ast_pass1_statements(ast.statements, ctx);
        return ast;
    }
    else if (ast instanceof VariableStatement)
    {
        var accum = [];
        ast.decls.forEach(function (decl, i, self)
                          {
                              ast_pass1_add_variable(decl.id, ctx, true);
                              if (decl.initializer != null)
                              {
                                  decl.initializer = ast_pass1(decl.initializer, ctx);
                                  accum.push(new OpExpr(decl.loc,
                                                        op2_table[EQUAL_CAT],
                                                        [new Ref(decl.id.loc,
                                                                 decl.id),
                                                         decl.initializer]));
                              }
                          });
        if (accum.length == 1)
            return accum[0];
        else
            return new BlockStatement(ast.loc,
                                      accum);
    }
    else if (ast instanceof ConstStatement)
    {
        // TODO
        error("ConstStatement not implemented");
        return ast;
    }
    else if (ast instanceof FunctionDeclaration)
    {
        ast_pass1_add_variable(ast.id, ctx, true);
        ast.funct = ast_pass1(ast.funct, ctx);
        return ast;
    }
    else if (ast instanceof ExprStatement)
    {
        ast.expr = ast_pass1(ast.expr, ctx);
        return ast;
    }
    else if (ast instanceof IfStatement)
    {
        ast.expr = ast_pass1(ast.expr, ctx);
        ast.statements[0] = ast_pass1(ast.statements[0], ctx);
        if (ast.statements.length == 2)
            ast.statements[1] = ast_pass1(ast.statements[1], ctx);
        return ast;
    }
    else if (ast instanceof DoWhileStatement)
    {
        ast.statement = ast_pass1(ast.statement, ctx);
        ast.expr = ast_pass1(ast.expr, ctx);
        return ast;
    }
    else if (ast instanceof WhileStatement)
    {
        ast.expr = ast_pass1(ast.expr, ctx);
        ast.statement = ast_pass1(ast.statement, ctx);
        return ast;
    }
    else if (ast instanceof ForStatement)
    {
        ast.expr1 = ast_pass1(ast.expr1, ctx);
        ast.expr2 = ast_pass1(ast.expr2, ctx);
        ast.expr3 = ast_pass1(ast.expr3, ctx);
        ast.statement = ast_pass1(ast.statement, ctx);
        return ast;
    }
    else if (ast instanceof ForVarStatement)
    {
        var accum = new Literal(ast.loc, null);
        for (var i=ast.decls.length-1; i>=0; i--)
        {
            var decl = ast.decls[i];
            ast_pass1_add_variable(decl.id, ctx, true);
            if (decl.initializer != null)
            {
                decl.initializer = ast_pass1(decl.initializer, ctx);
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
        ast.expr2 = ast_pass1(ast.expr2, ctx);
        ast.expr3 = ast_pass1(ast.expr3, ctx);
        ast.statement = ast_pass1(ast.statement, ctx);
        return new ForStatement(ast.loc,
                                accum,
                                ast.expr2,
                                ast.expr3,
                                ast.statement);
    }
    else if (ast instanceof ForInStatement)
    {
        ast.lhs_expr = ast_pass1(ast.lhs_expr, ctx);
        ast.set_expr = ast_pass1(ast.set_expr, ctx);
        ast.statement = ast_pass1(ast.statement, ctx);
        return ast;
    }
    else if (ast instanceof ForVarInStatement)
    {
        ast_pass1_add_variable(ast.id, ctx, true);
        var initializer = ast_pass1(ast.initializer, ctx);
        var set_expr = ast_pass1(ast.set_expr, ctx);
        var statement = ast_pass1(ast.statement, ctx);
        var for_stat = new ForInStatement(ast.loc,
                                          new Ref(ast.id.loc,
                                                  ast.id),
                                          set_expr,
                                          statement);
        if (initializer == null)
            return for_stat;
        else
            return new BlockStatement(ast.loc,
                                      [new OpExpr(ast.loc,
                                                  op2_table[EQUAL_CAT],
                                                  [new Ref(ast.id.loc,
                                                           ast.id),
                                                   initializer]),
                                       for_stat]);
    }
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
        ast.expr = ast_pass1(ast.expr, ctx);
        return ast;
    }
    else if (ast instanceof WithStatement)
    {
        ast.expr = ast_pass1(ast.expr, ctx);
        ast.statement = ast_pass1(ast.statement, ctx);
        return ast;
    }
    else if (ast instanceof SwitchStatement)
    {
        // TODO
        error("SwitchStatement not implemented");
        /*
        pp_loc(ast.loc, pp_prefix(indent) + "SwitchStatement");
        pp_asts(indent, "expr", [ast.expr]);
        pp_asts(indent, "clauses", ast.clauses);
        */
    }
    else if (ast instanceof CaseClause)
    {
        // TODO
        error("CaseClause not implemented");
        /*
        pp_loc(ast.loc, pp_prefix(indent) + "CaseClause");
        pp_asts(indent, "expr", [ast.expr]);
        pp_asts(indent, "statements", ast.statements);
        */
    }
    else if (ast instanceof LabelledStatement)
    {
        // TODO
        error("LabelledStatement not implemented");
        /*
        pp_loc(ast.loc, pp_prefix(indent) + "LabelledStatement");
        print(pp_prefix(indent) + "|-id= " + ast.id.value);
        pp_asts(indent, "statement", [ast.statement]);
        */
    }
    else if (ast instanceof ThrowStatement)
    {
        // TODO
        error("ThrowStatement not implemented");
        /*
        pp_loc(ast.loc, pp_prefix(indent) + "ThrowStatement");
        pp_asts(indent, "expr", [ast.expr]);
        */
    }
    else if (ast instanceof TryStatement)
    {
        // TODO
        error("TryStatement not implemented");
        /*
        pp_loc(ast.loc, pp_prefix(indent) + "TryStatement");
        pp_asts(indent, "statement", [ast.statement]);
        print(pp_prefix(indent) + "|-id= " + ast.id.value);
        pp_asts(indent, "catch_part", [ast.catch_part]);
        pp_asts(indent, "finally_part", [ast.finally_part]);
        */
    }
    else if (ast instanceof DebuggerStatement)
    {
        return ast;
    }
    else if (ast instanceof OpExpr)
    {
        ast.exprs = ast_pass1_exprs(ast.exprs, ctx);
        return ast;
    }
    else if (ast instanceof NewExpr)
    {
        ast.expr = ast_pass1(ast.expr, ctx);
        ast.args = ast_pass1_exprs(ast.args, ctx);
        return ast;
    }
    else if (ast instanceof CallExpr)
    {
        ast.fn = ast_pass1(ast.fn, ctx);
        ast.args = ast_pass1_exprs(ast.args, ctx);
        return ast;
    }
    else if (ast instanceof FunctionExpr)
    {
        var new_ctx = ast_pass1_function_ctx(ast);
        if (ast.id != null)
            ast_pass1_add_variable(ast.id, new_ctx, true);
        ast.body = ast_pass1_statements(ast.body, new_ctx);
        ast.vars = new_ctx.vars;
        return ast;
    }
    else if (ast instanceof Literal)
    {
        return ast;
    }
    else if (ast instanceof ArrayLiteral)
    {
        ast.exprs = ast_pass1_exprs(ast.exprs, ctx);
        return ast;
    }
    else if (ast instanceof ObjectLiteral)
    {
        ast.properties.forEach(function (prop, i, self)
                               {
                                   prop.value = ast_pass1(prop.value, ctx);
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
        error("UNKNOWN AST");
}

function ast_pass1_statements(asts, ctx)
{
    var accum = [];
    asts.forEach(function (ast, i, asts)
                 {
                     var a = ast_pass1(ast, ctx);
                     if (a instanceof BlockStatement)
                         accum.push(a.statements); // merge embedded blocks
                     else
                         accum.push([a]);
                 });
    return Array.prototype.concat.apply([], accum);
}

function ast_pass1_exprs(asts, ctx)
{
    var accum = [];
    asts.forEach(function (ast, i, asts)
                 {
                     asts[i] = ast_pass1(ast, ctx);
                 });
    return asts;
}

function ast_passes(ast)
{
    ast_pass1(ast, null);
    return ast;
}

//=============================================================================

/*

 to be used in pass2 to resolve variable references

function ast_pass2_lookup_variable(id, ctx)
{
    var x = ctx.vars[id];
    if (x !== undefined)
        return x;
    else if (ctx.parent_ctx != null)
        return ast_pass1_lookup_variable(id, ctx.parent_ctx);
    else
        return pass2_add_variable(id, ctx, false);
}
*/
