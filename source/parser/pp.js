//=============================================================================

// File: "pp.js", Time-stamp: <2010-06-04 10:22:43 feeley>

// Copyright (c) 2010 by Marc Feeley, All Rights Reserved.

//=============================================================================

// AST pretty printer.

function pp(ast)
{
    pp_indent(ast, 0);
}

function pp_indent(ast, indent)
{
    if (ast == null)
        print(pp_prefix(indent) + "null");
    else if (ast instanceof Program)
    {
        pp_loc(ast.loc, pp_prefix(indent) + "Program");
        pp_asts(indent, "statements", ast.statements);
    }
    else if (ast instanceof BlockStatement)
    {
        pp_loc(ast.loc, pp_prefix(indent) + "BlockStatement");
        pp_asts(indent, "statements", ast.statements);
    }
    else if (ast instanceof VariableStatement)
    {
        pp_loc(ast.loc, pp_prefix(indent) + "VariableStatement");
        pp_asts(indent, "decls", ast.decls);
    }
    else if (ast instanceof Decl)
    {
        pp_loc(ast.loc, pp_prefix(indent) + "Decl");
        print(pp_prefix(indent) + "|-id= " + ast.id);
        pp_asts(indent, "initializer", [ast.initializer]);
    }
    else if (ast instanceof ConstStatement)
    {
        pp_loc(ast.loc, pp_prefix(indent) + "ConstStatement");
        pp_asts(indent, "decls", ast.decls);
    }
    else if (ast instanceof FunctionDeclaration)
    {
        pp_loc(ast.loc, pp_prefix(indent) + "FunctionDeclaration");
        print(pp_prefix(indent) + "|-id= " + ast.id);
        print(pp_prefix(indent) + "|-params= " + ast.params);
        pp_asts(indent, "body", ast.body);
    }
    else if (ast instanceof ExprStatement)
    {
        pp_loc(ast.loc, pp_prefix(indent) + "ExprStatement");
        pp_asts(indent, "expr", [ast.expr]);
    }
    else if (ast instanceof IfStatement)
    {
        pp_loc(ast.loc, pp_prefix(indent) + "IfStatement");
        pp_asts(indent, "expr", [ast.expr]);
        pp_asts(indent, "statements", ast.statements);
    }
    else if (ast instanceof DoWhileStatement)
    {
        pp_loc(ast.loc, pp_prefix(indent) + "DoWhileStatement");
        pp_asts(indent, "statement", [ast.statement]);
        pp_asts(indent, "expr", [ast.expr]);
    }
    else if (ast instanceof WhileStatement)
    {
        pp_loc(ast.loc, pp_prefix(indent) + "WhileStatement");
        pp_asts(indent, "expr", [ast.expr]);
        pp_asts(indent, "statement", [ast.statement]);
    }
    else if (ast instanceof ForStatement)
    {
        pp_loc(ast.loc, pp_prefix(indent) + "ForStatement");
        pp_asts(indent, "expr1", [ast.expr1]);
        pp_asts(indent, "expr2", [ast.expr2]);
        pp_asts(indent, "expr3", [ast.expr3]);
        pp_asts(indent, "statement", [ast.statement]);
    }
    else if (ast instanceof ForVarStatement)
    {
        pp_loc(ast.loc, pp_prefix(indent) + "ForVarStatement");
        pp_asts(indent, "decls", ast.decls);
        pp_asts(indent, "expr2", [ast.expr2]);
        pp_asts(indent, "expr3", [ast.expr3]);
        pp_asts(indent, "statement", [ast.statement]);
    }
    else if (ast instanceof ForInStatement)
    {
        pp_loc(ast.loc, pp_prefix(indent) + "ForInStatement");
        pp_asts(indent, "lhs_expr", [ast.lhs_expr]);
        pp_asts(indent, "set_expr", [ast.set_expr]);
        pp_asts(indent, "statement", [ast.statement]);
    }
    else if (ast instanceof ForVarInStatement)
    {
        pp_loc(ast.loc, pp_prefix(indent) + "ForVarInStatement");
        print(pp_prefix(indent) + "|-id= " + ast.id);
        pp_asts(indent, "initializer", [ast.initializer]);
        pp_asts(indent, "set_expr", [ast.set_expr]);
        pp_asts(indent, "statement", [ast.statement]);
    }
    else if (ast instanceof ContinueStatement)
    {
        pp_loc(ast.loc, pp_prefix(indent) + "ContinueStatement");
        print(pp_prefix(indent) + "|-label= " + ast.label);
    }
    else if (ast instanceof BreakStatement)
    {
        pp_loc(ast.loc, pp_prefix(indent) + "BreakStatement");
        print(pp_prefix(indent) + "|-label= " + ast.label);
    }
    else if (ast instanceof ReturnStatement)
    {
        pp_loc(ast.loc, pp_prefix(indent) + "ReturnStatement");
        pp_asts(indent, "expr", [ast.expr]);
    }
    else if (ast instanceof WithStatement)
    {
        pp_loc(ast.loc, pp_prefix(indent) + "WithStatement");
        pp_asts(indent, "expr", [ast.expr]);
        pp_asts(indent, "statement", [ast.statement]);
    }
    else if (ast instanceof SwitchStatement)
    {
        pp_loc(ast.loc, pp_prefix(indent) + "SwitchStatement");
        pp_asts(indent, "expr", [ast.expr]);
        pp_asts(indent, "clauses", ast.clauses);
    }
    else if (ast instanceof CaseClause)
    {
        pp_loc(ast.loc, pp_prefix(indent) + "CaseClause");
        pp_asts(indent, "expr", [ast.expr]);
        pp_asts(indent, "statements", ast.statements);
    }
    else if (ast instanceof LabelledStatement)
    {
        pp_loc(ast.loc, pp_prefix(indent) + "LabelledStatement");
        print(pp_prefix(indent) + "|-id= " + ast.id);
        pp_asts(indent, "statement", [ast.statement]);
    }
    else if (ast instanceof ThrowStatement)
    {
        pp_loc(ast.loc, pp_prefix(indent) + "ThrowStatement");
        pp_asts(indent, "expr", [ast.expr]);
    }
    else if (ast instanceof TryStatement)
    {
        pp_loc(ast.loc, pp_prefix(indent) + "TryStatement");
        pp_asts(indent, "statement", [ast.statement]);
        print(pp_prefix(indent) + "|-id= " + ast.id);
        pp_asts(indent, "catch_part", [ast.catch_part]);
        pp_asts(indent, "finally_part", [ast.finally_part]);
    }
    else if (ast instanceof DebuggerStatement)
    {
        pp_loc(ast.loc, pp_prefix(indent) + "DebuggerStatement");
    }
    else if (ast instanceof OpExpr)
    {
        pp_loc(ast.loc, pp_prefix(indent) + "OpExpr");
        print(pp_prefix(indent) + "|-op= \"" + ast.op + "\"");
        pp_asts(indent, "exprs", ast.exprs);
    }
    else if (ast instanceof NewExpr)
    {
        pp_loc(ast.loc, pp_prefix(indent) + "NewExpr");
        pp_asts(indent, "expr", [ast.expr]);
        pp_asts(indent, "args", ast.args);
    }
    else if (ast instanceof CallExpr)
    {
        pp_loc(ast.loc, pp_prefix(indent) + "CallExpr");
        pp_asts(indent, "fn", [ast.fn]);
        pp_asts(indent, "args", ast.args);
    }
    else if (ast instanceof FunctionExpr)
    {
        pp_loc(ast.loc, pp_prefix(indent) + "FunctionExpr");
        print(pp_prefix(indent) + "|-id= " + ast.id);
        print(pp_prefix(indent) + "|-params= " + ast.params);
        pp_asts(indent, "body", ast.body);
    }
    else if (ast instanceof Literal)
    {
        pp_loc(ast.loc, pp_prefix(indent) + "Literal");
        print(pp_prefix(indent) + "|-value= " + ast.value);
    }
    else if (ast instanceof ArrayLiteral)
    {
        pp_loc(ast.loc, pp_prefix(indent) + "ArrayLiteral");
        pp_asts(indent, "exprs", ast.exprs);
    }
    else if (ast instanceof ObjectLiteral)
    {
        pp_loc(ast.loc, pp_prefix(indent) + "ObjectLiteral");
        pp_asts(indent, "properties", ast.properties);
    }
    else if (ast instanceof Property)
    {
        pp_loc(ast.loc, pp_prefix(indent) + "Property");
        pp_asts(indent, "name", [ast.name]);
        pp_asts(indent, "value", [ast.value]);
    }
    else if (ast instanceof Ref)
    {
        pp_loc(ast.loc, pp_prefix(indent) + "Ref");
        print(pp_prefix(indent) + "|-id= " + ast.id);
    }
    else if (ast instanceof This)
    {
        pp_loc(ast.loc, pp_prefix(indent) + "This");
    }
    else
        print(pp_prefix(indent) + "UNKNOWN AST");
}

function pp_loc(loc, line)
{
    print(line + pp_spaces(40-line.length) + "  (" + loc.to_string() + ":)");
}

function pp_asts(indent, label, asts)
{
    if (asts != null)
    {
        print(pp_prefix(indent) + "|-" + label + "=");
        for (var i=0; i<asts.length; i++)
            pp_indent(asts[i], indent+1);
    }
}

function pp_prefix(indent)
{
    if (indent > 0)
        return "|   " + pp_prefix(indent-1);
    else
        return "";
}

function pp_spaces(n)
{
    if (n > 0)
        return " " + pp_spaces(n-1);
    else
        return "";
}

//=============================================================================
