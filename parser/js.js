//=============================================================================

// File: "js.js", Time-stamp: <2010-05-24 16:58:22 feeley>

// Copyright (c) 2010 by Marc Feeley, All Rights Reserved.

//=============================================================================

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
    else if (ast instanceof FunctionDeclaration)
    {
        pp_loc(ast.loc, pp_prefix(indent) + "FunctionDeclaration");
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
        pp_asts(indent, "elements", ast.elements);
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
    else if (ast instanceof This)
    {
        pp_loc(ast.loc, pp_prefix(indent) + "This");
    }
    else if (ast instanceof Var)
    {
        pp_loc(ast.loc, pp_prefix(indent) + "Var");
        print(pp_prefix(indent) + "|-id= " + ast.id);
    }
    else if (ast instanceof VarDecl)
    {
        pp_loc(ast.loc, pp_prefix(indent) + "VarDecl");
        print(pp_prefix(indent) + "|-id= " + ast.id);
        pp_asts(indent, "initializer", [ast.initializer]);
    }
    else if (ast instanceof VariableStatement)
    {
        pp_loc(ast.loc, pp_prefix(indent) + "VariableStatement");
        pp_asts(indent, "decls", ast.decls);
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
    else if (ast instanceof BreakStatement)
    {
        pp_loc(ast.loc, pp_prefix(indent) + "BreakStatement");
        print(pp_prefix(indent) + "|-label= " + ast.label);
    }
    else if (ast instanceof ContinueStatement)
    {
        pp_loc(ast.loc, pp_prefix(indent) + "ContinueStatement");
        print(pp_prefix(indent) + "|-label= " + ast.label);
    }
    else if (ast instanceof ReturnStatement)
    {
        pp_loc(ast.loc, pp_prefix(indent) + "ReturnStatement");
        pp_asts(indent, "expr", [ast.expr]);
    }
    else
        print(pp_prefix(indent) + "UNKNOWN AST for production rule " + ast.type);
}

function pp_loc(loc, line)
{
    print(line + pp_spaces(40-line.length) + "  (" + loc.to_string() + ":)");
}

function pp_asts(indent, label, asts)
{
    print(pp_prefix(indent) + "|-" + label + "=");
    for (var i=0; i<asts.length; i++)
        pp_indent(asts[i], indent+1);
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

function main()
{
    var args = command_line();

    if (args.length == 1)
    {
        var filename = args[0];
        var port = new File_input_port(filename);
        var s = new Scanner(port);
        var p = new Parser(s);
        var ast = p.parse();
        pp(ast); // pretty print the AST
    }
}

main();

//=============================================================================
