/**
@fileOverview
Intermediate Representation (IR) translation implementation

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010 Maxime Chevalier-Boisvert, All Rights Reserved
*/

// TODO: Explain result of translation in function description comments

/**
Convert an AST code unit into IR functions
*/
function UnitToIR(astUnit)
{
    // TODO
    //
    // Top level AST may contain functions and statements...
    // Top level AST is somewhat like a function of its own
    //
    // Parse into a top-level AST function per code unit? 
    // - Only called once
    //   - This ptr is always the global object
    // - Inner functions are nested
    // - Has variable declarations which are global...
    //   - Indicated by symbol resolution of AST
    // - Return value is irrelevant
    //   - Return is actually illegal at global scope
    //

    // Ensure that the top-level AST is a program
    assert (astUnit instanceof Program, 'top-level AST must be program');

    // Treat the top-level unit as a function
    return StmtsToIRFunc(
        '',
        null,
        [],
        astUnit.vars,
        [],
        astUnit.block.statements
    );
}

/**
Convert an AST function into an IR function
*/
function FuncToIR(astFunc)
{
    // TODO

    // Ensure that the top-level AST is a program
    assert (astFunc instanceof FunctionExpr, 'function must be AST function');

    



}

/**
Convert an AST statement list into an IR function
*/
function StmtsToIRFunc(
    funcName, 
    parentFunc,
    params,
    locals,
    freeVars,
    bodyStmts
)
{
    // Extract the argument names
    var argNames = [];
    for (var i = 0; i < params.length; ++i)
        argNames.push(params[i].toString());

    // Create a new function object for the function
    var newFunc = new IRFunction(funcName, argNames);

    // Create a new CFG for the function
    var cfg = new ControlFlowGraph(newFunc);

    // Set the CFG for the function
    newFunc.virginIR = cfg;

    // Set the parent for the function
    newFunc.parentFunc = parentFunc;

    // Generate code for the function body
    var body = StmtsToIR(bodyStmts, cfg);

    // Return the new function
    return newFunc;
}

/**
Convert an AST statement into IR code
*/
function StmtToIR(astStmt, cfg)
{
    // TODO
    // Take entry and exit basic blocks as input?



    /*
    if (ast instanceof FunctionDeclaration)
    {
        ast.funct = ctx.walk_statement(ast.funct);
        return ast;
    }
    else if (ast instanceof BlockStatement)
    {
        ast.statements = ast_walk_statements(ast.statements, ctx);
        return ast;
    }
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
    else if (ast instanceof ForInStatement)
    {
        ast.lhs_expr = ctx.walk_expr(ast.lhs_expr);
        ast.set_expr = ctx.walk_expr(ast.set_expr);
        ast.statement = ctx.walk_statement(ast.statement);
        return ast;
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
    */

}

/**
Convert a statement list to IR code
*/
function StmtsToIR(stmts, cfg)
{
}

/**
Convert an AST expression into IR code
*/
function ExprToIR(astExpr, cfg)
{
    // TODO
    // Take entry and exit basic blocks as input?




    /*
    if (ast instanceof OpExpr)
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
    */


}



var filename = 'parser/tests/test4.js';
var port = new File_input_port(filename);
var p = new Parser(new Scanner(port), true);
var ast = p.parse();
var normalized_ast = ast_normalize(ast);

pp(normalized_ast); // pretty-print AST
print('\n');

ir = UnitToIR(normalized_ast);

print(ir);




