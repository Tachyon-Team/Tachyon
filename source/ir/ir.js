/**
@fileOverview
Intermediate Representation (IR) translation implementation

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010 Maxime Chevalier-Boisvert, All Rights Reserved
*/

//
// TODO: Explain result of translation in function description comments
//

// ********************* TODO **************
// Refactor to create less useless basic blocks and time execution...

/**
Convert an AST code unit into IR functions
*/
function UnitToIR(
    astUnit
)
{
    //
    // Top level AST may contain functions and statements...
    // Top level AST is somewhat like a function of its own
    //
    // Parse into a top-level AST function per code unit
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
        [],
        [],
        astUnit.block.statements
    );
}

/**
Convert an AST function into an IR function
*/
function FuncToIR(
    funcName,
    parentFunc,
    astFunc
)
{
    // Ensure that the top-level AST is a program
    assert (astFunc instanceof FunctionExpr, 'function must be AST function expression');

    // Compile the function
    return StmtsToIRFunc(
        funcName, 
        parentFunc,
        astFunc.params,
        astFunc.vars,
        astFunc.free_vars,
        astFunc.body
    );
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
    {
        argNames.push(params[i].toString());
    }

    // Create a new function object for the function
    var newFunc = new IRFunction(funcName, argNames);

    // Create a new CFG for the function
    var cfg = new ControlFlowGraph(newFunc);

    // Set the CFG for the function
    newFunc.virginIR = cfg;

    // Set the parent for the function
    newFunc.parentFunc = parentFunc;

    // Create a map for the local variable storage locations
    var localsMap = new HashMap();

    // Add the arguments to the locals map
    localsMap.addItem('arguments', cfg.getArgObj());
    for (var i = 0; i < params.length; ++i)
    {
        localsMap.addItem(params[i].toString(), cfg.getArgVal(i));
    }

    // Add the lcoals to the locals map
    for (var i = 0; i < locals.length; ++i)
    {
        localsMap.addItem(locals[i].toString(), new UndefConst());
    }

    // Get the entry block for the CFG
    var entryBlock = cfg.getEntryBlock();

    // Create a block for the CFG exit
    var exitBlock = cfg.getNewBlock('exit');

    // Add a return undefined instruction to the exit block
    exitBlock.addInstr(new RetInstr(new UndefConst()));

    // Generate code for the function body
    StmtsToIR(
        bodyStmts, 
        cfg,
        localsMap,
        entryBlock,
        exitBlock
    );

    // Simplify the CFG
    cfg.simplify();

    // Run a validation test on the CFG
    var validation = cfg.validate();

    // Ensure that the CFG is valid
    assert (
        validation == true,
        'Invalid CFG for function "' + funcName + '":\n' + 
        validation + '\n' +
        cfg.toString()
    );

    // Return the new function
    return newFunc;
}

/**
Convert a statement list to IR code
*/
function StmtsToIR(
    stmtList, 
    cfg,
    localsMap,
    entryBlock,
    exitBlock
)
{
    // The current statement entry block is the entry block
    var curEntry = entryBlock;

    // For each statement
    for (var i = 0; i < stmtList.length; ++i)
    {
        var stmt = stmtList[i];

        // If the statement is a function declaration
        if (stmt instanceof FunctionDeclaration)
        {
            // Compile the function
            var newFunc = FuncToIR(
                stmt.id.toString(),
                cfg.ownerFunc,
                stmt.funct
            );

            // Make the new function a child of the function being compiled
            cfg.ownerFunc.addChildFunc(newFunc);
        }

        // Otherwise, for executable statements
        else
        {
            // Create a block for the statement's exit
            var curExit = cfg.getNewBlock();

            // Generate code for the statement
            StmtToIR(
                stmt,
                cfg,
                localsMap,
                curEntry,
                curExit
            );

            // Make the current exit the next entry block
            curEntry = curExit;
        }
    }

    // Jump from the current entry to the exit block
    curEntry.addInstr(new JumpInstr(exitBlock));
}

/**
Convert an AST statement into IR code
*/
function StmtToIR(
    astStmt,
    cfg,
    localsMap,
    entryBlock,
    exitBlock
)
{
    if (astStmt instanceof BlockStatement)
    {
        // Compile the statement list
        StmtsToIR(
            astStmt.statements, 
            cfg,
            localsMap,
            entryBlock,
            exitBlock
        );
    }

    else if (astStmt instanceof ConstStatement)
    {
        assert (false, 'ConstStatement not implemented');
    }

    else if (astStmt instanceof ExprStatement)
    {
        // Compile the expression
        ExprToIR(
            astStmt.expr, 
            cfg,
            localsMap,
            entryBlock,
            exitBlock
        );
    }

    else if (astStmt instanceof IfStatement)
    {
        // TODO: conversion to boolean, need ToBoolean()
        // False is: null, undefined, 0, NaN, ''
        // Could implement tobool instruction? Or leave this as detail for lower level conversion
        //
        // PROBABLY best to have explicit conversions which get removed later
        // These map to inlinable functions which have different cases for each type
        //
        // What about indexing and ToString!? We don't actually want a string!
        // Could possibly have custom tokey conversion... Or no conversion,
        // getprop_val already implies complex underlying algorithm, unlike if test

        // Compile the test expression
        var testExit = cfg.getNewBlock();
        var testVal = ExprToIR(
            astStmt.expr,
            cfg,
            localsMap,
            entryBlock,
            testExit
        );

        // Create a block for the joining of the if branches
        var joinBlock = cfg.getNewBlock('if_join');

        // Compile the true statement
        var trueEntry = cfg.getNewBlock('if_true');
        var trueExit = cfg.getNewBlock();
        var trueLocals = localsMap.copy();
        StmtToIR(
            astStmt.statements[0],
            cfg,
            trueLocals,
            trueEntry,
            trueExit
        );

        // Compile the true statement
        var falseEntry = cfg.getNewBlock('if_false');
        var falseExit = cfg.getNewBlock();
        var falseLocals = localsMap.copy();
        StmtToIR(
            astStmt.statements[1],
            cfg,
            falseLocals,
            falseEntry,
            falseExit
        );

        // Branch from the true and false exit blocks jump to the join block
        trueExit.addInstr(new JumpInstr(joinBlock));
        falseExit.addInstr(new JumpInstr(joinBlock));

        // Merge the local maps using phi nodes
        mergeLocals(
            [
                new JoinPoint(trueExit, trueLocals), 
                new JoinPoint(falseExit, falseLocals)
            ],
            joinBlock,
            localsMap
        );

        // Create the if branching instruction
        testExit.addInstr(new IfInstr(testVal, trueEntry, falseEntry));       

        // Jump from the join block to the exit block
        joinBlock.addInstr(new JumpInstr(exitBlock));
    }


    /*
    else if (astStmt instanceof DoWhileStatement)
    {
        ast.statement = ctx.walk_statement(ast.statement);
        ast.expr = ctx.walk_expr(ast.expr);
        return ast;
    }

    else if (astStmt instanceof WhileStatement)
    {
        ast.expr = ctx.walk_expr(ast.expr);
        ast.statement = ctx.walk_statement(ast.statement);
        return ast;
    }

    else if (astStmt instanceof ForStatement)
    {
        ast.expr1 = ctx.walk_expr(ast.expr1);
        ast.expr2 = ctx.walk_expr(ast.expr2);
        ast.expr3 = ctx.walk_expr(ast.expr3);
        ast.statement = ctx.walk_statement(ast.statement);
        return ast;
    }

    else if (astStmt instanceof ForInStatement)
    {
        ast.lhs_expr = ctx.walk_expr(ast.lhs_expr);
        ast.set_expr = ctx.walk_expr(ast.set_expr);
        ast.statement = ctx.walk_statement(ast.statement);
        return ast;
    }

    else if (astStmt instanceof ContinueStatement)
    {
        return ast;
    }

    else if (astStmt instanceof BreakStatement)
    {
        return ast;
    }
    */

    else if (astStmt instanceof ReturnStatement)
    {
        // Compile the return expression
        var retExit = cfg.getNewBlock();
        var retVal = ExprToIR(
            astStmt.expr,
            cfg,
            localsMap,
            entryBlock,
            retExit
        );

        // Add a return instruction
        retExit.addInstr(new RetInstr(retVal));
    }

    /*
    else if (astStmt instanceof WithStatement)
    {
        ast.expr = ctx.walk_expr(ast.expr);
        ast.statement = ctx.walk_statement(ast.statement);
        return ast;
    }

    else if (astStmt instanceof SwitchStatement)
    {
        ast.expr = ctx.walk_expr(ast.expr);
        ast.clauses.forEach(function (c, i, asts)
                            {
                                c.expr = ctx.walk_expr(c.expr);
                                c.statements = ast_walk_statements(c.statements, ctx);
                            });
        return ast;
    }

    else if (astStmt instanceof LabelledStatement)
    {
        ast.statement = ctx.walk_statement(ast.statement);
        return ast;
    }

    else if (astStmt instanceof ThrowStatement)
    {
        ast.expr = ctx.walk_expr(ast.expr);
        return ast;
    }

    else if (astStmt instanceof TryStatement)
    {
        ast.statement = ctx.walk_statement(ast.statement);
        ast.catch_part = ctx.walk_statement(ast.catch_part);
        ast.finally_part = ctx.walk_statement(ast.finally_part);
        return ast;
    }

    else if (astStmt instanceof DebuggerStatement)
    {
        return ast;
    }
    */

    else
    {
        // Temporary, for unimplemented statements, jump to the exit block to maintain a valid CFG
        entryBlock.addInstr(new JumpInstr(exitBlock));

        // TODO
        // error("UNKNOWN AST");
    }
}

/**
Convert an AST expression list into IR code
@returns a list of values for the evaluated expressions
*/
function ExprsToIR(
    exprList,
    cfg,
    localsMap,
    entryBlock,
    exitBlock
)
{
    // The current argument entry block is the entry block
    var curEntry = entryBlock;

    // Create a list for the expression values
    var exprVals = [];

    // For each expression
    for (var i = 0; i < exprList.length; ++i)
    {
        var expr = exprList[i];

        // Create a block for the expression's exit
        var curExit = cfg.getNewBlock();

        // Generate code for the argument expression
        exprVals.push(
            ExprToIR(
                expr,
                cfg,
                localsMap,
                curEntry,
                curExit
            )
        );

        // Make the current exit the next entry block
        curEntry = curExit;
    }

    // Jump from the current entry to the exit block
    curEntry.addInstr(new JumpInstr(exitBlock));

    // Return the expression value list
    return exprVals;
}

/**
Convert an AST expression into IR code
@returns the value of the evaluated expression
*/
function ExprToIR(
    astExpr,
    cfg,
    localsMap,
    entryBlock,
    exitBlock
)
{
    // TODO

    if (false)
    {
    }

    if (astExpr instanceof OpExpr)
    {
        /*
        if (astExpr.op == 'x && y' || astExpr.op == 'x || y')
        {
            // TODO
            entryBlock.addInstr(new JumpInstr(exitBlock));
            return new UndefConst();
        }
        */        

        // Compile the argument values
        var argsExit = cfg.getNewBlock();
        var argVals = ExprsToIR(
            astExpr.exprs,
            cfg,
            localsMap,
            entryBlock,
            argsExit
        );

        // Variable to store the operator's output value
        var opVal;

        // Switch on the operator
        switch (astExpr.op)
        {
            case 'x < y':
            opVal = argsExit.addInstr(new CompInstr(CompOp.LT, argVals[0], argVals[1]));
            break;

            case 'x + y':
            opVal = argsExit.addInstr(new ArithInstr(ArithOp.ADD, argVals[0], argVals[1]));
            break;

            case 'x - y':
            opVal = argsExit.addInstr(new ArithInstr(ArithOp.SUB, argVals[0], argVals[1]));
            break;

            // TODO
            default:
            opVal = new UndefConst();
        }

        // Jump to the exit block
        argsExit.addInstr(new JumpInstr(exitBlock));

        // Return the operator's output value
        return opVal;
    }

    /*
    else if (astExpr instanceof NewExpr)
    {
        ast.expr = ctx.walk_expr(ast.expr);
        ast.args = ast_walk_exprs(ast.args, ctx);
        return ast;
    }
    */

    else if (astExpr instanceof CallExpr)
    {
        // Variable for the function value
        var funcVal;

        // Variable for the "this" reference value
        var thisVal;

        // Variable for the current exit block
        var fnExit;

        // If the function expression is of the form x[y]
        if (astExpr.fn instanceof OpExpr && astExpr.fn.op == 'x [ y ]')
        {
            var thisExpr = astExpr.fn.exprs[0];
            var idxExpr = astExpr.fn.exprs[1];
    
            // Generate code for the "this" expression
            var thisExit = cfg.getNewBlock();
            thisVal = ExprToIR(
                thisExpr,
                cfg,
                localsMap,
                entryBlock,
                thisExit
            );

            // Generate code for the index expression
            fnExit = cfg.getNewBlock();
            var idxVal = ExprToIR(
                idxExpr,
                cfg,
                localsMap,
                thisExit,
                fnExit
            );

            // Get the function property from the object
            funcVal = fnExit.addInstr(new GetPropValInstr(thisVal, idxVal));
        }
        else
        {
            // Create a block for the function expression exit
            fnExit = cfg.getNewBlock();

            // Generate code for the statement
            funcVal = ExprToIR(
                astExpr.fn,
                cfg,
                localsMap,
                entryBlock,
                fnExit
            );

            // The this value is null
            thisVal = new NullConst();
        }

        // Compile the function argument list
        var argsExit = cfg.getNewBlock();
        var argVals = ExprsToIR(
            astExpr.args,
            cfg,
            localsMap,
            fnExit,
            argsExit
        );

        // Create the call instruction
        var exprVal = argsExit.addInstr(new CallRefInstr(funcVal, thisVal, argVals));

        // Jump from the current entry to the exit block
        argsExit.addInstr(new JumpInstr(exitBlock));

        // Return the expression value
        return exprVal;
    }

    /*
    else if (astExpr instanceof FunctionExpr)
    {
        ast.body = ast_walk_statements(ast.body, ctx);
        return ast;
    }
    */

    // Constant values
    else if (astExpr instanceof Literal)
    {
        var constValue;

        if (typeof astExpr.value == 'string')
        {
            constValue = new StrConst(astExpr.value);
        }
        else if (typeof astExpr.value == 'number')
        {
            if (astExpr.value == parseInt(astExpr.value))
                constValue = new IntConst(astExpr.value);
            else
                constValue = new FPConst(astExpr.value);
        }
        else if (typeof astExpr.value == 'boolean')
        {
            constValue = new BoolConst(astExpr.value);
        }
        else
        {
            assert (false, 'invalid constant value: ' + astExpr.value);
        }

        // Jump to the exit block
        entryBlock.addInstr(new JumpInstr(exitBlock));

        // Return the variable's value
        return constValue;
    }

    /*
    else if (astExpr instanceof ArrayLiteral)
    {
        ast.exprs = ast_walk_exprs(ast.exprs, ctx);
        return ast;
    }

    else if (astExpr instanceof ObjectLiteral)
    {
        ast.properties.forEach(function (prop, i, self)
                               {
                                   prop.name = ctx.walk_expr(prop.name);
                                   prop.value = ctx.walk_expr(prop.value);
                               });
        return ast;
    }
    */

    // Symbol expression
    else if (astExpr instanceof Ref)
    {
        var symName = astExpr.id.toString();

        var varValue;

        // If the variable is global
        if (astExpr.id.scope instanceof Program)
        {
            // Get the value from the global object
            varValue = entryBlock.addInstr(
                new GetPropValInstr(
                    new GlobalRefConst(),
                    new StrConst(symName)
                )
            );
        }

        // Otherwise, the variable is local
        else
        {
            assert (
                localsMap.hasItem(symName), 
                'local variable not in locals map: ' + symName
            );

            // Lookup the variable in the locals map
            varValue = localsMap.getItem(symName);
        }

        // Jump to the exit block
        entryBlock.addInstr(new JumpInstr(exitBlock));

        // Return the variable's value
        return varValue;
    }

    /*
    else if (astExpr instanceof This)
    {
        return ast;
    }
    */

    else
    {
        // Temporary, for unimplemented expressions, jump to the exit block to maintain a valid CFG
        // Also return an undefined value
        entryBlock.addInstr(new JumpInstr(exitBlock));
        return new UndefConst();

        // TODO
        // error("UNKNOWN AST");
    }    
}

/**
@class State before a branch merge point
*/
function JoinPoint(block, localsMap)
{
    /**
    Block jumping to the merge point
    @field
    */
    this.block = block;

    /**
    Map of locals at the join point
    @field
    */
    this.localsMap = localsMap;
}

/**
Merge local variables locations using phi nodes
*/
function mergeLocals(pointList, mergeBlock, mergeMap)
{
    // Ensure that at least one join point was specified
    assert (
        pointList.length > 0,
        'no join points provided for merge'
    );

    // Clear the contents of the merge map, if any
    mergeMap.clear();

    // Get the keys from the first join point
    var keys = pointList[0].localsMap.getKeys();

    // For each local
    for (var i = 0; i < keys.length; ++i)
    {
        var varName = keys[i];

        // Create arrays for the incoming values and corresponding predecessors
        var values = [];
        var preds = [];

        // For each join point
        for (var j = 0; j < pointList.length; ++j)
        {
            values.push(pointList[j].localsMap.getItem(varName));
            preds.push(pointList[j].block);                  
        }

        // Create a phi node for this variable
        var phiNode = new PhiInstr(values, preds);

        // Add the phi node to the merge block
        mergeBlock.addInstr(phiNode);

        // Add the phi node to the merge map
        mergeMap.addItem(varName, phiNode);
    }
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


