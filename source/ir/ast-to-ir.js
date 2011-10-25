/* _________________________________________________________________________
 *
 *             Tachyon : A Self-Hosted JavaScript Virtual Machine
 *
 *
 *  This file is part of the Tachyon JavaScript project. Tachyon is
 *  distributed at:
 *  http://github.com/Tachyon-Team/Tachyon
 *
 *
 *  Copyright (c) 2011, Universite de Montreal
 *  All rights reserved.
 *
 *  This software is licensed under the following license (Modified BSD
 *  License):
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions are
 *  met:
 *    * Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *    * Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions and the following disclaimer in the
 *      documentation and/or other materials provided with the distribution.
 *    * Neither the name of the Universite de Montreal nor the names of its
 *      contributors may be used to endorse or promote products derived
 *      from this software without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 *  IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 *  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 *  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL UNIVERSITE DE
 *  MONTREAL BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 *  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 *  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 *  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 *  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 *  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 *  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * _________________________________________________________________________
 */

/**
@fileOverview
Intermediate Representation (IR) translation implementation

@author
Maxime Chevalier-Boisvert
*/

// TODO: Explain result of translation in function description comments 

// TODO: handle eval
// - Make all local vars shared cells
// - Need to store params in shared cells too

// TODO: fix scope of catch variable
// TODO: use id directly (unique) instead of variable name?

/**
Translate an AST code unit into IR functions
@astUnit AST of the source unit to translate
@params Compilation parameters
*/
function unitToIR(
    astUnit,
    params
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
    assert (
        astUnit instanceof Program, 
        'top-level AST must be program'
    );

    // Ensure that the compilation parameters are valid
    assert (
        params instanceof CompParams,
        'expected compilation parameters'
    );

    // Make a function name from the file name
    var fileName = astUnit.loc.filename;
    var funcName = '';
    for (var i = 0; i < fileName.length; ++i)
    {
        var ch = fileName.charAt(i);
        if ((ch >= '0' && ch <= '9') ||
            (ch >= 'a' && ch <= 'z') ||
            (ch >= 'A' && ch <= 'Z'))
            funcName += ch;
        else
            funcName += '_';
    }

    // Treat the top-level unit as a function
    return stmtListToIRFunc(
        funcName,
        null,
        astUnit.vars,
        [],
        [],
        astUnit.funcs,
        astUnit.block.statements,
        astUnit,
        params
    );
}

/**
Convert an AST function into an IR function
*/
function funcToIR(
    funcName,
    parentFunc,
    astFunc,
    params
)
{
    // Ensure that the top-level AST is a program
    assert (astFunc instanceof FunctionExpr,
            'function must be AST function expression');

    // Compile the function
    return stmtListToIRFunc(
        funcName, 
        parentFunc,
        astFunc.vars,
        astFunc.clos_vars,
        astFunc.esc_vars,
        astFunc.funcs,
        astFunc.body,
        astFunc,
        params
    );
}

/**
Convert an AST statement list into an IR function
*/
function stmtListToIRFunc(
    funcName, 
    parentFunc,
    localVars,
    freeVars,
    escapeVars,
    nestedFuncs,
    bodyStmts,
    astNode,
    params
)
{
    //print('Generating IR for function: "' + funcName + '"');

    // Create a new function object for the function
    var newFunc = getIRFuncObj(
        funcName,
        parentFunc,
        astNode
    );

    // Create a new CFG for the function
    var cfg = new ControlFlowGraph(newFunc);

    // Set the CFG for the function
    newFunc.virginCFG = cfg;

    // Get the entry block for the CFG
    var entryBlock = cfg.getEntryBlock();

    // Current argument index
    var argIndex = 0;

    // If this is a C proxy function
    if (newFunc.cProxy === true)
    {
        // The function object and this value are undefined
        var funcObj = IRConst.getConst(undefined);
        var thisVal = IRConst.getConst(undefined);
    }
    else
    {
        // Add an instruction to get the function object argument
        var funcObj = new ArgValInstr(IRType.box, 'funcObj', argIndex++);
        entryBlock.addInstr(funcObj, 'funcObj');

        // Add an instruction to get the this value argument
        var thisVal = new ArgValInstr(IRType.box, 'thisVal', argIndex++);
        entryBlock.addInstr(thisVal, 'this');
    }

    // Create a map for the local variable storage locations
    var localMap = new HashMap();

    // For each function argument
    for (var i = 0; i < newFunc.argVars.length; ++i)
    {
        var symName = newFunc.argVars[i].toString();

        // Create an instruction to get the argument value
        var argVal = new ArgValInstr(newFunc.argTypes[i], symName, argIndex++);
        entryBlock.addInstr(argVal, symName);

        // Add the argument value to the local map
        localMap.set(symName, argVal);
    }

    // For each local variable declaration
    for (var i in localVars)
    {
        var symName = localVars[i].toString();

        // If there is no local map entry for this variable
        if (localMap.has(symName) === false)
        {
            // Add an undefined value to the local map
            localMap.set(symName, IRConst.getConst(undefined));
        }
    }

    // Create a map for the closure and escaping variable mutable cells
    var sharedMap = new HashMap();

    // Create a context for the function body
    var bodyContext = new IRConvContext(
        bodyStmts, 
        entryBlock,
        null,
        null,
        [],
        localMap,
        sharedMap,
        new HashMap(),
        new HashMap(),
        null,
        cfg,
        funcObj,
        thisVal,
        params
    );

    // If the function uses the arguments object
    if (newFunc.usesArguments)
    {
        // Add an instruction to get the number of arguments
        var numArgs = new GetNumArgsInstr();
        bodyContext.addInstr(numArgs, 'numArgs');

        // Add an instruction to get the arguments table
        var argTable = new GetArgTableInstr();
        bodyContext.addInstr(argTable, 'argTable');

        // Create the arguments object and add it to the variable map
        var argObj = insertPrimCallIR(
            bodyContext, 
            'makeArgObj', 
            [funcObj, numArgs, argTable]
        );
        localMap.set('arguments', argObj);
    }

    // For each closure variable
    for (var i = 0; i < newFunc.closVars.length; ++i)
    {
        var symName = newFunc.closVars[i];

        // Get the corresponding mutable cell from the closure
        var closCell = insertPrimCallIR(
            bodyContext, 
            'get_clos_cells', 
            [
                funcObj,
                IRConst.getConst(i, IRType.pint)
            ]
        );

        // Add the mutable cell to the shared variable map
        sharedMap.set(symName, closCell);
    }

    // For each escaping variable
    for (var i in escapeVars)
    {
        var symName = escapeVars[i].toString();

        // If this variable is not already provided by the local function object
        if (sharedMap.has(symName) === false)
        {
            // Create a new mutable cell for this variable
            var newCell = insertPrimCallIR(
                bodyContext,
                'makeCell', 
                []
            );

            // Map the variable to the mutable cell
            sharedMap.set(symName, newCell);
        }
    }

    // For each nested function
    for (var i in nestedFuncs)
    {
        var nestFuncAst = nestedFuncs[i];

        var nestFuncName;
        var nestFuncExpr;

        if (nestFuncAst instanceof FunctionDeclaration)
        {
            nestFuncName = nestFuncAst.id.toString();
            nestFuncExpr = nestFuncAst.funct;
        }
        else
        {
            nestFuncName = 'anon_' + (stmtListToIRFunc.nextAnonNum)++;
            nestFuncExpr = nestFuncAst;
        }

        // Translate the nested function to IR
        var nestFunc = funcToIR(
            nestFuncName,
            newFunc,
            nestFuncExpr,
            params
        );
        
        // Make the new function a child of the function being compiled
        newFunc.addChildFunc(nestFunc);

        // If the nested function is a function declaration
        if (nestFuncAst instanceof FunctionDeclaration)
        {
            // Create a list for the closure variable values
            var closVals = [];

            // For each closure variable of the new function
            for (var i = 0; i < nestFunc.closVars.length; ++i)
            {
                var symName = nestFunc.closVars[i];

                // Add the variable to the closure variable values
                closVals.push(sharedMap.get(symName));
            }

            // Create a closure for the function
            var closVal = insertPrimCallIR(
                bodyContext, 
                'makeClos', 
                [
                    nestFunc,
                    IRConst.getConst(closVals.length, IRType.pint)
                ]
            );

            // Write the closure variables into the closure
            for (var i = 0; i < closVals.length; ++i)
            {
                insertPrimCallIR(
                    bodyContext, 
                    'set_clos_cells',
                    [
                        closVal, 
                        IRConst.getConst(i, IRType.pint), 
                        closVals[i]
                    ]
                );
            }

            // Map the function name to the closure in the local variable map
            localMap.set(nestFuncName, closVal);
        }
    }

    // For each escaping variable
    for (var i in escapeVars)
    {
        var symName = escapeVars[i].toString();

        // If there is a local map entry for this symbol
        if (localMap.has(symName) === true)
        {
            // Get the mutable cell for this symbol
            var cell = sharedMap.get(symName);

            // Put the current value of the symbol into the cell
            insertPrimCallIR(
                bodyContext,
                'set_cell_val',
                [cell, localMap.get(symName)]
            );

            // Remove the symbol from the local map
            localMap.rem(symName);
        }
    }

    // If the current function is a unit level function
    if (astNode instanceof Program)
    {
        // Get the global object
        var globalObj = insertGetGlobal(bodyContext);

        // For each variable in the local variable map
        for (var itr = localMap.getItr(); itr.valid(); itr.next())
        {
            var varName = itr.get().key;
            var varVal = itr.get().value;

            // If the variable's initial value is undefined
            if (varVal === IRConst.getConst(undefined))
            {
                // Check if the property exists on the global object
                var hasProp = insertPrimCallIR(
                    bodyContext, 
                    'hasPropVal', 
                    [globalObj, IRConst.getConst(varName)]
                );

                // If the property doesn't exist, initialize it to undefined
                insertCondIR(
                    bodyContext,
                    hasProp,
                    undefined,
                    function (condCtx)
                    {
                        insertPrimCallIR(
                            condCtx,
                            'putPropVal', 
                            [globalObj, IRConst.getConst(varName), varVal]
                        );
                    }
                );
            }
            else
            {
                // Bind the variable's initial value in the global environment
                insertPrimCallIR(
                    bodyContext,
                    'putPropVal', 
                    [globalObj, IRConst.getConst(varName), varVal]
                );
            }
        }
    }

    //print('generating IR for function body');
    //if (astNode.loc instanceof Location) pp_loc(astNode.loc, "");

    // Generate code for the function body
    stmtListToIR(bodyContext);

    // If the context is not terminated and this function has a boxed return value
    if (!bodyContext.isTerminated() && newFunc.retType === IRType.box)
    {
        // Add a return undefined instruction to the exit block
        bodyContext.addInstr(
            new RetInstr(IRConst.getConst(undefined))
        );
    }

    //print('done generating IR');
    //print(cfg.ownerFunc);

    // Remove dead blocks from the CFG
    cfg.remDeadBlocks();

    if (DEBUG)
        cfg.validate();

    //print('Applying opt patterns');

    // Simplify the CFG using peephole patterns
    applyPatternsCFG(cfg, params);

    //print('validating');    

    // Run a validation test on the CFG
    if (DEBUG)
    {
        try
        {
            cfg.validate();
        }
        catch (e)
        {
            error(
                'Invalid CFG for function "' + funcName + '":\n' + 
                e + '\n' + cfg.toString()
            );
        }
    }

    //print(newFunc);

    //print('done generating IR for function');

    // Return the new function
    return newFunc;
}

/**
Next number to be assigned to an anonymous function
*/
stmtListToIRFunc.nextAnonNum = 0;

/**
Create or get the IR function object for an AST node
*/
function getIRFuncObj(
    funcName, 
    parentFunc, 
    astNode
)
{
    // If a function object already exists, return it
    if (astNode.irFunc !== undefined)
        return astNode.irFunc;

    // Arrays for the argument and closure variables
    var argVars = [];
    var closVars = [];

    // Array for the prologue annotations
    var annotations = [];

    // If this is a top-level program node
    if (astNode instanceof Program)
    {
        // Nothing for now
    }

    // If this is a function expression
    else if (astNode instanceof FunctionExpr)
    {
        // Extract the argument names
        for (var i in astNode.params)
            argVars.push(astNode.params[i].toString());

        // Extract the closure variable names
        for (var i in astNode.clos_vars)
            closVars.push(astNode.clos_vars[i].toString());

        // Extract the function annotations
        for (var i in astNode.annotations)
            annotations.push(astNode.annotations[i].value);
    }

    // Create a new function object for the function
    var newFunc = new IRFunction(
        funcName,
        argVars,
        closVars,
        undefined,
        undefined,
        parentFunc,
        astNode
    );

    // Verify if the function may be using the arguments object or eval
    if (astNode.usesArguments)
        newFunc.usesArguments = true;
    if (astNode.usesEval)
        newFunc.usesEval = true;

    // For each annotation
    for (var i = 0; i < annotations.length; ++i)
    {
        var annotation = annotations[i];

        var tokens = annotation.split(':');
        if (tokens.length < 2 || tokens[0] !== 'tachyon')
            continue;

        var tokens = tokens[1].split(' ');
        
        // If this is a C proxy function
        if (tokens.length === 1 && tokens[0] === 'cproxy')
        {
            newFunc.cProxy = true;
        }

        // If this is a static linkage annotation
        else if (tokens.length === 1 && tokens[0] === 'static')
        {
            newFunc.staticLink = true;
        }

        // If this is an inline annotation
        else if (tokens.length === 1 && tokens[0] === 'inline')
        {
            newFunc.inline = true;
            newFunc.staticLink = true;
        }

        // If this is a no global accesses annotation
        else if (tokens.length === 1 && tokens[0] === 'noglobal')
        {
            newFunc.noGlobal = true;
        }

        // If this is an argument type annotation (eg: arg <arg_name> <type>)
        else if (tokens.length === 3 && tokens[0] === 'arg')
        {
            var argName = tokens[1];
            var type = IRType[tokens[2]];

            var argNo = -1;
            for (var j = 0; j < newFunc.argVars.length; ++j)
            {
                if (newFunc.argVars[j].toString() === argName)
                {
                    argNo = j;
                    break;
                }
            }

            if (type !== IRType.box && newFunc.usesArguments)
                error(
                    'functions taking non-boxed arguments cannot ' + 
                    'use the arguments object'
                );

            if (argNo === -1)
                error('invalid argument name in argument type annotation');

            if (!type)
                error(
                    'invalid type in argument type annotation (' + 
                    funcName + ')'
                );

            newFunc.argTypes[argNo] = type;
        }

        // If this is a return type annotation (eg: ret <type>)
        else if (tokens.length === 2 && tokens[0] === 'ret')
        {
            var type = IRType[tokens[1]];

            if (!type)
                error('invalid type in return type annotation');

            newFunc.retType = type;
        }

        // Otherwise, if the annotation was not recognized
        else
        {
            error('unrecognized annotation: "' + annotation + '"');
        }
    }

    // Store a reference to the new object in the AST node
    astNode.irFunc = newFunc;

    // Return the new function object
    return newFunc;
}

/**
@class IR Conversion context
*/
function IRConvContext(
    astNode, 
    entryBlock,
    ctxNode,
    withVal,
    labels,
    localMap,
    sharedMap,
    breakMap, 
    contMap,
    throwList,
    cfg,
    funcObj,
    thisVal,
    params
)
{
    // Ensure that the arguments are valid
    assert (
        astNode !== undefined,
        'ast node not defined in IR conversion context'
    );
    assert (
        entryBlock !== undefined && entryBlock instanceof BasicBlock,
        'entry block not defined or invalid in IR conversion context'
    );
    assert (
        ctxNode !== undefined,
        'context ast node not defined in IR conversion context'
    );
    assert (
        withVal !== undefined,
        'with context value not defined in IR conversion context'
    );
    assert (
        labels instanceof Array || labels === null,
        'labels not defined or invalid in IR conversion context'
    );
    assert (
        localMap !== undefined,
        'local variable map not defined in IR conversion context'
    );
    assert (
        sharedMap !== undefined,
        'shared variable map not defined in IR conversion context'
    );
    assert (
        breakMap !== undefined,
        'break map not defined in IR conversion context'
    );
    assert (
        contMap !== undefined,
        'continue map not defined in IR conversion context'
    );
    assert (
        throwList !== undefined,
        'throw list not defined in IR conversion context'
    );
    assert (
        cfg !== undefined && cfg instanceof ControlFlowGraph,
        'CFG not defined or invalid in IR conversion context'
    );
    assert (
        funcObj instanceof IRValue,
        'invalid function object in IR conversion context'
    );
    assert (
        thisVal instanceof IRValue || thisVal === null,
        'invalid this value in IR conversion context'
    );
    assert (
        params instanceof CompParams,
        'invalid compilation parameters in IR conversion context'
    );

    /**
    AST node to convert
    @field
    */
    this.astNode = astNode;

    /**
    Entry block at which to begin IR stream output
    @field
    */
    this.entryBlock = entryBlock;

    /**
    Context AST node
    @field
    */
    this.ctxNode = ctxNode;

    /**
    With context value
    @field
    */
    this.withVal = withVal;

    /**
    Labels applied to this statement
    @field
    */
    this.labels = labels;

    /**
    Mutable map of local variable states
    @field
    */
    this.localMap = localMap;

    /**
    Map of shared variable locations
    @field
    */
    this.sharedMap = sharedMap;

    /**
    Break context lists map
    @field
    */
    this.breakMap = breakMap;

    /**
    Continue context lists map
    @field
    */
    this.contMap = contMap;

    /**
    Throw context list
    @field
    */
    this.throwList = throwList;

    /**
    Control-flow graph to which the generate code belongs
    @field
    */
    this.cfg = cfg;

    /**
    Function object value
    @field
    */
    this.funcObj = funcObj;

    /**
    This argument value
    @field
    */
    this.thisVal = thisVal;

    /**
    Current basic block at the output
    @field
    */
    this.exitBlock = undefined;

    /**
    Output value of the evaluated AST node
    @field
    */
    this.outValue = undefined;

    /**
    Flag to indicate that we are compiling tachyon code
    @field
    */
    this.params = params;
}
IRConvContext.prototype = {};

/**
Set the output of the IR conversion
*/
IRConvContext.prototype.setOutput = function (exitBlock, outValue)
{
    // Ensure that the arguments are valid
    assert (
        exitBlock !== undefined,
        'exit block not defined for IR conversion context output'
    );
    assert (
        outValue === undefined || outValue instanceof IRValue,
        'invalid value specified for IR conversion context output'
    );

    this.exitBlock = exitBlock;

    this.outValue = outValue;
};

/**
Stitch the last block of a non-linear code sequence into the current context
*/
IRConvContext.prototype.splice = function (contBlock)
{
    // Ensure that the arguments are valid
    assert (
        contBlock !== undefined,
        'continuation block not defined for IR conversion context splice'
    );

    if (this.exitBlock !== undefined)
        this.exitBlock = contBlock;
    else
        this.entryBlock = contBlock;
};

/**
Terminate a context so it cannot be pursued
*/
IRConvContext.prototype.terminate = function ()
{
    this.exitBlock = null;
};

/**
Bridge a context with no exit block
*/
IRConvContext.prototype.bridge = function ()
{
    if (this.exitBlock === undefined)
        this.exitBlock = this.entryBlock;
};

/**
Get the exit block from a conversion context
*/
IRConvContext.prototype.getExitBlock = function ()
{
    // Ensure that the output was set
    assert (
        this.exitBlock !== undefined,
        'cannot get exit block from conversion context, output not set'
    );

    return this.exitBlock;
};

/**
Get the output value from a conversion context
*/
IRConvContext.prototype.getOutValue = function ()
{
    // Ensure that the output was set
    assert (
        this.outValue instanceof IRValue,
        'cannot get output value from conversion context, output not set'
    );

    return this.outValue;
};

/**
Test if a context is terminated
*/
IRConvContext.prototype.isTerminated = function (astNode)
{
    return this.exitBlock === null;
};

/**
Create a new context to pursue the conversion of a sequential
unit of code for which a previous context exits
*/
IRConvContext.prototype.pursue = function (astNode)
{
    // Ensure that the context is not terminated
    assert (
        !this.isTerminated(),
        'cannot pursue terminated context'
    );

    return new IRConvContext(
        astNode,
        (this.exitBlock !== undefined) ? this.exitBlock : this.entryBlock,
        this.ctxNode,
        this.withVal,
        [],
        this.localMap,
        this.sharedMap,
        this.breakMap,
        this.contMap,
        this.throwList,
        this.cfg,
        this.funcObj,
        this.thisVal,
        this.params
    );
};

/**
Create a new context for the conversion of a branch
*/
IRConvContext.prototype.branch = function (
    astNode,
    entryBlock,
    localMap
)
{
    return new IRConvContext(
        astNode,
        entryBlock,
        this.ctxNode,
        this.withVal,
        [],
        localMap,
        this.sharedMap,
        this.breakMap,
        this.contMap,
        this.throwList,
        this.cfg,
        this.funcObj,
        this.thisVal,
        this.params
    );
};

/**
Add an instruction at the end of the current block of this context
*/
IRConvContext.prototype.addInstr = function (instr, outName)
{
    var insBlock = (this.exitBlock !== undefined)
                   ? this.exitBlock
                   : this.entryBlock;

    insBlock.addInstr(instr, outName);

    return instr;
};

/**
Convert a statement list to IR code
*/
function stmtListToIR(context)
{
    // Ensure that the IR conversion context is valid
    assert (
        context instanceof IRConvContext,
        'invalid IR conversion context specified'
    );

    var stmtList = context.astNode;

    // Bridge the current context, in case the statement list is empty
    context.bridge();

    // The current statement context is the entry context
    var curContext = context;

    // For each statement
    for (var i = 0; i < stmtList.length; ++i)
    {
        var stmt = stmtList[i];

        // Pursue the context for the statement
        curContext = curContext.pursue(stmt);

        // Generate code for the statement
        stmtToIR(curContext);

        // If the context is terminated, stop
        if (curContext.isTerminated())
            break;
    }

    // The exit block is the current exit block
    context.setOutput(curContext.getExitBlock());
}

/**
Convert an AST statement into IR code
*/
function stmtToIR(context)
{
    //print('Generating ir for stmt:');
    //pp(context.astNode)

    // Ensure that the IR conversion context is valid
    assert (
        context instanceof IRConvContext,
        'invalid IR conversion context specified'
    );

    // Get a reference to the statement
    var astStmt = context.astNode;

    // If the statement is a function declaration
    if (astStmt instanceof FunctionDeclaration)
    {
        // Do nothing
        context.bridge();
    }

    else if (astStmt instanceof BlockStatement)
    {
        // Compile the statement list
        var blockContext = context.pursue(astStmt.statements);
        stmtListToIR(blockContext);
        context.setOutput(blockContext.getExitBlock());
    }

    else if (astStmt instanceof ConstStatement)
    {
        assert (false, 'ConstStatement not implemented');
    }

    else if (astStmt instanceof ExprStatement)
    {
        // Compile the expression
        var exprContext = context.pursue(astStmt.expr);
        exprToIR(exprContext);
        context.setOutput(exprContext.getExitBlock());
    }

    else if (astStmt instanceof IfStatement)
    {
        // If the test expression is an inline conditional IR instruction
        if (context.params.tachyonSrc && isCondInlineIR(astStmt))
        {
            // Generate the inline IR instruction
            genCondInlineIR(context);

            // Exit early
            return;
        }

        // Compile the test expression
        var testContext = context.pursue(astStmt.expr);        
        exprToIR(testContext);

        // Get the boolean value of the first expression
        var boolVal = insertPrimCallIR(
            testContext, 
            'boxToBool',
            [testContext.getOutValue()]
        );

        // Create a context for the true statement
        var trueContext = context.branch(
            astStmt.statements[0],
            context.cfg.getNewBlock('if_true'),
            testContext.localMap.copy()
        );

        // Create a context for the false statement
        var falseContext = context.branch(
            (astStmt.statements.length > 1) ? astStmt.statements[1] : null,
            context.cfg.getNewBlock('if_false'),
            testContext.localMap.copy()
        );

        // Compile the true statement
        stmtToIR(trueContext);

        // Compile the false statement, if it is defined
        if (astStmt.statements.length > 1)
            stmtToIR(falseContext);
        else
            falseContext.bridge();

        // Merge the local maps using phi nodes
        var joinBlock = mergeContexts(
            [trueContext, falseContext],
            context.localMap,
            context.cfg,
            'if_join'
        );

        // Create the if branching instruction
        testContext.addInstr(
            new IfInstr(
                [boolVal, IRConst.getConst(true)],
                'EQ',
                trueContext.entryBlock,
                falseContext.entryBlock
            )
        );

        // Set the exit block to be the join block
        context.setOutput(joinBlock);
    }

    else if (astStmt instanceof DoWhileStatement)
    {
        // Create a context for the loop entry (the loop body)
        var entryLocals = new HashMap();
        var brkCtxList = [];
        var cntCtxList = [];
        var bodyContext = createLoopEntry(
            astStmt,
            astStmt.statement,
            context,
            entryLocals,
            brkCtxList,
            cntCtxList,
            'loop_body'
        );
              
        // Compile the loop body
        stmtToIR(bodyContext);

        // Add the body exit to the continue context list
        cntCtxList.push(bodyContext);

        // Merge the continue contexts
        var testLocals = new HashMap();
        var testEntry = mergeContexts(
            cntCtxList,
            testLocals,
            context.cfg,
            'loop_test'
        );

        // Compile the loop test
        var testContext = bodyContext.branch(
            astStmt.expr,
            testEntry,
            testLocals
        );
        exprToIR(testContext);

        // Get the boolean value of the test expression
        var boolVal = insertPrimCallIR(
            testContext, 
            'boxToBool',
            [testContext.getOutValue()]
        );

        // Add the test exit to the break context list
        brkCtxList.push(testContext);

        // Merge the break contexts
        var loopExit = mergeContexts(
            brkCtxList,
            context.localMap,
            context.cfg,
            'loop_exit'
        );

        // Replace the jump added by the context merging at the test exit
        // by the if branching instruction
        var testExit = testContext.getExitBlock();
        testExit.replBranch(
            new IfInstr(
                [boolVal, IRConst.getConst(true)],
                'EQ',
                bodyContext.entryBlock,
                loopExit
            )
        );

        // Merge the test exit context with the loop entry
        mergeLoopEntry(
            [testContext],
            entryLocals,
            context.localMap,
            bodyContext.entryBlock
        );

        // Add a jump from the entry block to the loop entry
        context.addInstr(new JumpInstr(bodyContext.entryBlock));

        // Set the exit block to be the join block
        context.setOutput(loopExit);
    }

    else if (astStmt instanceof WhileStatement)
    {
        // Create a context for the loop entry (the loop test)
        var entryLocals = new HashMap();
        var brkCtxList = [];
        var cntCtxList = [];
        var testContext = createLoopEntry(
            astStmt,
            astStmt.expr,
            context,
            entryLocals,
            brkCtxList,
            cntCtxList,
            'loop_test'
        );

        // Compile the loop test in the entry context
        exprToIR(testContext);

        // Get the boolean value of the test expression
        var boolVal = insertPrimCallIR(
            testContext, 
            'boxToBool',
            [testContext.getOutValue()]
        );

        // Compile the body statement
        var bodyContext = testContext.branch(
            astStmt.statement,
            context.cfg.getNewBlock('loop_body'),
            testContext.localMap.copy()
        );
        stmtToIR(bodyContext);

        // Add the test exit to the entry context list
        brkCtxList.push(testContext);

        // Add the body exit to the continue context list
        cntCtxList.push(bodyContext);  

        // Merge the break contexts
        var loopExit = mergeContexts(
            brkCtxList,
            context.localMap,
            context.cfg,
            'loop_exit'
        );

        // Merge the continue contexts with the loop entry
        mergeLoopEntry(
            cntCtxList,
            entryLocals,
            context.localMap,
            testContext.entryBlock
        );

        // Replace the jump added by the context merging at the test exit
        // by the if branching instruction
        var testExit = testContext.getExitBlock();
        testExit.replBranch(
            new IfInstr(
                [boolVal, IRConst.getConst(true)],
                'EQ',
                bodyContext.entryBlock,
                loopExit
            )
        );       

        // Add a jump from the entry block to the loop entry
        context.addInstr(new JumpInstr(testContext.entryBlock));

        // Set the exit block to be the join block
        context.setOutput(loopExit);
    }

    else if (astStmt instanceof ForStatement)
    {
        // Compile the loop initialization expression
        var initContext = context.pursue(astStmt.expr1);
        exprToIR(initContext);

        // Create a context for the loop entry (the loop test)
        var entryLocals = new HashMap();
        var brkCtxList = [];
        var cntCtxList = [];
        initContext.labels = context.labels;
        var testContext = createLoopEntry(
            astStmt,
            astStmt.expr2,
            initContext,
            entryLocals,
            brkCtxList,
            cntCtxList,
            'loop_test'
        );

        // Compile the loop test in the entry context
        exprToIR(testContext);

        // Get the boolean value of the test expression
        var boolVal = insertPrimCallIR(
            testContext, 
            'boxToBool',
            [testContext.getOutValue()]
        );

        // Compile the body statement
        var bodyContext = testContext.branch(
            astStmt.statement,
            context.cfg.getNewBlock('loop_body'),
            testContext.localMap.copy()
        );
        stmtToIR(bodyContext);

        // Add the test exit to the entry context list
        brkCtxList.push(testContext);

        // Add the body exit to the continue context list
        cntCtxList.push(bodyContext); 

        // Merge the break contexts
        var loopExit = mergeContexts(
            brkCtxList,
            context.localMap,
            context.cfg,
            'loop_exit'
        );

        // Merge the continue contexts
        var incrLocals = new HashMap();
        var loopIncr = mergeContexts(
            cntCtxList,
            incrLocals,
            context.cfg,
            'loop_incr'
        );

        // If there were non-terminated continue contexts
        if (loopIncr)
        {
            // Compile the loop incrementation
            var incrContext = testContext.branch(
                astStmt.expr3,
                loopIncr,
                incrLocals
            );
            exprToIR(incrContext);

            // Merge the continue contexts with the loop entry
            mergeLoopEntry(
                [incrContext],
                entryLocals,
                context.localMap,
                testContext.entryBlock
            );
        }        

        // Replace the jump added by the context merging at the test exit
        // by the if branching instruction
        var testExit = testContext.getExitBlock();
        testExit.replBranch(
            new IfInstr(
                [boolVal, IRConst.getConst(true)],
                'EQ',
                bodyContext.entryBlock,
                loopExit
            )
        );       

        // Add a jump from the init exit to the loop entry
        initContext.addInstr(new JumpInstr(testContext.entryBlock));

        // Set the exit block to be the join block
        context.setOutput(loopExit);
    }

    else if (astStmt instanceof ForInStatement)
    {
        // Compile the set expression
        var setCtx = context.pursue(astStmt.set_expr);
        exprToIR(setCtx);

        // Get the property names accessor function
        var propNameFunc = insertPrimCallIR(
            setCtx, 
            'getPropNames', 
            [setCtx.getOutValue()]
        );

        // Get the function pointer from the closure object
        var propNameFuncPtr = insertPrimCallIR(
            setCtx, 
            'get_clos_funcptr', 
            [propNameFunc]
        );

        // Create a context for the loop entry (the loop test)
        var entryLocals = new HashMap();
        var brkCtxList = [];
        var cntCtxList = [];
        setCtx.labels = context.labels;
        var testCtx = createLoopEntry(
            astStmt,
            astStmt,
            setCtx,
            entryLocals,
            brkCtxList,
            cntCtxList,
            'loop_test'
        );
  
        // Bridge the test context
        testCtx.bridge();

        // Get the current property
        var curPropName = insertExceptIR(
        testCtx,
            new CallFuncInstr(
                [
                    propNameFuncPtr, 
                    propNameFunc,
                    IRConst.getConst(undefined)
                ]
            )
        );

        // Create a context for the loop body
        var loopBody = context.cfg.getNewBlock('loop_body');
        var bodyCtx = testCtx.branch(
            astStmt.lhs_expr,
            loopBody,
            testCtx.localMap.copy()
        );

        // Assign the current prop name to LHS expr
        assgToIR(bodyCtx, curPropName);

        // Compile the loop body statement
        var bodyStmtCtx = bodyCtx.pursue(astStmt.statement);
        stmtToIR(bodyStmtCtx);

        // Add the test exit to the break context list
        brkCtxList.push(testCtx);

        // Add the body exit to the continue context list
        cntCtxList.push(bodyStmtCtx); 

        // Merge the break contexts
        var loopExit = mergeContexts(
            brkCtxList,
            context.localMap,
            context.cfg,
            'loop_exit'
        );

        // Merge the continue contexts with the loop entry
        mergeLoopEntry(
            cntCtxList,
            entryLocals,
            context.localMap,
            testCtx.entryBlock
        );

        // Replace the jump added by the context merging at the test exit
        // by an if branching instruction testing that the property is
        // not equal to undefined
        var testExit = testCtx.getExitBlock();
        testExit.replBranch(
            new IfInstr(
                [curPropName, IRConst.getConst(undefined)],
                'NE',
                loopBody,
                loopExit
            )
        );    

        // Add a jump from the entry block to the test block
        setCtx.addInstr(new JumpInstr(testCtx.entryBlock));

        // Set the exit block to be the join block
        context.setOutput(loopExit);
    }

    else if (astStmt instanceof ContinueStatement)
    {
        // Get the label, if one was specified
        var label = (astStmt.label !== null) ? astStmt.label.toString() : '';

        // If there is a continue list for this label
        if (context.contMap.has(label) === true)
        {
            // Create a new context and bridge it
            var newContext = context.pursue(context.astNode);
            newContext.bridge();

            // Add the new context to the list corresponding to this label
            context.contMap.get(label).push(newContext);

            // Terminate the current context, no instructions go after this
            context.terminate();
        }
        else
        {
            // Generate code to throw a syntax error
            insertErrorIR(
                context, 
                'SyntaxError', 
                'continue with invalid label'
            );
        }
    }

    else if (astStmt instanceof BreakStatement)
    {
        // Get the label, if one was specified
        var label = (astStmt.label !== null) ? astStmt.label.toString() : '';

        // If there is a break list for this label
        if (context.breakMap.has(label) === true)
        {
            // Create a new context and bridge it
            var newContext = context.pursue(context.astNode);
            newContext.bridge();

            // Add the new context to the list corresponding to this label
            context.breakMap.get(label).push(newContext);

            // Terminate the current context, no instructions go after this
            context.terminate();
        }
        else
        {
            // Generate code to throw a syntax error
            insertErrorIR(
                context, 
                'SyntaxError', 
                'break with invalid label'
            );
        }
    }

    else if (astStmt instanceof ReturnStatement)
    {
        if (context.cfg.ownerFunc.astNode instanceof Program)
            error('unit-level returns are not allowed');

        // Get the return type for this function
        var retType = context.cfg.ownerFunc.retType;

        // If there is a return expression
        if (astStmt.expr !== null)
        {
            // Compile the return expression
            var retContext = context.pursue(astStmt.expr);
            exprToIR(retContext);

            // Ensure that the type of the returned value is valid
            assert (
                retContext.getOutValue().type === retType,
                'returned value type must match function return type in "' +
                context.cfg.ownerFunc.funcName + '"'
            );

            // Return the expression value
            retContext.addInstr(new RetInstr(retContext.getOutValue()));
        }
        else
        {
            // If the return type is boxed
            if (retType === IRType.box)
            {
                // Return the undefined constant
                context.addInstr(new RetInstr(IRConst.getConst(undefined)));
            }

            // If the return type is none
            else if (retType === IRType.none)
            {
                // Return nothing
                context.addInstr(new RetInstr());
            }

            // For any other return type
            else
            {
                error('functions with return type "' + retType + '" must return a value');
            }
        }

        // Indicate that there is no continuation for this context
        context.terminate();
    }

    else if (astStmt instanceof WithStatement)
    {       
        //
        // TODO: need toObject instruction?
        //

        // Compile the object expression
        var objContext = context.pursue(astStmt.expr);
        exprToIR(objContext);

        // Pursue the context for the statement
        var stmtContext = objContext.pursue(astStmt.statement);

        // Set the with value for the statement context
        stmtContext.withVal = objContext.getOutValue();

        // Compile the statement
        stmtToIR(stmtContext);

        // Set the exit block for the current context
        context.setOutput(stmtContext.getExitBlock());
    }

    else if (astStmt instanceof SwitchStatement)
    {
        // Compile the switch expression
        var switchCtx = context.pursue(astStmt.expr);        
        exprToIR(switchCtx);

        // If there are no clauses in the switch statement, we are done
        if (astStmt.clauses.length === 0)
        {
            context.setOutput(switchCtx.getExitBlock());
            return;
        }

        // Create a list for the break contexts
        var brkCtxList = [];

        // Create a break context map
        var breakMap = context.breakMap.copy();
        breakMap.set('', brkCtxList);
        for (var i = 0; i < context.labels.length; ++i)
            breakMap.set(context.labels[i], brkCtxList);

        // Create a context for the first case test
        var nextTestCtx = context.branch(
            astStmt.clauses[0]? astStmt.clauses[0].expr:null,
            context.cfg.getNewBlock('switch_test_0'),
            switchCtx.localMap.copy()
        );

        // Jump to the first test
        switchCtx.addInstr(new JumpInstr(nextTestCtx.entryBlock));

        // Variable for the previous clause statements context
        var prevStmtCtx = null;

        // Variable for the default clause entry block
        var defaultEntry = null;

        // Local variable map for the default case entry
        var defaultLocals = null;

        // For each clause
        for (var i = 0; i < astStmt.clauses.length; ++i)
        {
            var clause = astStmt.clauses[i];
            var nextClause = astStmt.clauses[i + 1];
 
            // Get the context for the current test
            var curTestCtx = nextTestCtx;

            // Create a context for the next case test
            nextTestCtx = context.branch(
                nextClause? nextClause.expr:null,
                context.cfg.getNewBlock('switch_test_' + (i + 1)),
                curTestCtx.localMap.copy()
            );

            // If this is not the default clause
            if (clause.expr !== null)
            {
                // Generate code for the test expression
                exprToIR(curTestCtx);

                // Insert the HIR instruction
                var testVal = insertExceptIR(
                    curTestCtx,
                    new HIRSeInstr([curTestCtx.getOutValue(), switchCtx.getOutValue()])
                );

                // Merge the incoming contexts
                var caseLocals = curTestCtx.localMap.copy();
                var caseEntry = mergeContexts(
                    prevStmtCtx? [curTestCtx, prevStmtCtx]:[curTestCtx],
                    caseLocals,
                    context.cfg,
                    'switch_case_' + i
                );

                // Create a new context for the clause statements
                var stmtCtx = context.branch(
                    clause.statements,
                    caseEntry,
                    caseLocals
                );
                stmtCtx.breakMap = breakMap;

                // Generate code for the statement
                stmtListToIR(stmtCtx);

                // Replace the merge branch by an if instruction
                curTestCtx.getExitBlock().replBranch(
                    new IfInstr(
                        [testVal, IRConst.getConst(true)],
                        'EQ',
                        stmtCtx.entryBlock,
                        nextTestCtx.entryBlock
                    )
                );
            }

            // Otherwise, this is the default case
            else
            {
                // Bridge the test context
                curTestCtx.bridge();

                // The test evaluates to false
                var testVal = IRConst.getConst(false);

                // Create a new context for the clause statements
                var stmtCtx = createLoopEntry(
                    astStmt,
                    clause.statements,
                    curTestCtx,
                    curTestCtx.localMap.copy(),
                    brkCtxList,
                    null,
                    'switch_case_' + i
                );

                // Store the local map for the default case entry
                defaultLocals = stmtCtx.localMap.copy();

                // Store the entry block for the default clause
                defaultEntry = stmtCtx.entryBlock;

                // Generate code for the statement
                stmtListToIR(stmtCtx);

                // If there was a previous statement, merge its locals at the
                // current clause statement entry
                if (prevStmtCtx && !prevStmtCtx.isTerminated())
                {
                    mergeLoopEntry(
                        [prevStmtCtx],
                        defaultLocals,
                        defaultLocals,
                        stmtCtx.entryBlock
                    );
                }

                // Add the if test instruction
                curTestCtx.addInstr(
                    new IfInstr(
                        [testVal, IRConst.getConst(true)],
                        'EQ',
                        stmtCtx.entryBlock,
                        nextTestCtx.entryBlock
                    )
                );
            }

            // Update the previous statement context
            prevStmtCtx = stmtCtx;
        }

        // Bridge the last test context
        nextTestCtx.bridge();

        // If a default clause was specified
        if (defaultEntry !== null)
        {
            // Merge the context from the default case into the default entry
            mergeLoopEntry(
                [nextTestCtx],
                defaultLocals,
                defaultLocals,
                defaultEntry
            );
        }
        else
        {
            // Add the last test context to the break context list
            brkCtxList.push(nextTestCtx);
        }

        // Add the last clause context to the break context list
        brkCtxList.push(prevStmtCtx);

        // Merge the break contexts
        var switchExit = mergeContexts(
            brkCtxList,
            context.localMap,
            context.cfg,
            'switch_exit'
        );

        // Set the exit block to be the join block
        context.setOutput(switchExit);
    }    

    else if (astStmt instanceof LabelledStatement)
    {
        // Create a new context for the statement
        var stmtContext = context.pursue(astStmt.statement);

        // Add the label to the label list
        stmtContext.labels = context.labels.slice(0);
        stmtContext.labels.push(astStmt.label.toString());

        // Compile the inner statement
        stmtToIR(stmtContext);
        context.setOutput(stmtContext.getExitBlock());
    }

    else if (astStmt instanceof ThrowStatement)
    {
        // Compile the throw expression
        var throwContext = context.pursue(astStmt.expr);
        exprToIR(throwContext);

        // Generate a throw instruction
        throwToIR(context, throwContext, throwContext.getOutValue());
    }

    else if (astStmt instanceof TryStatement)
    {
        // Create a list for all the throw contexts in the try body
        var throwCtxList = [];
               
        // Create a context for the try body
        var tryBodyCtx = context.pursue(astStmt.statement);
        tryBodyCtx.localMap = context.localMap.copy();
        tryBodyCtx.throwList = throwCtxList;

        // Compile the try body statement
        stmtToIR(tryBodyCtx);

        // Merge the throw contexts
        var catchLocals = context.localMap.copy();
        var catchBlock = mergeContexts(
            throwCtxList,
            catchLocals,
            context.cfg,
            'try_catch'
        );

        // For each throw context
        for (var c in throwCtxList)
        {
            var throwExit = throwCtxList[c].getExitBlock();

            // Get the last instruction (the throw instruction) in the block
            var throwInstr = throwExit.getLastInstr();

            // Set the throw target to the catch block
            throwInstr.setThrowTarget(catchBlock);

            // Make the catch block a successor of the throw block
            throwExit.addSucc(catchBlock);
            catchBlock.addPred(throwExit);
        }

        // Create a new context for the catch statement
        var catchCtx = context.branch(
            astStmt.catch_part.statement,
            catchBlock,
            catchLocals
        );

        // TODO: disabled until catch scope issues are fixed in parser
        /*
        // Create a new shared map for the catch block
        catchCtx.sharedMap = context.sharedMap.copy();

        // Set the exception value in a mutable cell
        var catchVal = catchBlock.addInstr(new CatchInstr());
        var catchCell = catchBlock.addInstr(new MakeCellInstr());
        catchBlock.addInstr(new PutCellInstr(catchCell, catchVal));
        catchCtx.sharedMap.set(astStmt.id.toString(), catchCell);
        */

        // Compile the catch statement
        stmtToIR(catchCtx);

        // Merge the finally contexts
        //var finallyLocals = new HashMap();
        var finallyBlock = mergeContexts(
            [tryBodyCtx, catchCtx],
            context.localMap,
            context.cfg,
            'try_finally'
        );

        // Create a context for the finally statement
        var finallyCtx = context.branch(
            astStmt.finally_part,
            finallyBlock,
            context.localMap
        );

        // Compile the finally statement, if it is defined
        if (astStmt.finally_part !== null)
            stmtToIR(finallyCtx);
        else
            finallyCtx.bridge();

        // The exit block is the exit of the finaly context
        context.setOutput(finallyCtx.getExitBlock());
    }

    else if (astStmt instanceof DebuggerStatement)
    {
        // This statement does nothing for now
        context.bridge();
    }

    else
    {
        pp (astStmt);
        assert (false, 'unsupported statement in IR translation');
    }
}

/**
Convert an AST expression list into IR code
@returns a list of values for the evaluated expressions
*/
function exprListToIR(context)
{
    // Ensure that the IR conversion context is valid
    assert (
        context instanceof IRConvContext,
        'invalid IR conversion context specified'
    );

    // Get the expression list
    var exprList = context.astNode;

    // Bridge the current context, in case the expression list is empty
    context.bridge();

    // The current context is the entry context
    var curContext = context;

    // Create a list for the expression values
    var exprVals = [];

    // For each expression
    for (var i = 0; i < exprList.length; ++i)
    {
        var expr = exprList[i];

        // Pursue the context for this expression
        curContext = curContext.pursue(expr);

        // Generate code for the argument expression
        exprToIR(curContext);

        // Add the expression's value to the list
        exprVals.push(curContext.getOutValue());
    }

    // The exit block is the current exit block
    context.setOutput(curContext.getExitBlock());

    // Return the expression value list
    return exprVals;
}

/**
Convert an AST expression into IR code
*/
function exprToIR(context)
{
    //print('Generating ir for expr:');
    //pp(context.astNode)

    // Ensure that the IR conversion context is valid
    assert (
        context instanceof IRConvContext,
        'invalid IR conversion context specified'
    );

    // Get a reference to the expression
    var astExpr = context.astNode;

    // If the expression is null (empty expression)
    if (astExpr === null)
    {
        // Set the output to the true constant
        // This works for empty expression statements and the case where
        // the test expression of a for loop is not specified
        context.setOutput(context.entryBlock, IRConst.getConst(true));
    }

    else if (astExpr instanceof FunctionExpr)
    {
        // Find the compiled nested function corresponding to this expression
        var curFunc = context.cfg.ownerFunc;
        var nestFunc = null;
        for (var f in curFunc.childFuncs)
            if (curFunc.childFuncs[f].astNode === astExpr)
                nestFunc = curFunc.childFuncs[f];

        // Ensure that he nested function was found
        assert (
            nestFunc !== null,
            'nested function not found for function expression'
        );

        // Create a list for the closure variable values
        var closVals = [];

        // For each closure variable of the new function
        for (var i = 0; i < nestFunc.closVars.length; ++i)
        {
            var symName = nestFunc.closVars[i];

            // Add the variable to the closure variable values
            closVals.push(context.sharedMap.get(symName));
        }

        // Create a closure for the function
        var closVal = insertPrimCallIR(
            context, 
            'makeClos', 
            [nestFunc, IRConst.getConst(closVals.length, IRType.pint)]
        );

        // Write the closure variables into the closure
        for (var i = 0; i < closVals.length; ++i)
        {
            insertPrimCallIR(
                context, 
                'set_clos_cells', 
                [
                    closVal, 
                    IRConst.getConst(i, IRType.pint),
                    closVals[i]
                ]
            );
        }

        // Set the output to the new closure
        context.setOutput(context.entryBlock, closVal);
    }

    else if (astExpr instanceof OpExpr)
    {
        // Compile the operator expression
        opToIR(context);
    }

    else if (astExpr instanceof NewExpr)
    {
        // Compile the function argument list
        var argsContext = context.pursue(astExpr.args);
        var argVals = exprListToIR(argsContext);

        // Generate code for the statement
        var funcContext = argsContext.pursue(astExpr.expr);
        exprToIR(funcContext);

        // Create the call instruction
        var exprVal = insertConstructIR(
            funcContext,
            funcContext.getOutValue(),
            argVals
        );

        // Set the output
        context.setOutput(funcContext.getExitBlock(), exprVal);
    }

    else if (astExpr instanceof CallExpr)
    {
        // If this is an assertion and we are not in debug mode
        if (astExpr.fn instanceof Ref && 
            astExpr.fn.id.toString() === 'assert' &&
            context.params.debug === false)
        {
            // Set the undefined value as the context output
            context.setOutput(context.entryBlock, IRConst.getConst(undefined));

            // Exit early
            return;
        }  

        // If this is an inline IR instruction
        if (context.params.tachyonSrc && isInlineIR(astExpr))
        {
            // Generate the inline IR instruction
            genInlineIR(context);

            // Exit early
            return;
        }

        // Compile the function argument list
        var argsContext = context.pursue(astExpr.args);
        var argVals = exprListToIR(argsContext);

        // Variable for the function value
        var funcVal;

        // Variable for the "this" value
        var thisVal;

        // Variable for the last context used
        var lastContext;

        // If the function expression is of the form x[y]
        if (astExpr.fn instanceof OpExpr && astExpr.fn.op === 'x [ y ]')
        {
            var thisExpr = astExpr.fn.exprs[0];
            var idxExpr = astExpr.fn.exprs[1];
    
            // Generate code for the "this" expression
            var thisContext = argsContext.pursue(thisExpr);
            exprToIR(thisContext);

            // Generate code for the index expression
            var idxContext = thisContext.pursue(idxExpr);
            exprToIR(idxContext);

            // Get the function property from the object
            funcVal = insertPrimCallIR(
                idxContext, 
                'getPropVal', 
                [thisContext.getOutValue(), idxContext.getOutValue()]
            );

            // The this value is the result of the this expression evaluation
            thisVal = thisContext.getOutValue();

            lastContext = idxContext;
        }
        else
        {
            // Generate code for the statement
            var funcContext = argsContext.pursue(astExpr.fn);
            funcContext.ctxNode = astExpr;
            exprToIR(funcContext);
            funcVal = funcContext.getOutValue();

            // Get the global object
            var globalObj = insertGetGlobal(funcContext);            

            // The this value is the global object
            thisVal = globalObj;

            lastContext = funcContext;
        }

        // Create the call instruction
        try
        {
            // If this is a static call
            if (funcVal instanceof IRFunction)
            {
                var funcPtr = funcVal;
                var funcObj = IRConst.getConst(undefined);
            }
            else
            {
                // Test if the callee value is a function
                var testVal = insertPrimCallIR(
                    lastContext,
                    'boxIsFunc',
                    [funcVal]
                );

                // Throw an error if the callee is not a function
                insertCondErrorIR(
                    lastContext, 
                    testVal, 
                    'TypeError',
                    'callee is not a function (' + 
                    context.astNode.loc.to_string() + ')'
                );

                // Get the function pointer from the closure object
                var funcPtr = insertPrimCallIR(
                    lastContext, 
                    'get_clos_funcptr', 
                    [funcVal]
                );

                var funcObj = funcVal;
            }

            // Insert the function call
            var exprVal = insertExceptIR(
                lastContext,
                new CallFuncInstr(
                    [funcPtr, funcObj, thisVal].concat(argVals)
                )
            );
        }

        // If an error occurred, rethrow it with source code location information
        catch (exc)
        {
            rethrowError(exc, context.astNode.loc.to_string());
        }

        // Set the output
        context.setOutput(lastContext.getExitBlock(), exprVal);
    }

    // Regular expression literal
    else if (astExpr instanceof RegExpLiteral)
    {
        context.bridge();
        
        // Find the RegExp constructor in the context
        var regexpCtor = insertCtxReadIR(
            context,
            ['regexp']
        );

        // Create the call instruction
        var regexpObj = insertConstructIR(
            context,
            regexpCtor,
            [
                IRConst.getConst(astExpr.pattern),
                IRConst.getConst(astExpr.flags)
            ]
        );

        // Set the output
        context.setOutput(context.getExitBlock(), regexpObj);
    }

    // Constant values
    else if (astExpr instanceof Literal)
    {
        var constValue;

        var constType = typeof astExpr.value;

        if (constType === 'string' || constType === 'number' || 
            constType === 'boolean' || constValue === null || 
            constValue === undefined)
        {
            constValue = IRConst.getConst(astExpr.value);
        }
        else
        {
            assert (false, 'invalid constant value: ' + astExpr.value);
        }

        // Set the constant value as the output
        context.setOutput(context.entryBlock, constValue);
    }

    else if (astExpr instanceof ArrayLiteral)
    {
        //print('Generating IR for array literal');

        // Compile the element value expressions
        var elemCtx = context.pursue(astExpr.exprs);
        var elemVals = exprListToIR(elemCtx);

        // Create a new array
        var newArray = insertPrimCallIR(
            elemCtx, 
            'newArray', 
            [IRConst.getConst(elemVals.length, IRType.pint)]
        );

        // Set the value of each element in the new array
        for (var i = 0; i < elemVals.length; ++i)
        {
            insertPrimCallIR(
                elemCtx, 
                'putPropVal', 
                [newArray, IRConst.getConst(i), elemVals[i]]
            );
        }

        // Set the new array as the output
        context.setOutput(elemCtx.getExitBlock(), newArray);
    }

    else if (astExpr instanceof ObjectLiteral)
    {
        // Get the property names and values
        var propNames = astExpr.properties.map(function (v) { return v.name; });
        var propValues = astExpr.properties.map(function (v) { return v.value; });

        // Compile the property name expressions
        var nameCtx = context.pursue(propNames);
        var nameVals = exprListToIR(nameCtx);

        // Compile the property value expressions
        var valCtx = nameCtx.pursue(propValues);
        var valVals = exprListToIR(valCtx);

        // Find the object prototype object in the context
        var objProto = insertCtxReadIR(
            valCtx,
            ['objproto']
        );

        // Create a new object
        var newObject = insertPrimCallIR(
            valCtx, 
            'newObject', 
            [objProto]
        );

        // Set the value of each property in the new object
        for (var i = 0; i < propNames.length; ++i)
        {
            var propName = nameVals[i];
            var propValue = valVals[i];

            insertPrimCallIR(
                valCtx, 
                'putPropVal', 
                [newObject, propName, propValue]
            );
        }
        
        // Set the new object as the output
        context.setOutput(valCtx.getExitBlock(), newObject);
    }

    // Symbol expression
    else if (astExpr instanceof Ref)
    {
        // Compile the expression
        var exprContext = context.pursue(astExpr);
        refToIR(exprContext);
        context.setOutput(exprContext.getExitBlock(), exprContext.getOutValue());
    }

    else if (astExpr instanceof This)
    {
        // Set the value of the this argument as output
        context.setOutput(
            context.entryBlock,
            context.thisVal
        );
    }

    else
    {
        pp(astExpr);
        assert (false, 'unsupported expression in AST->IR translation');
    }
}

/**
Convert ast operator expression nodes to IR
*/
function opToIR(context)
{
    //print('Generating IR for op expr:');
    //pp(context.astNode);

    // Ensure that the IR conversion context is valid
    assert (
        context instanceof IRConvContext,
        'invalid IR conversion context specified'
    );

    assert (
        context.astNode instanceof OpExpr,
        'expected operator expression'
    );

    // Get references to the operator and the sub-expressions
    var op = context.astNode.op;
    var exprs = context.astNode.exprs;

    // Function to generate code for pre/post increment/decrement operations
    function prePostGen(primName, instrClass, post)
    {
        // Get the variable expression
        var varExpr = exprs[0];

        // Generate IR for the assignee expression
        var fstContext = context.pursue(exprs[0]);
        exprToIR(fstContext);
        
        // Get the pre-incrementation value
        var preVal = fstContext.getOutValue();

        // Get an incrementation value (1) with the same type
        // as the input value
        var incrVal = IRConst.getConst(1, preVal.type);

        // Compute the incremented value
        var postVal = makeOp(
            fstContext,
            primName,
            instrClass,
            [fstContext.getOutValue(), incrVal]
        );
    
        // Assign the incremented value to the variable
        var secContext = fstContext.pursue(varExpr);
        assgToIR(secContext, postVal);

        // Set the output to the pre or post value
        context.setOutput(
            secContext.getExitBlock(),
            (post === true)? preVal:postVal
        );
    }

    // Function to generate code for composite assignment expressions
    function compAssgGen(primName, instrClass)
    {
        // Function to implement the operator code gen
        function opFunc(context, lhsVal)
        {
            // Generate IR for the rhs expression
            var rhsContext = context.pursue(exprs[1]);
            exprToIR(rhsContext);

            // Compute the added value
            var addVal = makeOp(
                rhsContext, 
                primName, 
                instrClass,
                [lhsVal, rhsContext.getOutValue()]
            );

            context.setOutput(rhsContext.getExitBlock(), addVal);
        }
    
        // Assign the added value to the left expression
        var assgContext = context.pursue(exprs[0]);
        assgToIR(assgContext, opFunc);

        // Set the output to the original value
        context.setOutput(assgContext.getExitBlock(), assgContext.getOutValue());
    }

    // Function to generate code for generic unary/binary operators
    function opGen(primName, instrClass)
    {
        // Compile the argument values
        var argsContext = context.pursue(exprs);
        var argVals = exprListToIR(argsContext);

        // Create the appropriate operator instruction
        var opVal = makeOp(argsContext, primName, instrClass, argVals);

        // Set the operator's output value as the output
        context.setOutput(argsContext.getExitBlock(), opVal);
    }

    // Function to create either a primitive call or a machine instruction
    // depending on the argument types
    function makeOp(curContext, prim, instrClass, argVals)
    {
        // Test if all arguments are boxed
        var allBoxed = true;
        for (var i = 0; i < argVals.length; ++i)
            if (argVals[i].type !== IRType.box)
                allBoxed = false;

        // If all values are boxed
        if (allBoxed)
        {
            // If the primitive is an instruction
            if (prim instanceof Function)
            {
                // Insert the instruction
                var opVal = insertExceptIR(
                    curContext,
                    new prim(argVals)
                );
            }
            else
            {
                // Create the primitive call
                var opVal = insertPrimCallIR(
                    curContext,
                    prim, 
                    argVals
                );
            }
        }
        else
        {
            // Setup a try block to catch potential type errors
            try
            {
                if (!instrClass)
                    throw 'operator cannot operate on typed values: ' + primName;

                // Create the machine instruction
                var opVal = curContext.addInstr(
                    new instrClass(argVals)
                );
            }
            catch (exc)
            {
                // Rethrow the exception with source code location information
                rethrowError(exc, context.astNode.loc.to_string());
            }
        }

        return opVal;
    }

    // Function to generate code for a binary comparison operation
    function cmpGen(prim, cmpOp)
    {
        // Compile the argument values
        var argsContext = context.pursue(exprs);
        var argVals = exprListToIR(argsContext);

        // Test if all arguments are boxed
        var allBoxed = true;
        for (var i = 0; i < argVals.length; ++i)
            if (argVals[i].type !== IRType.box)
                allBoxed = false;

        // If all values are boxed
        if (allBoxed)
        {
            // Insert the HIR instruction
            var opVal = insertExceptIR(
                argsContext,
                new prim(argVals[0], argVals[1])
            );

            // Set the operator's output value as the output
            context.setOutput(argsContext.getExitBlock(), opVal);
        }
        else
        {
            // Ensure that the comparison is valid for non-boxed values
            if (cmpOp === undefined)
                throw 'comparison operation only applies to boxed values (' +
                    context.astNode.loc.to_string() + ')';

            // Ensure that the arguments have the same type
            if (argVals[0].type !== argVals[1].type)
                throw 'only values of the same type can be compared (' +
                    context.astNode.loc.to_string() + ')';
    
            // Create blocks for the true and false cases
            var trueBlock = context.cfg.getNewBlock('cmp_true');
            var joinBlock = context.cfg.getNewBlock('cmp_join');

            // Add the test instruction
            argsContext.addInstr(
                new IfInstr(
                    [argVals[0], argVals[1]],
                    cmpOp,
                    trueBlock,
                    joinBlock
                )
            );

            // Get the basic block for the false case
            var falseBlock = argsContext.getExitBlock();

            // Make the true block jump to the join block
            trueBlock.addInstr(new JumpInstr(joinBlock));

            // Add a phi node to select the value
            var joinVal = joinBlock.addInstr(
                new PhiInstr(
                    [IRConst.getConst(true), IRConst.getConst(false)],
                    [trueBlock, falseBlock]
                )
            );

            // Set the phi node's output value as the output
            context.setOutput(joinBlock, joinVal);
        }
    }

    // Switch on the operator
    switch (op)
    {
        // If this is an assignment expression
        case 'x = y':
        {
            // Get the left and right expressions
            var leftExpr = exprs[0];
            var rightExpr = exprs[1];

            // Generate IR for the right expression
            var rightContext = context.pursue(rightExpr);
            exprToIR(rightContext);

            // Convert the assignment
            var leftContext = rightContext.pursue(leftExpr);
            assgToIR(leftContext, rightContext.getOutValue());

            // Set the output to that of the assignment
            context.setOutput(leftContext.getExitBlock(), leftContext.getOutValue());
        }
        break;

        // If this is a logical AND expression
        case 'x && y':
        {
            // Compile the first expression
            var fstEntry = context.cfg.getNewBlock('log_and_fst');
            var fstContext = context.branch(
                exprs[0],
                fstEntry,
                context.localMap.copy()
            );
            exprToIR(fstContext);

            // Get the boolean value of the first expression
            var boolVal = insertPrimCallIR(
                fstContext, 
                'boxToBool',
                [fstContext.getOutValue()]
            );

            // Compile the second expression
            var secEntry = context.cfg.getNewBlock('log_and_sec');
            var secContext = context.branch(
                exprs[1],
                secEntry,
                fstContext.localMap.copy()
            );
            exprToIR(secContext);

            // Merge the local maps using phi nodes
            var joinBlock = mergeContexts(
                [fstContext, secContext],
                context.localMap,
                context.cfg,
                'log_and_join'
            );

            // Create a phi node to merge the values
            var phiValue = joinBlock.addInstr(
                new PhiInstr(
                    [fstContext.getOutValue(), secContext.getOutValue()],
                    [fstContext.getExitBlock(), secContext.getExitBlock()]
                )
            );

            // Jump to the first expression evaluation
            context.addInstr(new JumpInstr(fstEntry));
            
            // If the first expression evaluates to true, evaluate the second
            fstContext.getExitBlock().replBranch(
                new IfInstr(
                    [boolVal, IRConst.getConst(true)],
                    'EQ',
                    secEntry,
                    joinBlock
                )
            );

            // Set the exit block to be the join block
            context.setOutput(joinBlock, phiValue);
        }
        break;

        // If this is a logical OR expression
        case 'x || y':
        {
            // Compile the first expression
            var fstEntry = context.cfg.getNewBlock('log_or_fst');
            var fstContext = context.branch(
                exprs[0],
                fstEntry,
                context.localMap.copy()
            );
            exprToIR(fstContext);

            // Get the boolean value of the first expression
            var boolVal = insertPrimCallIR(
                fstContext, 
                'boxToBool',
                [fstContext.getOutValue()]
            );

            // Compile the second expression
            var secEntry = context.cfg.getNewBlock('log_or_sec');
            var secContext = context.branch(
                exprs[1],
                secEntry,
                fstContext.localMap.copy()
            );
            exprToIR(secContext);

            // Merge the local maps using phi nodes
            var joinBlock = mergeContexts(
                [fstContext, secContext],
                context.localMap,
                context.cfg,
                'log_or_join'
            );

            // Create a phi node to merge the values
            var phiValue = joinBlock.addInstr(
                new PhiInstr(
                    [fstContext.getOutValue(), secContext.getOutValue()],
                    [fstContext.getExitBlock(), secContext.getExitBlock()]
                )
            );

            // Jump to the first expression evaluation
            context.addInstr(new JumpInstr(fstEntry));

            // If the first expression evaluates to false, evaluate the second
            fstContext.getExitBlock().replBranch(
                new IfInstr(
                    [boolVal, IRConst.getConst(true)],
                    'EQ',
                    joinBlock,
                    secEntry
                )
            );

            // Set the exit block to be the join block
            context.setOutput(joinBlock, phiValue);
        }
        break;
        
        // If this is a conditional operator expression
        case 'x ? y : z':
        {
            // Compile the test expression
            var testContext = context.pursue(exprs[0]);
            exprToIR(testContext);

            // Get the boolean value of the test expression
            var boolVal = insertPrimCallIR(
                testContext, 
                'boxToBool',
                [testContext.getOutValue()]
            );

            // Compile the true expression
            var trueContext = context.branch(
                exprs[1],
                context.cfg.getNewBlock('cond_true'),
                testContext.localMap.copy()
            );
            exprToIR(trueContext);

            // Compile the false expression
            var falseContext = context.branch(
                exprs[2],
                context.cfg.getNewBlock('cond_false'),
                testContext.localMap.copy()
            );
            exprToIR(falseContext);

            // Create the if branching instruction
            testContext.addInstr(
                new IfInstr(
                    [boolVal, IRConst.getConst(true)],
                    'EQ',
                    trueContext.entryBlock,
                    falseContext.entryBlock
                )
            );

            // Merge the local maps using phi nodes
            var joinBlock = mergeContexts(
                [trueContext, falseContext],
                context.localMap,
                context.cfg,
                'cond_join'
            );

            // Create a phi node to merge the values
            var phiValue = joinBlock.addInstr(
                new PhiInstr(
                    [trueContext.getOutValue(), falseContext.getOutValue()],
                    [trueContext.getExitBlock(), falseContext.getExitBlock()]
                )
            );

            // Set the exit block to be the join block
            context.setOutput(joinBlock, phiValue);
        }
        break;

        // If this is the comma operator
        case 'x , y':
        { 
            // Compile the argument values
            var argsContext = context.pursue(exprs);
            var argVals = exprListToIR(argsContext);

            // Set the second argument's output value as the output
            context.setOutput(argsContext.getExitBlock(), argVals[1]);
        }
        break;

        // If this is the unary minus operator
        case '- x':
        { 
            // Compile the argument values
            var argsContext = context.pursue(exprs);
            var argVals = exprListToIR(argsContext);

            // FIXME: want to add HIR instructions for HIR ops, be able to
            // perform constant propagation on HIR
            if (argVals[0] instanceof IRConst && argVals[0].isNumber())
            {
                var opVal = IRConst.getConst(
                    num_mul(argVals[0].value, -1),
                    argVals[0].type
                );
            }
            else 
            {
                // Subtract the argument value from the constant 0
                var opVal = makeOp(
                    argsContext,
                    'sub',
                    SubInstr,
                    [IRConst.getConst(0, argVals[0].type), argVals[0]]
                );
            }

            // Set the subtraction's output value as the output
            context.setOutput(argsContext.getExitBlock(), opVal);
        }
        break;

        // If this is the unary plus operator
        case '+ x':
        { 
            // Compile the argument values
            var argsContext = context.pursue(exprs);
            var argVals = exprListToIR(argsContext);

            // Add the argument value to the constant 0
            var opVal = makeOp(
                argsContext,
                'add',
                AddInstr,
                [IRConst.getConst(0, argVals[0].type), argVals[0]]
            );

            // Set the subtraction's output value as the output
            context.setOutput(argsContext.getExitBlock(), opVal);
        }
        break;

        // If this is the object indexing operator
        case 'x [ y ]':
        {
            // Compile the argument values
            var argsContext = context.pursue(exprs);
            var argVals = exprListToIR(argsContext);

            /*
            // Create the appropriate operator instruction
            var opVal = insertPrimCallIR(
                argsContext, 
                'getPropVal', 
                [argVals[0], argVals[1]]
            );
            */

            var opVal = insertExceptIR(
                argsContext,
                new GetPropInstr(argVals[0], argVals[1])
            );

            // Set the operator's output value as the output
            context.setOutput(argsContext.getExitBlock(), opVal);
        }
        break;

        // If this is a field deletion
        case 'delete x':
        {
            // Get a reference to the field expression
            var fieldExpr = exprs[0];

            // If the op is a field indexing
            if (fieldExpr instanceof OpExpr && fieldExpr.op === 'x [ y ]')
            {
                // Compile the argument values
                var argsContext = context.pursue(fieldExpr.exprs);
                var argVals = exprListToIR(argsContext);

                // Create the appropriate operator instruction
                var opVal = insertPrimCallIR(
                    argsContext, 
                    'delPropVal', 
                    [argVals[0], argVals[1]]
                );

                // Set the operator's output value as the output
                context.setOutput(argsContext.getExitBlock(), opVal);
            }

            // Otherwise, if the op is a variable
            else if (fieldExpr instanceof Ref)
            {
                // Get the variable name
                var varName = fieldExpr.id.toString();

                // If there is a local variable with this name
                if (context.localMap.has(varName) === true ||
                    context.sharedMap.has(varName) === true)
                {
                    // Do nothing
                    context.setOutput(
                        context.entryBlock, 
                        IRConst.getConst(true)
                    );
                }

                // Otherwise, for the global variable case
                else
                {
                    // Declare variables for the operator value
                    var opValueObj;
                    var opValueGlob;

                    // If we are within a with block
                    if (context.withVal !== null)
                    {
                        // Add a has-property test on the object for the symbol name
                        var hasTestVal = insertPrimCallIR(
                            context,
                            'hasPropVal',
                            [context.withVal, IRConst.getConst(varName)]
                        );

                        // Context for the case where the with object has the property
                        var objContext = context.branch(
                            null,
                            context.cfg.getNewBlock('with_obj'),
                            context.localMap.copy()
                        );

                        // Context for the case where the object doesn't have the property
                        var globContext = context.branch(
                            null,
                            context.cfg.getNewBlock('with_glob'),
                            context.localMap.copy()
                        );

                        // Add the if branch expression
                        context.addInstr(
                            new IfInstr(
                                [hasTestVal, IRConst.getConst(true)],
                                'EQ',
                                objContext.entryBlock,
                                globContext.entryBlock
                            )
                        );
                    }
                    else
                    {
                        var globContext = context;
                    }

                    // Get the global object
                    var globalObj = insertGetGlobal(globContext);

                    // Delete the property from the global object
                    opValueGlob = insertPrimCallIR(
                        globContext, 
                        'delPropVal', 
                        [globalObj, IRConst.getConst(varName)]
                    );

                    // If we are within a with block
                    if (context.withVal !== null)
                    {
                        // Delete the property from the with object
                        var opValueObj = insertPrimCallIR(
                            objContext, 
                            'delPropVal', 
                            [context.withVal, IRConst.getConst(varName)]
                        );

                        // Bridge the prop and var contexts
                        objContext.bridge();
                        globContext.bridge();

                        // Merge the local maps using phi nodes
                        var joinBlock = mergeContexts(
                            [objContext, globContext],
                            context.localMap,
                            context.cfg,
                            'with_join'
                        );

                        // Create a phi node to merge the output values
                        var varValue = joinBlock.addInstr(
                            new PhiInstr(
                                [opValueGlob, opValueObj],
                                [globContext.getExitBlock(), objContext.getExitBlock()]
                            )
                        );

                        // Create a new context for the join block
                        var curContext = context.branch(
                            context.astNode,
                            joinBlock,
                            context.localMap
                        );
                    }
                    else
                    {
                        // Use the value and context from the global case
                        var curContext = globContext;
                        var opValue = opValueGlob;
                    }

                    // The operator value is the output value
                    context.setOutput(curContext.entryBlock, opValue);
                }
            }

            // Otherwise, for any other kind of expression
            else
            {
                // Evaluate the field expression
                var fieldCtx = context.pursue(fieldExpr);
                exprListToIR(fieldCtx);

                // Set the output to the true constant
                context.setOutput(
                    fieldCtx.getExitBlock(), 
                    IRConst.getConst(true)
                );
            }
        }
        break;

        case '++ x':
        prePostGen('add', AddInstr, false);
        break;

        case '-- x':
        prePostGen('sub', SubInstr, false);
        break;

        case 'x ++':
        prePostGen('add', AddInstr, true);     
        break;

        case 'x --':
        prePostGen('sub', SubInstr, true);           
        break;

        case 'x += y':
        compAssgGen('add', AddInstr);
        break;

        case 'x -= y':
        compAssgGen('sub', SubInstr);
        break;

        case 'x *= y':
        compAssgGen('mul', MulInstr);
        break;

        case 'x /= y':
        compAssgGen('div', DivInstr);
        break;

        case 'x %= y':
        compAssgGen('mod', ModInstr);
        break;

        case '~= x':
        compAssgGen('not', NotInstr);
        break;

        case 'x &= y':
        compAssgGen('and', AndInstr);
        break;

        case 'x |= y':
        compAssgGen('or', OrInstr);
        break;

        case 'x ^= y':
        compAssgGen('xor', XorInstr);
        break;

        case 'x <<= y':
        compAssgGen('lsft', LsftInstr);
        break;

        case 'x >>= y':
        compAssgGen('rsft', RsftInstr);
        break;

        case 'x >>>= y':
        compAssgGen('ursft', UrsftInstr);
        break;

        case 'x + y':
        opGen('add', AddInstr);
        break;

        case 'x - y':
        opGen('sub', SubInstr);
        break;

        case 'x * y':
        opGen('mul', MulInstr);
        break;

        case 'x / y':
        opGen('div', DivInstr);
        break;

        case 'x % y':
        opGen('mod', ModInstr);
        break;

        case '! x':
        opGen('logNot');
        break;

        case '~ x':
        opGen('not', NotInstr);
        break;

        case 'x & y':
        opGen('and', AndInstr);
        break;

        case 'x | y':
        opGen('or', OrInstr);
        break;

        case 'x ^ y':
        opGen('xor', XorInstr);
        break;

        case 'x << y':
        opGen('lsft', LsftInstr);
        break;

        case 'x >> y':
        opGen('rsft', RsftInstr);
        break;

        case 'x >>> y':
        opGen('ursft', UrsftInstr);
        break;

        case 'x < y':
        cmpGen(HIRLtInstr, 'LT');
        break;

        case 'x <= y':
        cmpGen(HIRLeInstr, 'LE');
        break;

        case 'x > y':
        cmpGen(HIRGtInstr, 'GT');
        break;

        case 'x >= y':
        cmpGen(HIRGeInstr, 'GE');
        break;

        case 'x === y':
        cmpGen(HIRSeInstr, 'EQ');
        break;

        case 'x !== y':
        cmpGen(HIRNsInstr, 'NE');
        break;

        case 'x == y':
        opGen(HIREqInstr);
        break;

        case 'x != y':
        opGen(HIRNeInstr);
        break;

        case 'typeof x':
        opGen('typeOf');
        break;

        case 'x instanceof y':
        opGen('instanceOf');
        break;

        case 'x in y':
        opGen('inOp');
        break;

        default:
        {
            assert (false, 'Unsupported AST operation "' + op + '"');
        }
    }
}

/**
Convert an assignment expression to IR code
@param rhsVal value to assign or code gen function
*/
function assgToIR(context, rhsVal)
{
    // Ensure that the IR conversion context is valid
    assert (
        context instanceof IRConvContext,
        'invalid IR conversion context specified'
    );

    // Get the left side expression
    var leftExpr = context.astNode;

    // If the left-hand side is a simple variable name
    if (leftExpr instanceof Ref)
    {
        var symName = leftExpr.id.toString();   

        // If the variable is global
        if (leftExpr.id.scope instanceof Program)
        {
            // If global variable accesses are forbidden
            if (context.cfg.ownerFunc.noGlobal)
            {
                error(
                    'lookup of global variable "' + symName + '" but ' +
                    'global variable accesses are not allowed in ' +
                    context.cfg.ownerFunc.funcName
                );
            }

            // Get the global object
            var globalObj = insertGetGlobal(context);
        }

        // If we are within a with block
        if (context.withVal !== null)
        {
            // Add a has-property test on the object for the symbol name
            var hasTestVal = insertPrimCallIR(
                context, 
                'hasPropVal', 
                [context.withVal, IRConst.getConst(symName)]
            );

            // Context for the case where the with object has the property
            var propContext = context.branch(
                null,
                context.cfg.getNewBlock('with_prop'),
                context.localMap.copy()
            );

            // Context for the case where the object doesn't have the property
            var varContext = context.branch(
                null,
                context.cfg.getNewBlock('with_var'),
                context.localMap.copy()
            );

            // Add the if branch expression
            context.addInstr(
                new IfInstr(
                    [hasTestVal, IRConst.getConst(true)],
                    'EQ',
                    propContext.entryBlock,
                    varContext.entryBlock
                )
            );
        }
        else
        {
            var varContext = context;
        }

        // If a RHS code gen function was specified
        if (rhsVal instanceof Function)
        {      
            // Declare a variable for the LHS value
            var lhsVal;
      
            // If the variable is global
            if (leftExpr.id.scope instanceof Program)
            {
                // Get the value from the global object
                lhsVal = insertPrimCallIR(
                    varContext, 
                    'getPropVal', 
                    [globalObj, IRConst.getConst(symName)]
                );
            }

            // Otherwise, if this variable is a shared closure variable
            else if (varContext.sharedMap.has(symName) === true)
            {
                // Get the mutable cell for the variable
                var cellValue = context.sharedMap.get(symName);

                // Get the value in the mutable cell
                lhsVal = insertPrimCallIR(
                    varContext,
                    'get_cell_val',
                    [cellValue]
                );
            }

            // Otherwise, the variable is local
            else
            {
                // Get the value in the local map
                lhsVal = varContext.localMap.get(symName, rhsVal);
            }

            // Update the RHS value according to the specified function
            var funcCtx = varContext.pursue(context.astNode);
            rhsVal(funcCtx, lhsVal);
            var rhsValAssg = funcCtx.getOutValue();

            // Create a new context to pursue the code generation
            varContext = funcCtx.pursue(funcCtx.astNode);
        }
        else
        {
            var rhsValAssg = rhsVal;
        }

        // If the variable is global
        if (leftExpr.id.scope instanceof Program)
        {
            // Set the value in the global object
            insertPrimCallIR(
                varContext, 
                'putPropVal', 
                [globalObj, IRConst.getConst(symName), rhsValAssg]
            );
        }

        // Otherwise, if this variable is a shared closure variable
        else if (context.sharedMap.has(symName) === true)
        {
            // Get the mutable cell for the variable
            var cellValue = context.sharedMap.get(symName);

            // Set the value in the mutable cell
            insertPrimCallIR(
                varContext,
                'set_cell_val',
                [cellValue, rhsValAssg]
            );
        }

        // Otherwise, the variable is local
        else
        {
            // Update the variable value in the locals map
            varContext.localMap.set(symName, rhsValAssg);
        }

        // If we are within a with block
        if (context.withVal !== null)
        {
            // If a RHS code gen function was specified
            if (rhsVal instanceof Function)
            {   
                // Declare a variable for the LHS value
                var lhsVal;

                // Get the value in the with object
                lhsVal = insertPrimCallIR(
                    propContext, 
                    'getPropVal', 
                    [context.withVal, IRConst.getConst(symName)]
                );

                // Update the RHS value according to the specified function
                var funcCtx = propContext.pursue(context.astNode);
                rhsVal(funcCtx, lhsVal);
                var rhsValAssg = funcCtx.getOutValue();

                // Create a new context to pursue the code generation
                propContext = funcCtx.pursue(funcCtx.astNode);
            }
            else
            {
                var rhsValAssg = rhsVal;
            }

            // Set the value in the with object
            insertPrimCallIR(
                propContext, 
                'putPropVal', 
                [context.withVal, IRConst.getConst(symName), rhsValAssg]
            );

            // Bridge the prop and var contexts
            propContext.bridge();
            varContext.bridge();

            // Merge the local maps using phi nodes
            var joinBlock = mergeContexts(
                [propContext, varContext],
                context.localMap,
                context.cfg,
                'with_join'
            );

            // Create a new context for the join block
            var curContext = context.branch(
                context.astNode,
                joinBlock,
                context.localMap
            );
        }
        else
        {
            // Use the context from the variable case
            var curContext = varContext;
        }

        // If the assignment value is an instruction, but not a function argument
        if (rhsValAssg instanceof IRInstr && !(rhsValAssg instanceof ArgValInstr))
        {
            // Assign the lhs variable name to the instruction
            rhsValAssg.outName = symName;
        }

        // The value of the right expression is the assignment expression's value
        context.setOutput(curContext.entryBlock, rhsValAssg);
    }

    // Otherwise, if the left-hand side is an object field
    else if (leftExpr instanceof OpExpr && leftExpr.op === 'x [ y ]')
    {
        var objExpr = leftExpr.exprs[0];
        var idxExpr = leftExpr.exprs[1];
    
        // Generate code for the object expression
        var objContext = context.pursue(objExpr);
        exprToIR(objContext);

        // Generate code for the index expression
        var idxContext = objContext.pursue(idxExpr);
        exprToIR(idxContext);

        // Create a new context to pursue the code generation
        var curContext = idxContext.pursue(context.astNode);

        // If a RHS code gen function was specified
        if (rhsVal instanceof Function)
        {
            // Declare a variable for the LHS value
            var lhsVal;

            // Get the property's current value
            lhsVal = insertPrimCallIR(
                curContext, 
                'getPropVal', 
                [objContext.getOutValue(), idxContext.getOutValue()]
            );

            // Update the RHS value according to the specified function
            var funcCtx = curContext.pursue(context.astNode);
            rhsVal(funcCtx, lhsVal);
            rhsVal = funcCtx.getOutValue();

            // Create a new context to pursue the code generation
            curContext = funcCtx.pursue(funcCtx.astNode);
        }

        // Set the property to the right expression's value
        insertPrimCallIR(
            curContext, 
            'putPropVal', 
            [objContext.getOutValue(), idxContext.getOutValue(), rhsVal]
        );

        // The value of the right expression is the assignment expression's value
        context.setOutput(curContext.entryBlock, rhsVal);
    }

    // Otherwise
    else
    {
        pp(leftExpr);
        assert (false, 'unsupported assignment lhs expression');
    }
}

/**
Convert a variable reference expression to IR code
*/
function refToIR(context)
{
    //print('Generating IR for ref expr:');
    //pp(context.astNode);

    // Ensure that the IR conversion context is valid
    assert (
        context instanceof IRConvContext,
        'invalid IR conversion context specified'
    );

    var astExpr = context.astNode;

    var symName = astExpr.id.toString();

    // Declare variables for the variable value
    var varValueVar;
    var varValueProp;

    // If we are within a with block
    if (context.withVal !== null)
    {
        // Add a has-property test on the object for the symbol name
        var hasTestVal = insertPrimCallIR(
            context,
            'hasPropVal',
            [context.withVal, IRConst.getConst(symName)]
        );

        // Context for the case where the with object has the property
        var propContext = context.branch(
            null,
            context.cfg.getNewBlock('with_prop'),
            context.localMap.copy()
        );

        // Context for the case where the object doesn't have the property
        var varContext = context.branch(
            null,
            context.cfg.getNewBlock('with_var'),
            context.localMap.copy()
        );

        // Add the if branch expression
        context.addInstr(
            new IfInstr(
                [hasTestVal, IRConst.getConst(true)],
                'EQ',
                propContext.entryBlock,
                varContext.entryBlock
            )
        );
    }
    else
    {
        var varContext = context;
    }

    // If the variable is global
    if (astExpr.id.scope instanceof Program && symName !== 'arguments')
    {
        // If we are compiling tachyon code and there is a named static
        // binding with this name
        if (context.params.tachyonSrc && context.params.staticEnv.hasBinding(symName))
        {
            // Use the value of the binding
            varValueVar = context.params.staticEnv.getBinding(symName);
        }
        else
        {
            // If global variable accesses are forbidden
            if (context.cfg.ownerFunc.noGlobal)
            {
                error(
                    'lookup of global variable "' + symName + '" but ' +
                    'global variable accesses are not allowed in ' +
                    context.cfg.ownerFunc.funcName
                );
            }

            // Precompute the hash code of the symbol
            var symHashVal = IRConst.getConst(
                precompHash(symName, context.params),
                IRType.pint
            );

            // Get the global object
            var globalObj = insertGetGlobal(varContext);

            // If this is a global function lookup
            if (context.ctxNode instanceof CallExpr)
            {
                // Get a function value from the global object
                varValueVar = insertPrimCallIR(
                    varContext, 
                    'getGlobalFunc', 
                    [
                        globalObj, 
                        IRConst.getConst(symName),
                        symHashVal
                    ]
                );
            }
            else
            {
                // Get the value from the global object
                varValueVar = insertPrimCallIR(
                    varContext, 
                    'getGlobal', 
                    [
                        globalObj, 
                        IRConst.getConst(symName),
                        symHashVal
                    ]
                );
            }
        }

        // If the assignment value is an instruction
        if (varValueVar instanceof IRInstr)
        {
            // Assign the symbol name to the instruction
            varValueVar.outName = symName;
        }
    }

    // Otherwise, if this variable is a shared closure variable
    else if (context.sharedMap.has(symName) === true)
    {
        // Get the mutable cell for the variable
        var cellValue = context.sharedMap.get(symName);

        // Get the value from the mutable cell
        varValueVar = insertPrimCallIR(
            varContext,
            'get_cell_val',
            [cellValue]
        );
    }

    // Otherwise, the variable is local
    else
    {
        assert (
            context.localMap.has(symName) === true,
            'local variable not in locals map: ' + symName
        );

        // Lookup the variable in the locals map
        varValueVar = varContext.localMap.get(symName);
    }

    // If we are within a with block
    if (context.withVal !== null)
    {
        // Get the value in the with object
        var varValueProp = insertPrimCallIR(
            propContext, 
            'getPropVal', 
            [context.withVal, IRConst.getConst(symName)]
        );

        // Bridge the prop and var contexts
        propContext.bridge();
        varContext.bridge();

        // Merge the local maps using phi nodes
        var joinBlock = mergeContexts(
            [propContext, varContext],
            context.localMap,
            context.cfg,
            'with_join'
        );

        // Create a phi node to merge the variable values
        var varValue = joinBlock.addInstr(
            new PhiInstr(
                [varValueVar, varValueProp],
                [varContext.getExitBlock(), propContext.getExitBlock()]
            )
        );

        // Create a new context for the join block
        var curContext = context.branch(
            context.astNode,
            joinBlock,
            context.localMap
        );
    }
    else
    {
        // Use the value and context from the variable case
        var curContext = varContext;
        var varValue = varValueVar;
    }

    // The variable value is the output value
    context.setOutput(curContext.entryBlock, varValue);
}

/**
Insert a read from the runtime context
*/
function insertCtxReadIR(context, query, outName)
{
    // Get a pointer to the context object
    var ctxPtr = context.addInstr(
        new GetCtxInstr()
    );

    // Generate IR to access the field
    var field = context.params.memLayouts.ctx.genfieldAccessIR(context, query);

    // Read the variable from the context object
    var readVal = context.addInstr(
        new LoadInstr(
            field.type,
            ctxPtr, 
            field.offset
        ),
        outName
    );

    // Return the value read
    return readVal;
}

/**
Insert a context read to get the global object
*/
function insertGetGlobal(context)
{
    return insertCtxReadIR(context, ['globalobj'], 'global');
}

/**
Insert IR to be conditionally executed.
*/
function insertCondIR(context, testVal, trueGenFunc, falseGenFunc)
{
    // Create a basic block for the non-error case
    var contBlock = context.cfg.getNewBlock('test_cont');

    // If a true case is defined
    if (trueGenFunc !== undefined)
    {
        // Create a context for the true case
        var trueCtx = context.branch(
            null,
            context.cfg.getNewBlock('test_true'),
            context.localMap.copy()
        );
    }

    // If a false case is defined
    if (falseGenFunc !== undefined)
    {
        // Create a context for the false case
        var falseCtx = context.branch(
            null,
            context.cfg.getNewBlock('test_false'),
            context.localMap.copy()
        );
    }

    // Branch based on the test value
    context.addInstr(
        new IfInstr(
            [testVal, IRConst.getConst(true)],
            'EQ',
            (trueCtx !== undefined)? trueCtx.entryBlock:contBlock,
            (falseCtx !== undefined)? falseCtx.entryBlock:contBlock
        )
    );

    // If a true case is defined
    if (trueGenFunc !== undefined)
    {
        // Generate the code for the conditional branch
        trueGenFunc(trueCtx);

        // If the block is not terminated, add a jump to the continuation
        if (trueCtx.isTerminated() === false)
            trueCtx.addInstr(new JumpInstr(contBlock));
    }

    // If a false case is defined
    if (falseGenFunc !== undefined)
    {
        // Generate the code for the conditional branch
        falseGenFunc(falseCtx);

        // If the block is not terminated, add a jump to the continuation
        if (falseCtx.isTerminated() === false)
            falseCtx.addInstr(new JumpInstr(contBlock));
    }

    // Splice the non-error block into the context
    context.splice(contBlock);
}

/**
Throw an error object containing an error message if a test fails
*/
function insertCondErrorIR(context, testVal, errorName, errorMsg)
{
    // If the test fails, execute the error code
    insertCondIR(
        context,
        testVal,
        undefined,
        function (errorCtx)
        {
            // Generate code to throw an error
            insertErrorIR(
                errorCtx, 
                errorName, 
                errorMsg
            );
        }
    );
}

/**
Throw an error object containing an error message
*/
function insertErrorIR(context, errorName, errorMsg)
{
    // Find the error contructor
    var errorCtor;
    switch (errorName)
    {
        case 'RangeError':
        errorCtor = insertCtxReadIR(context, ['rangeerror']);
        break;
        case 'ReferenceError':
        errorCtor = insertCtxReadIR(context, ['referror']);
        break;
        case 'SyntaxError':
        errorCtor = insertCtxReadIR(context, ['syntaxerror']);
        break;
        case 'TypeError':
        errorCtor = insertCtxReadIR(context, ['typeerror']);
        break;
        case 'URIError':
        errorCtor = insertCtxReadIR(context, ['urierror']);
        break;
    }
    assert (
        errorCtor,
        'error constructor not found for: "' + errorName + '"'
    );

    // Create a new context from which to throw the exception
    var throwCtx = context.pursue(null);

    // Insert a call to the error constructor
    var excVal = insertPrimCallIR(
        throwCtx, 
        'makeError', 
        [
            errorCtor,
            IRConst.getConst(errorMsg)
        ]
    );

    // Throw the error created
    throwToIR(context, throwCtx, excVal);
}

/**
Insert a call to a primitive function
*/
function insertPrimCallIR(context, primName, argVals)
{
    // Get the static binding for the primitive function
    var primFunc = context.params.staticEnv.getBinding(primName);

    // Insert the function call
    var retVal = insertExceptIR(
        context,
        new CallFuncInstr(
            [
                primFunc, 
                IRConst.getConst(undefined),
                IRConst.getConst(undefined)
            ].concat(argVals)
        )
    );

    return retVal;
}

/**
Insert a construct instruction in a given context, connect it with its
continue and throw targets, produce the related object creation code, and
splice this into the current context
*/
function insertConstructIR(context, funcVal, argVals)
{
    if (argVals.length === 0)
    {
        return insertPrimCallIR(
            context,
            'newCtor0', 
            [funcVal]
        );
    }

    // Get the function's prototype field
    var funcProto = insertPrimCallIR(
        context,
        'getPropVal', 
        [funcVal, IRConst.getConst('prototype')]
    );

    // If the prototype field is an object use it, otherwise, use the
    // object prototype object
    var protoIsObj = context.cfg.getNewBlock('proto_is_obj');
    var protoNotObjCtx = context.branch(
        context.astNode,
        context.cfg.getNewBlock('proto_not_obj'),
        context.localMap.copy()
    );
    var protoMerge = context.cfg.getNewBlock('proto_merge');
    var testVal = insertPrimCallIR(
        context,
        'boxIsObjExt',
        [funcProto]
    );
    context.addInstr(
        new IfInstr(
            [testVal, IRConst.getConst(true)],
            'EQ',
            protoIsObj,
            protoNotObjCtx.entryBlock
        )
    );
    var objProto = insertCtxReadIR(
        protoNotObjCtx,
        ['objproto']
    );
    protoIsObj.addInstr(new JumpInstr(protoMerge));
    protoNotObjCtx.addInstr(new JumpInstr(protoMerge));
    var protoVal = protoMerge.addInstr(
        new PhiInstr(
            [
                funcProto,
                objProto
            ],
            [
                protoIsObj, 
                protoNotObjCtx.entryBlock
            ]
        ),
        'proto_val'
    );
    context.splice(protoMerge);
    
    // Create a new object
    var newObj = insertPrimCallIR(
        context, 
        'newObject', 
        [protoVal]
    );

    // If this is not a direct function call
    if ((funcVal instanceof IRFunction) === false)
    {
        // Test if the callee value is a function
        var testVal = insertPrimCallIR(
            context,
            'boxIsFunc',
            [funcVal]
        );

        // Throw an error if the callee is not a function
        insertCondErrorIR(
            context, 
            testVal, 
            'TypeError',
            'constructor is not a function (' +
            context.astNode.loc.to_string() + ')'
        );
    }

    var funcPtr = insertPrimCallIR(
        context,
        'get_clos_funcptr',
        [funcVal]
    );
    
    // Create the constructor call instruction
    var retVal = insertExceptIR(
        context,
        new ConstructInstr(
            [
                funcPtr,
                funcVal,
                newObj
            ].concat(argVals)
        )
    );

    // If the return value is an object, use it, otherwise use the new object
    var retIsObj = context.cfg.getNewBlock('ret_is_obj');
    var retNotObj = context.cfg.getNewBlock('ret_not_obj');
    var retMerge = context.cfg.getNewBlock('ret_merge');
    var testVal = insertPrimCallIR(
        context,
        'boxIsObjExt',
        [retVal]
    );
    context.addInstr(
        new IfInstr(
            [testVal, IRConst.getConst(true)],
            'EQ',
            retIsObj,
            retNotObj
        )
    );
    retIsObj.addInstr(new JumpInstr(retMerge));
    retNotObj.addInstr(new JumpInstr(retMerge));
    var objVal = retMerge.addInstr(
        new PhiInstr(
            [
                retVal,
                newObj
            ],
            [
                retIsObj, 
                retNotObj
            ]
        ),
        'obj_val'
    );
    context.splice(retMerge);

    // Return the newly created object
    return objVal;
}

/**
Insert an exception-producing instruction in a given context, connect it with
its continue and throw targets and splice this into the current context
*/
function insertExceptIR(context, instr)
{
    // If we are in a try block
    if (context.throwList !== null)
    {
        // Create a basic block for the call continuation
        var contBlock = context.cfg.getNewBlock(instr.mnemonic + '_cont');

        // Set the continue target for the instruction
        instr.setContTarget(contBlock);

        // Create a new context and bridge it
        var newCtx = context.pursue(null);
        newCtx.bridge();

        // Copy the local map so as to not make available
        // new bindings after the throw
        newCtx.localMap = newCtx.localMap? newCtx.localMap.copy():newCtx.localMap;

        // Add the new context to the list of throw contexts
        context.throwList.push(newCtx);

        // Add the call instruction to the current context
        context.addInstr(instr);

        // Splice the context to use the continue block
        context.splice(contBlock);
    }
    else
    {
        // Add the call instruction to the current context
        context.addInstr(instr);
    }

    // Return the instruction value
    return instr;
}

/**
Generate a throw instruction with a given exception value
@param throwCtx context after which to throw the exception
@param excVal exception value to throw
*/
function throwToIR(context, throwCtx, excVal)
{
    // Add a throw instruction
    throwCtx.addInstr(
        new ThrowInstr(excVal)
    );

    // If this is an intraprocedural throw
    if (context.throwList !== null)
    {
        // Add the context to the list of throw contexts
        context.throwList.push(throwCtx);
    }

    // Terminate the current context, no instructions go after this
    context.terminate();

    // Bridge the throw context to make sure it has an exit block
    throwCtx.bridge();
}

/**
Merge IR conversion contexts local variables locations using phi nodes
*/
function mergeContexts(
    contexts,
    mergeMap,
    cfg,
    blockName
)
{
    // Build a list of non-terminated contexts
    var ntContexts = [];
    for (var i = 0; i < contexts.length; ++i)
        if (!contexts[i].isTerminated())
            ntContexts.push(contexts[i]);

    // Create a block for the merging
    var mergeBlock = cfg.getNewBlock(blockName);

    // If there are no non-terminated contexts, there is nothing to merge, stop
    if (ntContexts.length === 0)
        return null;

    // Clear the contents of the merge map, if any
    mergeMap.clear();

    // Get the keys from the first join point
    var keys = ntContexts[0].localMap.getKeys();

    // For each local
    for (var i = 0; i < keys.length; ++i)
    {
        var varName = keys[i];

        // Create arrays for the incoming values and corresponding predecessors
        var values = [];
        var preds = [];

        // For each context
        for (var j = 0; j < ntContexts.length; ++j)
        {
            var context = ntContexts[j];

            // Add the value of the current variable to the list
            values.push(context.localMap.get(varName));
            preds.push(context.exitBlock);
        }

        // Compute properties of the input values
        var firstVal = values[0];
        var allEqual = true;
        var allSameType = true;
        for (var j = 0; j < values.length; ++j)
        {
            var value = values[j];
            if (value !== firstVal)
                allEqual = false;
            if (value.type !== firstVal.type)
                allSameType = false;
        }

        // If not all values have the same type
        if (allSameType === false)
        {
            // Set the merge value to undefined
            mergeMap.set(varName, IRConst.getConst(undefined));
        }

        // If not all incoming values are the same
        else if (!allEqual)
        {
            // Create a phi node for this variable
            var phiNode = new PhiInstr(values, preds);

            // Add the phi node to the merge block
            mergeBlock.addInstr(phiNode, varName);

            // Add the phi node to the merge map
            mergeMap.set(varName, phiNode);
        }

        // Otherwise, all values are the same
        else
        {
            // Add the value directly to the merge map
            mergeMap.set(varName, firstVal);
        }
    }

    // For each context
    for (var i = 0; i < ntContexts.length; ++i)
    {
        var context = ntContexts[i];

        // Make the block jump to the merge block
        if (!context.getExitBlock().hasBranch())
            context.addInstr(new JumpInstr(mergeBlock));
    }

    // Return the merge block
    return mergeBlock;
}

/**
Create an IR conversion context for a loop entry
*/
function createLoopEntry(
    loopStmt,
    entryNode,
    context,
    entryLocals,
    brkCtxList,
    cntCtxList,
    blockName
)
{
    // Update the break and continue context maps for the loop body
    var breakMap = context.breakMap.copy();
    var contMap = context.contMap.copy();
    if (brkCtxList)
    {
        breakMap.set('', brkCtxList);
        for (var i = 0; i < context.labels.length; ++i)
            breakMap.set(context.labels[i], brkCtxList);
    }
    if (cntCtxList)
    {
        contMap.set('', cntCtxList);
        for (var i = 0; i < context.labels.length; ++i)
            contMap.set(context.labels[i], cntCtxList);
    }

    // Create a basic block for the loop entry
    var loopEntry = context.cfg.getNewBlock(blockName);

    // Create a phi node for each local variable in the current context
    var localVars = context.localMap.getKeys();
    for (var i = 0; i < localVars.length; ++i)
    {
        var varName = localVars[i];
        var phiNode = new PhiInstr(
            [context.localMap.get(varName)],
            [(context.exitBlock !== undefined)
             ? context.exitBlock
             : context.entryBlock]
        );
        loopEntry.addInstr(phiNode, varName);
        entryLocals.set(varName, phiNode);
    }

    // Return the loop entry context
    return new IRConvContext(
        entryNode,
        loopEntry,
        context.ctxNode,
        context.withVal,
        [],
        entryLocals.copy(),
        context.sharedMap,
        breakMap,
        contMap,
        context.throwList,
        context.cfg,
        context.funcObj,
        context.thisVal,
        context.params
    );
}

/**
Merge contexts for a loop entry block
*/
function mergeLoopEntry(
    contexts,
    entryLocals,
    contLocals,
    entryBlock
)
{
    // Get the local variable names
    var localVars = entryLocals.getKeys();

    // For each local variable
    for (var i = 0; i < localVars.length; ++i)
    {
        var varName = localVars[i];
        var phiNode = entryLocals.get(varName);

        // For each incoming context
        for (var j = 0; j < contexts.length; ++j)
        {
            var context = contexts[j];

            // If this context is terminated, skip it
            if (context.isTerminated())
                continue;

            var varValue = context.localMap.get(varName);

            // If there would be a mix of values of different types
            if (varValue.type !== phiNode.type)
            {
                // If the phi node value is already being used
                if (phiNode.dests.length > 0)
                {
                    // Print an error and abort compilation
                    error(
                        'phi node for "' + varName + '" merges values of ' +
                        'different types, but cannot be removed because its ' +
                        'value is used'
                    );
                }

                // Replace the phi node by the undefined value in the loop
                // entry and the continuation locals
                entryLocals.set(varName, IRConst.getConst(undefined));
                contLocals.set(varName, IRConst.getConst(undefined));

                // Remove the phi node
                entryBlock.remInstr(phiNode);

                // Abort the merge for this variable
                break;
            }

            // Add an incoming value for this context
            phiNode.addIncoming(varValue, context.getExitBlock());
        }
    }

    // Add a branch from every continue context to the loop entry
    for (var j = 0; j < contexts.length; ++j)
    {
        var context = contexts[j];

        // If this context is terminated, skip it
        if (context.isTerminated())
            continue;

        var exitBlock = context.getExitBlock();

        if (!exitBlock.hasBranch())
            exitBlock.addInstr(new JumpInstr(entryBlock));
    }
}

/**
Test if a call expression is an inline IR instruction
*/
function isInlineIR(callExpr)
{
    var fnExpr = callExpr.fn;

    return (
        fnExpr instanceof OpExpr && 
        fnExpr.op === 'x [ y ]' &&
        fnExpr.exprs[0] instanceof Ref && 
        fnExpr.exprs[0].id.toString() === 'iir'
    );
}

/**
Generate an inline IR instruction
*/
function genInlineIR(context, branches)
{
    // Ensure that a valid expression was passed
    assert (
        context.astNode instanceof CallExpr && isInlineIR(context.astNode),
        'invalid inline IR expression'
    );

    // Get a reference to the function expression
    var fnExpr = context.astNode.fn;

    // Get a the call arguments
    var args = context.astNode.args.slice(0);

    // Create a list for the instruction arguments
    var instrArgs = [];

    // While the first argument is an IR type
    while (
        args.length > 0 && 
        args[0] instanceof OpExpr &&
        args[0].op === 'x [ y ]' &&
        args[0].exprs[0] instanceof Ref &&
        args[0].exprs[0].id.toString() === 'IRType'
    )
    {
        // Get a reference to the IR type object
        var typeName = args[0].exprs[1].value;
        var typeObj = IRType[typeName];

        // Remove the first argument from the list
        args.shift();

        // Add the type object to the instruction arguments
        instrArgs.push(typeObj);
    }

    // Compile the function argument list
    var argsContext = context.pursue(args);
    var argVals = exprListToIR(argsContext);
    instrArgs = instrArgs.concat(argVals);

    // If branch targets were specified, add them to the instruction arguments
    if (branches !== undefined)
        instrArgs = instrArgs.concat(branches);

    // Get a reference to the instruction constructor
    var instrName = fnExpr.exprs[1].value;
    var instrCtor = iir[instrName];

    // Create the new instruction or value
    var newVal = new instrCtor(instrArgs);

    // If the value is an instruction, add it to the current basic block
    if (newVal instanceof IRInstr) 
        argsContext.addInstr(newVal);

    // Set the new instruction as the output
    context.setOutput(argsContext.getExitBlock(), newVal);
}

/**
Test if an if statement is a conditional inline IR instruction
*/
function isCondInlineIR(ifStmt)
{
    return (
        isInlineIR(ifStmt.expr) 
        ||
        (
            ifStmt.expr instanceof OpExpr &&
            ifStmt.expr.op === 'x = y' &&
            ifStmt.expr.exprs[0] instanceof Ref &&
            isInlineIR(ifStmt.expr.exprs[1])
        )
    );
}

/**
Generate a conditional inline IR instruction
*/
function genCondInlineIR(context)
{
    // Ensure that a valid statement was passed
    assert (
        context.astNode instanceof IfStatement && isCondInlineIR(context.astNode),
        'invalid conditional inline IR expression'
    );

    // Get a reference to the if statement
    var astStmt = context.astNode;

    // Create basic blocks for the true and false branches
    var trueBlock = context.cfg.getNewBlock('iir_true');
    var falseBlock = context.cfg.getNewBlock('iir_false');

    // If this is an inline IR instruction with value assignment
    if (
        astStmt.expr instanceof OpExpr &&
        astStmt.expr.op === 'x = y' &&
        astStmt.expr.exprs[0] instanceof Ref &&
        isInlineIR(astStmt.expr.exprs[1])
    )
    {
        // Compile the inline IR instruction
        var testContext = context.pursue(astStmt.expr.exprs[1]);
        genInlineIR(testContext, [trueBlock, falseBlock]);

        // Store the IIR instruction output value
        var iirValue = testContext.getOutValue();

        // Create a context for the true assignment
        var trueAssgCtx = context.branch(
            astStmt.expr.exprs[0],
            trueBlock,
            testContext.localMap.copy()
        );

        // Create a context for the false assignment
        var falseAssgCtx = context.branch(
            astStmt.expr.exprs[0],
            falseBlock,
            testContext.localMap.copy()
        );

        // Assign the value on both branches
        assgToIR(trueAssgCtx, iirValue);
        assgToIR(falseAssgCtx, iirValue);

        // Pursue the assignment context for the true statement
        var trueContext = trueAssgCtx.pursue(
            astStmt.statements[0]
        );

        // Pursue the assignment context for the false statement
        var falseContext = falseAssgCtx.pursue(
            astStmt.statements[1]? astStmt.statements[1]:null
        );
    }
    else
    {
        // Compile the inline IR instruction
        var testContext = context.pursue(astStmt.expr);
        genInlineIR(testContext, [trueBlock, falseBlock]);

        // Create a context for the true statement
        var trueContext = context.branch(
            astStmt.statements[0],
            trueBlock,
            testContext.localMap.copy()
        );

        // Create a context for the false statement
        var falseContext = context.branch(
            astStmt.statements[1]? astStmt.statements[1]:null,
            falseBlock,
            testContext.localMap.copy()
        );
    }

    // Compile the true statement
    stmtToIR(trueContext);

    // Compile the false statement, if it is defined
    if (astStmt.statements.length > 1)
        stmtToIR(falseContext);
    else
        falseContext.bridge();

    // Merge the local maps using phi nodes
    var joinBlock = mergeContexts(
        [trueContext, falseContext],
        context.localMap,
        context.cfg,
        'iir_join'
    );

    // Set the exit block to be the join block
    context.setOutput(joinBlock);
}

/**
Precompute the hash code of a value to be used in a hash lookup.
*/
function precompHash(val, params)
{
    if (typeof val === 'number')
    {
        return val;
    }
    else if (typeof val === 'string')
    {
        // Initialize the hash code to 0
        var hashCode = 0;

        // Initialize the integer value to 0
        var intVal = 0;

        // Flag indicating that the string represents an integer
        var isInt = true;

        // For each character, update the hash code
        for (var i = 0; i < val.length; i += 1)
        {
            // Get the current character
            var ch = val.charCodeAt(i);

            // If this character is a digit
            if (ch >= 48 && ch <= 57)
            {
                // Update the number value
                var digitVal = ch - 48;
                intVal = 10 * intVal + digitVal;
            }
            else
            {
                // This string does not represent a number
                isInt = false;
            }

            // Update the hash code
            hashCode = (((hashCode << 8) + ch) & 536870911) % 426870919;
        }

        var HASH_CODE_STR_OFFSET = params.staticEnv.getBinding('HASH_CODE_STR_OFFSET').value;

        // If this is an integer value within the supported range
        if (val.length > 0 && isInt && intVal < HASH_CODE_STR_OFFSET)
        {
            // Set the hash code to the integer value
            hashCode = intVal;
        }
        else
        {
            // Offset the string hash code to indicate this is not an integer value
            hashCode += HASH_CODE_STR_OFFSET;
        }

        return hashCode;
    }
    
    // For other value types
    else
    {
        return precompHash(val.toString(), params);
    }
}

