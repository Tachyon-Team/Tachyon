/**
@fileOverview
Implementation of high-level IR lowering and specialization in preparation
for code generation.

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010 Maxime Chevalier-Boisvert, All Rights Reserved
*/

/**
Perform IR lowering on a function and its subfunctions
*/
function lowerIRFunc(irFunc)
{
    // For each function in the IR
    var funcList = irFunc.getChildrenList();
    for (var i = 0; i < funcList.length; ++i)
    {
        var func = funcList[i];

        // Perform lowering on the function's CFG
        lowerIRCFG(func.virginCFG);
    }
}

/**
Perform IR lowering on a control-flow graph
*/
function lowerIRCFG(cfg)
{
    // Get the entry block
    var entry = cfg.getEntryBlock();

    // For each instruction of the entry block
    for (var itr = entry.getInstrItr(); itr.valid(); itr.next())
    {
        var instr = itr.get();

        if (instr.outName == 'global')
            var globalObj = instr;
    }

    // If the global object was not found
    if (!globalObj)
    {
        // Load the global object from the context
        var ctxPtr = new GetCtxInstr();
        var globalObj = contextLayout.genCtxLoad(ctxPtr, 'GLOBAL_OBJECT');

        // Add the new instructions at the end of the entry block
        entry.addInstr(ctxPtr, 'ctx', entry.instrs.length - 1);
        entry.addInstr(globalObj, 'global', entry.instrs.length - 1);
    }

    // For each instruction in the CFG
    for (var itr = cfg.getInstrItr(); itr.valid(); itr.next())
    {
        var instr = itr.get();

        // Test if some instruction uses are boxed values
        var usesBoxed = false
        for (var i = 0; i < instr.uses.length; ++i)
            if (instr.uses[i].type === IRType.box)
                usesBoxed = true;

        // If this is an untyped if instruction
        if (usesBoxed && instr instanceof IfInstr)
        {
            // Create a boolean conversion instruction
            var toBoolInstr = new ToBoolInstr(instr.uses[0]);
            
            // Replace the if instruction by a typed if
            var ifBoolInstr = new IfInstr([toBoolInstr].concat(instr.targets));
            cfg.replInstr(itr, ifBoolInstr);

            // Add the instruction before the if
            cfg.addInstr(itr, toBoolInstr);

            // Move past the new instruction
            itr.next();
        }

        // If this instruction should be translated into a primitive call
        else if (usesBoxed && primitiveMap[instr.mnemonic])
        {
            // Get a reference to the primitive function
            var primFunc = primitiveMap[instr.mnemonic];

            // Create a call instruction            
            var callInstr = new CallFuncInstr(
                [
                    primFunc,
                    globalObj
                ]
                .concat(instr.uses)
                .concat(instr.targets)
            );
            
            // Replace the instruction by the call
            cfg.replInstr(itr, callInstr);

            var instr = itr.get();
        }

        // If this is a function call to a known function
        if (instr instanceof CallInstr && instr.uses[0] instanceof IRFunction)
        {
            var calleeFunc = instr.uses[0]

            // If the callee is marked inline and is inlinable
            if (calleeFunc.inline && isInlinable(calleeFunc))
            {
                //print('inlining: ' + calleeFunc.funcName);

                // Inline the call
                inlineCall(instr, calleeFunc);
            }
        }
    }

    // Simplify the lowered CFG
    cfg.simplify();
}

/**
Map of instruction names to primitive functions
*/
var primitiveMap = {}

/**
Compile the primitives source code to enable IR lowering
*/
function compPrimitives()
{
    // Parse the handler source code
    var ast = parse_src_file('runtime/primitives.js'); 

    // Parse static bindings in the unit
    staticEnv.parseUnit(ast);

    // Generate IR from the AST
    var ir = unitToIR(ast, true);

    // For each function in the IR
    var funcList = ir.getChildrenList();
    for (var i = 0; i < funcList.length; ++i)
    {
        var func = funcList[i];

        if (!func.funcName)
            continue;

        // Add the function to the primitive map
        primitiveMap[func.funcName] = func;
    }

    // For each handler
    for (var handlerName in primitiveMap)
    {
        var func = primitiveMap[handlerName];

        // Perform IR lowering on the handler
        lowerIRFunc(func);

        print(func);

        // Validate the resulting handler code
        func.validate();
    }
}

// Compile the IR primitives
compPrimitives()

