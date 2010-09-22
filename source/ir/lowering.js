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
            var toBoolInstr = new CallFuncInstr(
                [
                    primitiveMap["boxToBool"],
                    ConstValue.getConst(undefined),
                    instr.uses[0]
                ]
            );

            // Replace the if instruction by a typed if
            var ifBoolInstr = new IfInstr([toBoolInstr].concat(instr.targets));
            cfg.replInstr(itr, ifBoolInstr);

            // Add the instruction before the if
            cfg.addInstr(itr, toBoolInstr);

            var instr = itr.get();
        }

        // If this instruction should be translated into a primitive call
        if (usesBoxed && primitiveMap[instr.mnemonic])
        {
            // Get a reference to the primitive function
            var primFunc = primitiveMap[instr.mnemonic];

            // Create a call instruction  
            var callInstr = new CallFuncInstr(
                [
                    primFunc,
                    ConstValue.getConst(undefined)
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

    // Perform constant propagation on the CFG
    constProp(cfg);

    // Apply peephole optimization patterns to the CFG
    applyPatternsCFG(cfg);

    // Validate the CFG
    cfg.validate();

    // For each instructon in the CFG
    for (var itr = cfg.getInstrItr(); itr.valid(); itr.next())
    {
        var instr = itr.get();

        // If any instruction reads or writes from memory, annotate the
        // function as reading or writing memory
        if (instr.writesMem)
            cfg.ownerFunc.writesMem = true;
        if (instr.readsMem)
            cfg.ownerFunc.readsMem = true;
    }
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
    // Build a list of the ASTs of the primitive code
    var astList = [
        // Generated code for the memory objects
        parse_src_str(ObjectLayout.sourceStr),
        // Source code for the primitives
        parse_src_file('runtime/primitives.js'),
    ];

    // For each AST
    for (var i = 0; i < astList.length; ++i)
    {
        var ast = astList[i];

        // Parse static bindings in the unit
        staticEnv.parseUnit(ast);
    }

    // For each AST
    for (var i = 0; i < astList.length; ++i)
    {
        var ast = astList[i];

        // Generate IR from the AST
        var ir = unitToIR(ast, true);

        // For each function in the IR
        var funcList = ir.getChildrenList();
        for (var j = 0; j < funcList.length; ++j)
        {
            var func = funcList[j];

            if (!func.funcName)
                continue;

            // Add the function to the primitive map
            primitiveMap[func.funcName] = func;
        }
    }

    // For each primitive
    for (var funcName in primitiveMap)
    {
        var func = primitiveMap[funcName];

        // Perform IR lowering on the primitive
        lowerIRFunc(func);

        print(func);

        // Validate the resulting code
        func.validate();
    }
}
