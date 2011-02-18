/**
@fileOverview
Implementation of high-level IR lowering and specialization in preparation
for code generation.

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010-2011 Maxime Chevalier-Boisvert, All Rights Reserved
*/

/*
TODO: 
- Reintroduce HIRInstr class with descendents for arith ops, putprop, getprop.
- Have lowering function for each HIRInstr class
- Default lowering can be translation to primitive call
- Perform const prop on IR before lowering begins, using HIR instructions
*/

/**
Perform IR lowering on a function and its subfunctions
*/
function lowerIRFunc(irFunc, params)
{
    assert (
        params instanceof CompParams,
        'expected compilation parameters'
    );

    if (params.printHIR === true)
        print(irFunc);

    // For each function in the IR
    var funcList = irFunc.getChildrenList();
    for (var i = 0; i < funcList.length; ++i)
    {
        var func = funcList[i];

        //print('calling lowerIRCFG for "' + func.funcName + '"');

        // Perform lowering on the function's CFG
        lowerIRCFG(func.virginCFG, params);

        //print('back from lowerIRCFG for "' + func.funcName + '"');
    }

    //print('lowerIRFunc done for "' + irFunc.funcName + '"');

    if (params.printLIR === true)
        print(irFunc);
}

/**
Perform IR lowering on a control-flow graph
*/
function lowerIRCFG(cfg, params)
{
    assert (
        params instanceof CompParams,
        'expected compilation parameters'
    );

    // For each instruction in the CFG
    for (var itr = cfg.getInstrItr(); itr.valid(); itr.next())
    {
        var instr = itr.get();

        // Test if some instruction uses are boxed values
        var usesBoxed = false;
        for (var i = 0; i < instr.uses.length; ++i)
            if (instr.uses[i].type === IRType.box)
                usesBoxed = true;

        // If this is a load or a store instruction on a boxed value
        if ((instr instanceof LoadInstr || instr instanceof StoreInstr) && 
            instr.uses[0].type === IRType.box)
        {
            // Create an unboxing operation
            var unboxVal = new CallFuncInstr(
                [
                    params.staticEnv.getBinding('unboxRef'),
                    ConstValue.getConst(undefined),
                    ConstValue.getConst(undefined),
                    instr.uses[0]
                ]
            );

            var instrConstr = (instr instanceof LoadInstr)? LoadInstr:StoreInstr;

            // Replace the load/store instruction
            cfg.replInstr(
                itr, 
                new instrConstr(
                    [instr.typeParams[0], unboxVal].concat(instr.uses.slice(1))
                )
            );

            // Add the instruction before the load
            cfg.addInstr(itr, unboxVal);

            var instr = itr.get();
        }

        // If this is an untyped if instruction
        if (usesBoxed && instr instanceof IfInstr)
        {
            // Create a boolean conversion operation
            var boolVal = new CallFuncInstr(
                [
                    params.staticEnv.getBinding('boxToBool'),
                    ConstValue.getConst(undefined),
                    ConstValue.getConst(undefined),
                    instr.uses[0]
                ]
            );

            // Replace the if instruction by a typed if
            var ifBoolInstr = new IfInstr([boolVal].concat(instr.targets));
            cfg.replInstr(itr, ifBoolInstr);

            // Add the instruction before the if
            cfg.addInstr(itr, boolVal);

            var instr = itr.get();
        }

        // If this is a function call to a known function
        if (instr instanceof CallInstr && instr.uses[0] instanceof IRFunction)
        {
            var calleeFunc = instr.uses[0];

            // If the callee is marked inline and is inlinable
            if (calleeFunc.inline && isInlinable(calleeFunc))
            {
                /*
                print(
                    'inlining: ' + calleeFunc.funcName + ' in ' + 
                    cfg.ownerFunc.funcName
                );
                */

                // Inline the call
                inlineCall(instr, calleeFunc);
            }
        }
    }

    //print('*** applying patterns ***');

    // Apply peephole optimization patterns to the CFG
    applyPatternsCFG(cfg, params);

    //print('*** validating after patterns ***');

    // Validate the CFG
    cfg.validate();

    //print(cfg.ownerFunc);

    //print('*** const prop ***');

    // Perform constant propagation on the CFG
    constProp(cfg, params);

    //print(cfg.ownerFunc);

    // Validate the CFG
    cfg.validate();

    // Apply peephole optimization patterns to the CFG
    applyPatternsCFG(cfg, params);

    // Validate the CFG
    cfg.validate();

    //print('*** comm elim ***');

    // Perform common subexpression elimination on the CFG
    commElim(cfg);

    //print('*** done ***');

    // Validate the CFG
    cfg.validate();

    //print('*** validated ***');

    // Assume that the function does not read or write from/to memory
    cfg.ownerFunc.writesMem = false;
    cfg.ownerFunc.readsMem = false;

    // For each instructon in the CFG
    for (var itr = cfg.getInstrItr(); itr.valid(); itr.next())
    {
        var instr = itr.get();

        // If any instruction reads or writes from memory, annotate the
        // function as reading or writing memory
        if (instr.writesMem())
        {
            //print('******' + cfg.ownerFunc.funcName + ' writes mem: ' + instr);
            cfg.ownerFunc.writesMem = true;
        }
        if (instr.readsMem())
        {
            //print('******' + cfg.ownerFunc.funcName + ' reads mem: ' + instr);
            cfg.ownerFunc.readsMem = true;
        }
    }

    //if (!cfg.ownerFunc.writesMem)
    //    print('############ DOES NOT WRITE MEM: ' + cfg.ownerFunc.funcName);

    //print('*** lowering done ***');
}

