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
Implementation of high-level IR lowering and specialization in preparation
for code generation.

@author
Maxime Chevalier-Boisvert
*/

//=============================================================================
//
// HIR function lowering code
//
//=============================================================================

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

    measurePerformance(
        "inlining/transform",
        function ()
        {
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

                    // Add the unbox instruction before the load
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

                // If this is an HIR instruction
                if (instr instanceof HIRInstr)
                {
                    //print('lowering HIR instruction: ' + instr);

                    // If HIR instruction is found, split current block.
                    // Create IR conversion context?

                    var instrBlock = instr.parentBlock;
                    var instrIndex = itr.instrIt.getIndex();

                    // Split the block containing the instruction
                    var exitBlock = cfg.splitBlock(instrBlock, instrIndex);

                    // Get the throw target for the HIR instruction
                    var throwTarget = instr.getThrowTarget();

                    // List of exception throwing instructions
                    var throwCtxList = throwTarget? []:null;

                    // Create an IR conversion context for the lowering
                    var context = new IRConvContext(
                        null, 
                        instrBlock,
                        null,
                        null,
                        null,
                        null,
                        null,
                        null,
                        null,
                        throwCtxList,
                        cfg,
                        cfg.ownerFunc,
                        ConstValue.getConst(undefined),
                        params
                    );

                    // Perform lowering for this instruction
                    instr.lower(context);

                    // Make the flow jump to the exit block
                    context.getExitBlock().addInstr(new JumpInstr(exitBlock));

                    // Replace the HIR instruction by its new value
                    exitBlock.replInstrAtIndex(
                        0,
                        instr.isBranch()? 
                        new JumpInstr(instr.getContTarget()):undefined,
                        context.getOutValue()
                    )

                    // If the instruction may throw
                    if (throwTarget)
                    {
                        // For each throw context
                        for (var c in throwCtxList)
                        {
                            var throwExit = throwCtxList[c].getExitBlock();

                            // Get the last instruction (the throw instruction) in the block
                            var throwInstr = throwExit.getLastInstr();

                            // Set the throw target to the catch block
                            throwInstr.setThrowTarget(throwTarget);

                            // Make the catch block a successor of the throw block
                            throwExit.addSucc(throwTarget);
                            throwTarget.addPred(throwExit);
                        }
                    }

                    var instr = itr.get();
                }

                // If this is a function call to a known function
                if (instr instanceof CallFuncInstr && instr.getCallee() instanceof IRFunction)
                {
                    var calleeFunc = instr.getCallee();

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
        }
    );

    measurePerformance(
        "opt patterns pass 1",
        function ()
        {
            //print('*** applying patterns ***');

            // Apply peephole optimization patterns to the CFG
            applyPatternsCFG(cfg, params);

            //print('*** validating after patterns ***');

            // Validate the CFG
            if (DEBUG)
                cfg.validate();
        }
    );

    //print(cfg.ownerFunc);

    measurePerformance(
        "const prop",
        function ()
        {
            //print('*** const prop ***');

            // Perform constant propagation on the CFG
            constProp(cfg, params);

            //print(cfg.ownerFunc);

            // Validate the CFG
            if (DEBUG)
                cfg.validate();
        }
    );

    measurePerformance(
        "opt patterns pass 2",
        function ()
        {
            // Apply peephole optimization patterns to the CFG
            applyPatternsCFG(cfg, params);

            // Validate the CFG
            if (DEBUG)
                cfg.validate();
        }
    );

    measurePerformance(
        "comm elim",
        function ()
        {
            //print('*** comm elim ***');

            // Perform common subexpression elimination on the CFG
            commElim(cfg);

            //print('*** done ***');

            // Validate the CFG
            if (DEBUG)
                cfg.validate();
        }
    );

    measurePerformance(
        "r/w analysis",
        function ()
        {
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
        }
    );

    //print('*** lowering done ***');
}

//=============================================================================
//
// HIR instruction lowering functions
//
//=============================================================================

/*
TODO: 
- Reintroduce HIRInstr class with descendents for arith ops, putprop, getprop.
- Have lowering function for each HIRInstr class
- Default lowering can be translation to primitive call
- Perform const prop on IR before lowering begins, using HIR instructions
*/

/*
TODO #1: implement function to split a basic block into two

TODO #2: begin by implementing lowering/specialization for HIR add, getprop only,
use this to perfect structure. Other HIR instructions still primitive calls for
now.


Ideally, would like to reuse IR generation code. Could produce an IR conversion
context with a new basic block for lowering, jump to the new basic block. Patch
jump to next block in the CFG afterwards. Either way, will need to insert new
code in the middle of a basic block.

This is not really necessary if simply inlining. Already done by inlining
function. Could modify current inlining function to work in the same way.
Pre-split caller block? No. Need inlining function as-is for inlining
optimizations!
Solution: replace HIR instruction by call, have inlining pass happen after.

HIRInstr.lower(entryBlock)
- Returns exit block?
- Add jump to next block

This implies that we have to split the current block. May actually be faster
than inserting many instructions in the middle of it... Want unified treatment.
*/





/*
Better code generation idea?

PROBLEM: generating code using the ast-to-ir framework is tedious, error-prone.
Code generation code becomes big, hard to read. The framework itself may
become limiting in the end.

Could, instead of generating IR, generate source for a function to be
compiled/called/inlined. Generating source code this way is much easier.
Generated function can also be pre-optimized.

PROBLEM: generating source code at every call site is slow!

Solution: we could employ some kind of contextual memoization scheme. Given
some flags/attributes indicating what we know about the use of a primitive
(likely types, etc.), generate a specialized primitive function only once for
this given HIR instruction.

Could also use this to have some specialized general-case functions eventually.

How would we implement this?
- Can scrap code for context-based lowering, implement by replacing the HIR
  instruction by a primitive call. Lowering function returns the chosen
  primitive to be inserted.
- Already have code to generate/compile SRC for primitive, not difficult.
  - Can simplify this further with a helper function
- Need some kind of memoizer/memoization engine.




*/








/**
TODO: implement default lowering function which simply does inlining
- Use for HasPropInstr
*/
function makeLowerFunc(primName)
{
}

/**
HIR add instruction
*/
/*HIRAddInstr.prototype.lower = function (ctx)
{
}*/


/*
TODO:
PROBLEM: global fetches need a property existence check...
May need a GetGlobalInstr
Can reuse lower func from GetProp? No, need no is object check.
*/





/**
HIR getProp instruction
*/
GetPropInstr.prototype.lower = function (ctx)
{
    /*
    Current workings:
    - Check if array
    - Check if string
    - Check if box int
    - Default object case

    Opt ideas:
    - Check if prop name is string constant
      - Check if prop is likely from array, string, number proto
      - Check if prop name is length
    - If prop name is not constant, likely array access

    - Can try one or two quick guesses. If wrong, call getPropVal general case.
    - If no guess, just call getPropVal

    - Can we know anything about the receiver object?
      - We can know if it's a new array, or is likely to be one
    */

    // Get the receiver and property name values
    var receiver = this.uses[0];
    var propName = this.uses[1];

    //print('prop name: ' + propName);

    // If the property name is a constant string    
    if (propName instanceof ConstValue && typeof propName.value === 'string')
    {
        var propName = propName.value;

        //print('prop name is cst str');

        if (propName === 'length')
        {
            /*
            // If this is an array
            if (boxIsArray(obj))
                return boxInt(iir.icast(IRType.pint, get_arr_len(obj)));
            else
                getPropVal
            Insert phi node
            */

            // Check if the receiver is an array
            var isArray = insertPrimCallIR(ctx, 'boxIsArray', [receiver]);

            // Compile the true expression
            var trueCtx = ctx.branch(
                null,
                ctx.cfg.getNewBlock('is_array'),
                null
            );
            var lenVal = insertPrimCallIR(trueCtx, 'get_arr_len', [receiver]);
            var lenInt = trueCtx.addInstr(new ICastInstr(IRType.pint, lenVal));
            var propVal = insertPrimCallIR(trueCtx, 'boxInt', [lenInt]);
            trueCtx.setOutput(trueCtx.entryBlock, propVal);

            // Compile the false expression
            var falseCtx = ctx.branch(null,
                ctx.cfg.getNewBlock('not_array'),
                null
            );
            var propVal = insertPrimCallIR(falseCtx, 'getPropVal', this.uses);
            falseCtx.setOutput(falseCtx.entryBlock, propVal);

            // Create the if branching instruction
            ctx.addInstr(
                new IfInstr(
                    isArray,
                    trueCtx.entryBlock,
                    falseCtx.entryBlock
                )
            );

            // Merge the local maps using phi nodes
            var joinBlock = ctx.cfg.getNewBlock('cond_join');
            trueCtx.addInstr(new JumpInstr(joinBlock));
            falseCtx.addInstr(new JumpInstr(joinBlock));
            var phiValue = joinBlock.addInstr(
                new PhiInstr(
                    [trueCtx.getOutValue(), falseCtx.getOutValue()],
                    [trueCtx.getExitBlock(), falseCtx.getExitBlock()]
                )
            );

            // Set the exit block to be the join block
            ctx.setOutput(joinBlock, phiValue);

            // Done generating code
            return;
        }

        /*
        else if (propName === 'prototype')
            print('prototype!');

        else if (propName in Array.prototype)
            print('array func!');

        else if (propName in String.prototype)
            print('string func!');

        else if (propName in Number.prototype)
            print('number func!');
        */
    }

    // Call the generic property access function
    var val = insertPrimCallIR(ctx, 'getPropVal', this.uses);

    // Set the operator's output value as the output
    ctx.setOutput(ctx.entryBlock, val);
}

