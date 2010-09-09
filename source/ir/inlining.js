/**
@fileOverview
Implementation of high-level IR instructions through handler functions

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010 Maxime Chevalier-Boisvert, All Rights Reserved
*/

/**
Test if a callee function can be inlined
*/
function isInlinable(calleeFunc)
{
    // Cannot inline functions that use the arguments object
    if (calleeFunc.usesArguments)
        return false;

    // The callee is inlinable
    return true;
}

/**
Inline an IR function at a call site
*/
function inlineCall(callInstr, calleeFunc)
{
    // Ensure that the call site is valid
    assert (
        callInstr instanceof CallFuncInstr ||
        callInstr instanceof ConstructInstr,
        'call site must be call instruction'
    );

    // Ensure that the callee function is inlinable
    assert (
        isInlinable(calleeFunc),
        'callee function is not inlinable'
    );

    // Get the call instruction's continuation and throw target
    var contTarget = callInstr.getContTarget();
    var throwTarget = callInstr.getThrowTarget();

    // Get a reference to the caller's CFG
    var callerCFG = callInstr.parentBlock.parentCFG;

    // Copy the callee function's CFG
    var calleeCFG = calleeFunc.virginCFG.copy();

    // Create a block for the call resolution
    var resBlock = callerCFG.getNewBlock('call_res');

    // Create a phi node for the return value resolution
    var phiRes = resBlock.addInstr(new PhiInstr([], []), 'phires');

    // For each callee block
    for (var itr = calleeCFG.getBlockItr(); itr.valid(); itr.next())
    {
        var block = itr.get();

        // Add the block to the caller CFG
        callerCFG.addBlock(itr.get());

        // If this block has no final branch, continue
        if (!block.hasBranch())
            continue;

        // Get the last instruction of the block
        var branchInstr = block.getLastInstr();

        // If the branch is a return instruction, continue
        if (branchInstr instanceof RetInstr)
        {
            // Add a phi predecessor for the return value
            phiRes.addIncoming(branchInstr.uses[0], block);

            // Replace the return instruction by a jump
            block.replInstrAtIndex(
                new JumpInstr(resBlock),
                block.instrs.length - 1
            );
        }

        // If the call instruction has a throw target and the branch is an 
        // exception producing instruction with no throw target
        if (throwTarget &&
            branchInstr instanceof ExceptInstr &&
            !branchInstr.getThrowTarget()
        )
        {
            // Set the throw target for the branch instruction
            branchInstr.setThrowTarget(throwTarget);
            block.addSucc(throwTarget);
            throwTarget.addPred(block);
        }
    }

    // For each instruction of the callee entry block
    for (var i = 0; i < calleeCFG.entry.instrs.length; ++i)
    {
        var instr = calleeCFG.entry.instrs[i];

        // If this is an argument value instruction
        if (instr instanceof ArgValInstr)
        {
            // Replace uses of this instruction by the argument value
            for (var j = 0; j < instr.dests.length; ++j)
            {
                var dest = instr.dests[j];

                var argVal = 
                    instr.argIndex < callInstr.uses.length?
                    callInstr.uses[instr.argIndex]:
                    ConstValue.getConst(undefined)
                ;

                dest.replUse(
                    instr,
                    argVal
                );

                if (argVal instanceof IRInstr)
                    argVal.addDest(dest);
            }

            // Remove the instruction
            calleeCFG.entry.remInstrAtIndex(i);
            --i;
        }
    }

    // Get a reference to the call block
    var callBlock = callInstr.parentBlock;

    // If the call is in the middle of a basic block
    if (!contTarget)
    {
        // Find the index of the call instruction
        for (var ci = 0; callBlock.instrs[ci] !== callInstr; ++ci);

        // Move all instructions after the call to the resolution block
        while (callBlock.instrs.length - 1 > ci)
        {
            var instr = callBlock.instrs[ci + 1];
            callBlock.remInstrAtIndex(ci + 1);
            resBlock.addInstr(instr, instr.outName);
        }



        // For each successor of the resolution block
        for (var i = 0; i < resBlock.succs.length; ++i)
        {
            var succ = resBlock.succs[i];

            // For each phi instruction
            for (var j = 0; j < succ.instrs.length; ++j)
            {
                var instr = succ.instrs[j];

                if (!(instr instanceof PhiInstr))
                    continue;

                instr.replPred(callBlock, resBlock);
            }
        }



    }

    // Otherwise, the call is at the end of a basic block
    else
    {
        // Make the resolution block jump to the continuation block
        resBlock.addInstr(new JumpInstr(contTarget));

        // Replace the call block by the res block as a continuation predecessor
        contTarget.remPred(callBlock);
        contTarget.addPred(resBlock);
        for (var i = 0; i < contTarget.instrs.length; ++i)
        {
            var instr = contTarget.instrs[i];

            if (!(instr instanceof PhiInstr))
                continue;

            instr.replPred(callBlock, resBlock);
        }
    }

    // Replace uses of the return value by uses of the resolution phi
    for (var i = 0; i < callInstr.dests.length; ++i)
    {
        var dest = callInstr.dests[i];

        dest.replUse(
            callInstr,
            phiRes
        );

        phiRes.addDest(
            dest
        );
    }

    // Replace the call instruction by a jump to the callee entry block
    callBlock.remInstrAtIndex(
        callBlock.instrs.length - 1
    );
    callBlock.addInstr(
        new JumpInstr(calleeCFG.entry)
    );
}

