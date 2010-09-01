/**
@fileOverview
Implementation of high-level IR instructions through handler functions

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010 Maxime Chevalier-Boisvert, All Rights Reserved
*/

/**
Inline an IR function at a call site
*/
function inlineCall(callInstr, calleeFunc)
{
    /*
    TODO

    Copy the callee function's CFG [X]

    Add blocks from callee CFG to caller CFG [X]

    Create call resolution block [X]

    Replace all rets by jump to call resolution [X]
    - Need phi node for ret value in res block

    Potentially add throw target to all throw instructions [X]

    Replace arg instrs in entry block by call instruction args? [X]
    - Remove instrs, replace dests

    Make call res block jump to continuation block
    - If handler call in middle of block, need to move all instrs after the call
      to the resolution block

    Replace the call instruction by a jump to callee entry block

    Replace uses of the call by uses of the resolution phi
    */

    // Ensure that the call site is valid
    assert (
        callInstr instanceof CallFuncInstr ||
        callInstr instanceof ConstructInstr ||
        callInstr instanceof CallHandlerInstr,
        'call site must be call instruction'
    );

    // Ensure that the callee does not use the arguments object
    assert (
        !calleeFunc.usesArguments,
        'callee function uses arguments object'
    );

    //
    // TODO: handlers have no this value, func obj args
    //

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
    var phiRes = resBlock.addInstr(new PhiNode([], []));

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
            // Replace the return instruction by a jump
            block.replInstrAtIndex(
                new JumpInstr(resBlock),
                block.instrs.length - 1
            );

            // Add a phi predecessor for the return value
            phiRes.addIncoming(branchInstr.uses[0], block);
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
            for (var j = 0; j < instr.dests; ++j)
            {
                instr.dests[j].replUse(
                    instr, 
                    callInstr.uses[instr.argIndex]
                );
            }

            // Remove the instruction
            calleeCFG.entry.remInstrAtIndex(i);
            --i;
        }
    }



    /*
    Make call res block jump to continuation block
    - If handler call in middle of block, need to move all instrs after the call
      to the resolution block
    */

    /*
    Replace the call instruction by a jump to callee entry block

    Replace uses of the call by uses of the resolution phi
    */







}

