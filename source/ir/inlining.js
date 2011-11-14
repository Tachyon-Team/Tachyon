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
Implementation of high-level IR instructions through handler functions

@author
Maxime Chevalier-Boisvert
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
        callInstr instanceof CallFuncInstr,
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

    // TODO: should reduce default inlining during lowering
    // For now, inline LIR CFG if available, for speed
    //
    // Copy the callee function's High-level CFG
    //var calleeCFG = calleeFunc.hirCFG.copy();
    if (calleeFunc.lirCFG)
        var calleeCFG = calleeFunc.lirCFG.copy();
    else
        var calleeCFG = calleeFunc.hirCFG.copy();

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

        // Get the last instruction of the block
        var branchInstr = block.getLastInstr();

        // If the branch is a return instruction
        if (branchInstr instanceof RetInstr)
        {
            // Add a phi predecessor for the return value
            phiRes.addIncoming(branchInstr.uses[0], block);

            // Replace the return instruction by a jump
            block.replBranch(new JumpInstr(resBlock));
        }

        // If the call has a throw target
        if (throwTarget)
        {
            var curBlock = block;

            // For each instruction of the block
            for (var i = 0; i < curBlock.instrs.length; ++i)
            {
                var instr = curBlock.instrs[i];

                //print(instr);

                // If this is an exception producing instruction
                // with no throw target
                if (instr instanceof ExceptInstr && !instr.getThrowTarget())
                {
                    // Set the throw target for the instruction
                    instr.setThrowTarget(throwTarget);
                    curBlock.addSucc(throwTarget);
                    throwTarget.addPred(curBlock);

                    // If this is the last instruction of the block, stop
                    if (curBlock.getLastInstr() === instr)
                        break;

                    // Split the block to create a continuation target
                    var contBlock = callerCFG.splitBlock(curBlock, i + 1);

                    // Set the continuation for the instruction
                    if (instr.setContTarget)
                    {
                        instr.setContTarget(contBlock);
                        curBlock.addSucc(contBlock);
                        contBlock.addPred(curBlock);
                    }

                    // Process the continuation block next
                    curBlock = contBlock;
                    i = 0;
                }
            }
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

                //print(calleeFunc);
                //calleeFunc.validate();

                var argVal = 
                    instr.argIndex < callInstr.uses.length?
                    callInstr.uses[instr.argIndex + 1]:
                    IRConst.getConst(undefined);

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
    if (contTarget === null)
    {
        // For each successor of the call block
        for (var i = 0; i < callBlock.succs.length; ++i)
        {
            var succ = callBlock.succs[i];

            // For each phi instruction
            for (var j = 0; j < succ.instrs.length; ++j)
            {
                var instr = succ.instrs[j];

                if (!(instr instanceof PhiInstr))
                    continue;

                // Make the new phi predecessor the resolution block
                instr.replPred(callBlock, resBlock);
            }
        }

        // Find the index of the call instruction
        for (var ci = 0; callBlock.instrs[ci] !== callInstr; ++ci);

        // Move all instructions after the call to the resolution block
        while (callBlock.instrs.length - 1 > ci)
        {
            var instr = callBlock.instrs[ci + 1];
            callBlock.remInstrAtIndex(ci + 1);
            resBlock.addInstr(instr, instr.outName);
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

