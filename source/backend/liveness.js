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
Liveness analysis implementation. Needed by register allocation.

@author
Maxime Chevalier-Boisvert
*/

/**
Liveness analysis used by the register allocator
*/
function liveAnalysis(blockOrder)
{
    //
    // Liveness analysis
    //

    // Work list for the analysis
    var workList = new LinkedList();

    // Add the blocks to the work list, in reverse order
    for (var i = blockOrder.length - 1; i >= 0; --i)
        workList.addLast(blockOrder[i]);

    // Array to store live sets at the input of each block, after the phi nodes
    var blockLiveIn = [];

    // Array of sets of live variables before instructions
    var instrLiveIn = [];

    // Array of sets of live variables after instructions
    var instrLiveOut = [];

    // Until the work list is empty
    while (workList.isEmpty() === false)
    {
        var block = workList.remFirst();

        // Compute the union of the successor live in sets
        var liveCur = new HashMap();
        for (var i = 0; i < block.succs.length; ++i)
        {
            var succ = block.succs[i];

            var liveSucc = blockLiveIn[succ.blockId];
            if (liveSucc === undefined)
                continue;

            for (var itr = liveSucc.getItr(); itr.valid(); itr.next())
            {
                var value = itr.get().key;

                // If this value is a phi node from the successor,
                // add the incoming value from this block instead
                if (value instanceof PhiInstr && value.parentBlock === succ)
                    value = value.getIncoming(block);

                liveCur.setItem(value);
            }
        }

        // For each instruction, in reverse order
        for (var i = block.instrs.length - 1; i >= 0; --i)
        {
            var instr = block.instrs[i];

            // If this instruction is a phi node, stop
            if (instr instanceof PhiInstr)
                break;

            /*
            log.debug('instr live out: ' + instr);
            for (var itr = liveCur.getItr(); itr.valid(); itr.next())
                log.debug(itr.get().key.getValName());
            */

            // Remove the output of this instruction from the live set
            if (instr.dests.length > 0 && liveCur.hasItem(instr) === true)
                liveCur.remItem(instr);

            // Store the live set at the output of this instruction
            if (i === block.instrs.length - 1)
                var curLiveSet = liveCur.copy();
            else
                var curLiveSet = instrLiveIn[block.instrs[i+1].instrId];
            instrLiveOut[instr.instrId] = curLiveSet;

            // For each use of the instruction
            for (var j = 0; j < instr.uses.length; ++j)
            {
                var use = instr.uses[j];

                // Map this use in the live set
                liveCur.setItem(use);
            }

            // Store the live set at the input of this instruction
            instrLiveIn[instr.instrId] = liveCur.copy();
        }

        // Find the current live in set for this block
        var liveInCur = blockLiveIn[block.blockId];

        // If the new live set has more temps
        if (liveInCur === undefined || liveInCur.numItems !== liveCur.numItems)
        {
            // Replace the live in set for this block
            blockLiveIn[block.blockId] = liveCur;

            // Add all predecessors to the work list
            for (var i = 0; i < block.preds.length; ++i)
                workList.addLast(block.preds[i]);
        }
    }

    return {
        blockIn: blockLiveIn,
        instrIn: instrLiveIn,
        instrOut: instrLiveOut
    };
}

