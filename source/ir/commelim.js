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
Code related to common/redundant code elimination.

@author
Maxime Chevalier-Boisvert
*/

/**
Perform common/redundant code elimination on a CFG
*/
function commElim(cfg, maxItrs)
{
    // Hashing function for IR values        
    function hashFunc(val)
    {
        assert (
            val instanceof IRInstr,
            'expected instruction value'
        );

        var hashVal;

        hashVal = val.mnemonic.length;
        hashVal = (hashVal << 1) + val.mnemonic.charCodeAt(0);
    
        for (var i = 0; i < val.uses.length; ++i)
        {
            var use = val.uses[i];

            var no;
            if (use instanceof IRInstr)
                no = use.instrId;
            else if (typeof use.value === 'number')
                no = use.value;
            else
                no = 0;

            hashVal = (((hashVal << 1) + no) & 536870911) % 426870919;
        }

        return hashVal;
    }

    // Equality function for IR values
    function equalFunc(val1, val2)
    {
        assert (
            val1 instanceof IRInstr && val2 instanceof IRInstr,
            'expected instruction values'
        );

        if (val1 instanceof ArgValInstr || 
            val1 instanceof PhiInstr)
            return false;

        if (val1.mnemonic !== val2.mnemonic)
            return false;

        if (val1.uses.length !== val2.uses.length)
            return false;

        for (var i = 0; i < val1.uses.length; ++i)
            if (val1.uses[i] !== val2.uses[i])
                return false;
  
        return true;
    }

    // Value number hash map, indexed by IR values
    var valNoHash = new HashMap(hashFunc, equalFunc);

    // Value number cache, indexed by instruction id
    var valNoCache = [];

    // Function to get a value number for an instruction
    function getValNo(val)
    {
        // If the value number is already computed, return it
        if (valNoCache[val.instrId] !== undefined)
            return valNoCache[val.instrId];

        // Try to find the value number in the hash map
        var valNo = valNoHash.get(val);

        // If this value matches an existing value number, get that number
        // otherwise, assign it a new value number
        if (valNo === HashMap.NOT_FOUND)
        {
            valNo = valNoHash.numItems;
            valNoHash.set(val, valNo);
        }

        // Store the value number in the value number cache
        valNoCache[val.instrId] = valNo;

        return valNo;
    }




























    /*
    print('********************\n');
    for (var itr = cfg.getInstrItr(); itr.valid(); itr.next())
        print(itr.get() + " ==> " + getValNo(itr.get()));
    print('********************\n');
    */

    // Flag to indicate a change occurred
    var changed = true;

    // Until no changes occur or the max iteration count is reached
    for (var itrCount = 0;
        changed && (maxItrs === undefined || itrCount < maxItrs);
        ++itrCount
    )
    {
        /*
        print('******************ITR*****************');
        try
        {        
            cfg.validate();
        }
        catch (e)
        {
            print('\n' + cfg.ownerFunc + '\n');
            throw e;
        }
        */

        // No changes in this iteration yet
        changed = false;

        // Reset the value number hash and cache
        valNoHash.clear();
        valNoCache = [];

        // Value reaching an instruction, indexed by instruction id
        var reachInstr = [];

        // Work list for the analysis
        var workList = new LinkedList();
        var workListSet = [];

        // Add the entry block to the work list
        workList.addLast(cfg.entry);
        workListSet[cfg.entry.blockId] = true;

        // Array to store must reach input sets for each block
        var mustReachIn = [];

        // Initialize the must reach in set for the entry block
        mustReachIn[cfg.entry.blockId] = [];

        // Compute the set of blocks in a merge position
        var mergeBlocks = [];
        for (var itr = cfg.getBlockItr(); itr.valid(); itr.next())
        {
            var block = itr.get();

            // Count the number of successors with only us as a predecessor
            var numSinglePred = 0;
            for (var i = 0; i < block.succs.length; ++i)
            {
                var succ = block.succs[i];
                if (succ.preds.length === 1)
                    ++numSinglePred;
            }

            // For each successor
            for (var i = 0; i < block.succs.length; ++i)
            {
                var succ = block.succs[i];

                // This block is in a merge position if it has more than
                // one predecessor, or if there is more than one block with
                // only us as a predecessor
                mergeBlocks[succ.blockId] = (
                    succ.preds.length > 1 || numSinglePred > 1
                );
            }
        }

        // Until the work list is empty
        while (workList.isEmpty() === false)
        {
            var block = workList.remFirst();
            workListSet[block.blockId] = false;

            /*
            print(
                block.getBlockName() + 
                ', preds: ' + block.preds.length +
                ', succs: ' + block.succs.length
            );
            */

            // Get the must reach set at this block's entry
            var mustReachCur = mustReachIn[block.blockId];

            // If this block is in a merge position, copy the must reach
            // set before modifying it
            if (mergeBlocks[block.blockId] === true)
                mustReachCur = mustReachCur.slice(0);

            // Remove return values flowing through exception edges
            for (var i = 0; i < mustReachCur.length; ++i)
            {
                var instr = mustReachCur[i];
                if (instr instanceof CallFuncInstr && 
                    instr.getThrowTarget() === block)
                {
                    mustReachCur.splice(i, 1);
                    --i;
                }
            }

            // For each instruction
            for (var i = 0; i < block.instrs.length; ++i)
            {
                var instr = block.instrs[i];

                assert (
                    arraySetHas(mustReachCur, instr) === false,
                    'INSTR REACHING SELF'
                );

                // Get the value number for this instruction
                var valNo = getValNo(instr);            

                // If this instruction writes memory, kill any reaching instruction
                // that reads or writes memory, except get_ctx
                if (instr.writesMem())
                {
                    for (var j = 0; j < mustReachCur.length; ++j)
                    {
                        var rinstr = mustReachCur[j];
                        if ((rinstr.readsMem() || rinstr.writesMem()) && 
                            !(rinstr instanceof GetCtxInstr))
                        {
                            //print('killing: ' + rinstr);
                            //print('with: ' + instr);

                            mustReachCur.splice(j, 1);
                            --j;
                        }
                    }
                }

                // If this is a set_ctx instruction, kill any reaching get_ctx
                if (instr instanceof SetCtxInstr)
                {
                    for (var j = 0; j < mustReachCur.length; ++j)
                    {
                        if (mustReachCur[j] instanceof GetCtxInstr)
                        {
                            mustReachCur.splice(j, 1);
                            --j;
                        }
                    }
                }

                // Unmark any previously found reaching instruction
                reachInstr[instr.instrId] = undefined;

                // If an instruction with the same value number must reach this
                for (var j = mustReachCur.length - 1; j >= 0; --j)
                {
                    var rinstr = mustReachCur[j];
                    if (getValNo(rinstr) === valNo)
                    {
                        //print('  &&& found repl: ' + instr + ' ==> ' + rinstr.instrId);

                        // Note that the instruction reaches here
                        reachInstr[instr.instrId] = rinstr;

                        // Don't add the current instruction to the reach set
                        break;
                    }
                }

                // No instruction with the same value number reaches here
                if (j < 0)
                {
                    // Add the instruction to the set of reaching values
                    arraySetAdd(mustReachCur, instr);
                }
            }

            // For each successor
            for (var i = 0; i < block.succs.length; ++i)
            {
                var succ = block.succs[i];

                //print('succ: ' + succ.getBlockName());

                // If the successor is in a merge position
                if (mergeBlocks[succ.blockId] === true)
                {
                    var succReachIn = mustReachIn[succ.blockId];

                    // Merge with the must reach in of the successor
                    if (succReachIn !== undefined)
                    {
                        mustReachIn[succ.blockId] = arraySetIntr(
                            mustReachCur,
                            succReachIn
                        );
                    }
                    else
                    {
                        mustReachIn[succ.blockId] = mustReachCur.slice(0);
                    }

                    // If the must reach in set changed, add the succ to the work list
                    if (succReachIn === undefined || 
                        arraySetEqual(mustReachIn[succ.blockId], succReachIn) === false)
                    {
                        //print('ADDING BLOCK LAST: ' + succ.getBlockName());

                        if (workListSet[succ.blockId] !== true)
                        {
                            workList.addLast(succ);
                            workListSet[succ.blockId] = true;
                        }
                    }
                }

                // Otherwise, this successor has only one predecessor (us), and
                // it is the only such block among our successors
                else
                {
                    // Give it our output reach set as input without
                    // copying the set
                    mustReachIn[succ.blockId] = mustReachCur;

                    //print('ADDING BLOCK FIRST: ' + succ.getBlockName());

                    // Make the successor next on the work list
                    workList.addFirst(succ);
                    workListSet[succ.blockId] = true;
                }
            }
        }
        
        // Set of removed/replaced instructions
        var remSet = [];

        // For each instruction in the CFG
        for (var itr = cfg.getInstrItr(); itr.valid(); itr.next())
        {
            var instr = itr.get();

            var rinstr = reachInstr[instr.instrId];

            // If the instruction is a call or a non-branch instruction and 
            // there is a replacement, which was not previously removed
            if ((instr instanceof CallInstr || !instr.isBranch()) && 
                rinstr !== undefined && !arraySetHas(remSet, rinstr))
            {
                /*   
                print('********************');
                print(instr + ' ==> ' + rinstr);
                */

                // If this is a call instruction in a branch position,
                // add a jump to the call continuation block
                var replBranch = 
                    (instr instanceof CallInstr && instr.isBranch())?
                    new JumpInstr(instr.getContTarget()):
                    undefined;

                // Replace the instruction by the value
                cfg.replInstr(itr,
                    replBranch,
                    rinstr
                );

                // Add the instruction to the set of removed instructions
                arraySetAdd(remSet, instr);

                // Set the changed flag
                changed = true;
            }
        }
    }
}

