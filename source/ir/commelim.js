/**
@fileOverview
Code related to common/redundant code elimination.

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010 Maxime Chevalier-Boisvert, All Rights Reserved
*/

/**
Perform common/redundant code elimination on a CFG
*/
function commElim(cfg, maxItrs)
{
    // If no maximum iteration count was specified, there is no limit
    if (!maxItrs)
        maxItrs = Infinity;

    // Hashing function for IR values        
    function hashFunc(val)
    {
        assert (val instanceof IRInstr);

        var valStr = val.mnemonic;
        for (var i = 0; i < val.uses.length; ++i)
            valStr += val.uses[i].getValName();

        /*
        for (var i = 0; i < val.targets.length; ++i)
            if (val.targets[i])
                valStr += val.targets[i].getBlockName();
        */

        return defHashFunc(valStr);
    }

    // Equality function for IR values
    function equalFunc(val1, val2)
    {
        assert (val1 instanceof IRInstr && val2 instanceof IRInstr);

        if (val1 instanceof ArgValInstr || 
            val1 instanceof PhiInstr)
            return false;

        if (val1.mnemonic != val2.mnemonic)
            return false;

        if (val1.uses.length != val2.uses.length)
            return false;

        for (var i = 0; i < val1.uses.length; ++i)
            if (val1.uses[i] !== val2.uses[i])
                return false;

        /*
        print('equal 1: ' + val1);
        print('equal 2: ' + val2);
        print('hash 1: ' + hashFunc(val1));
        print('hash 2: ' + hashFunc(val2));
        */

        /*
        for (var i = 0; i < val1.targets.length; ++i)
            if (val1.targets[i] !== val2.targets[i])
                return false;
        */

        /*
        print('got match');
        print(val1);
        print(val2);
        */
  
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
        if (valNoCache[val.instrId])
            return valNoCache[val.instrId];

        // If this value matches an existing value number, get that number
        // otherwise, assign it a new value number
        if (valNoHash.hasItem(val))
        {
            //print('*******GOT VAL**********');
            var valNo = valNoHash.getItem(val);
        }
        else
        {
            var valNo = valNoHash.numItems;
            valNoHash.addItem(val, valNo);
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
    for (var itrCount = 0; changed && itrCount < maxItrs; ++itrCount)
    {
        /*
        print('******************ITR*****************');
        cfg.validate();
        */

        // No changes in this iteration yet
        changed = false;

        // Reset the value number hash and cache
        valNoHash.clear();
        valNoCache = [];

        // Value reaching an instruction, indexed by instruction id
        var reachInstr = [];

        // Sets of values reaching the exit basic blocks, indexed by block id
        var mustReachOut = [];

        // Compute the set of all definitions in the CFG
        var fullReachSet = [];
        for (var itr = cfg.getInstrItr(); itr.valid(); itr.next())
            fullReachSet.push(itr.get());

        // Initialize the reaching def sets for all blocks
        for (var i = 0; i < cfg.blocks.length; ++i)
        {
            var block = cfg.blocks[i];
            mustReachOut[block.blockId] = fullReachSet;
        }

        // Work list of CFG blocks to examine
        var workList = [cfg.entry];

        // Until the work list is empty
        while (workList.length != 0)
        {
            var block = workList.pop();

            // Compute the must and may reach sets at this block's entry
            var mustReachCur = (block.preds.length > 0)? fullReachSet:[];
            for (var i = 0; i < block.preds.length; ++i)
            {
                var pred = block.preds[i];
                mustReachCur = arraySetIntr(mustReachCur, mustReachOut[pred.blockId]);
            }

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
            INSTR_LOOP:
            for (var i = 0; i < block.instrs.length; ++i)
            {
                var instr = block.instrs[i];

                // Get the value number for this instruction
                var valNo = getValNo(instr);            

                // If this instruction writes memory, kill any reaching instruction
                // that reads memory, except get_ctx
                if (instr.writesMem())
                {
                    for (var j = 0; j < mustReachCur.length; ++j)
                    {
                        var rinstr = mustReachCur[j];
                        if (rinstr.readsMem() && !(rinstr instanceof GetCtxInstr))
                        {
                            /*
                            print('killing: ' + rinstr);
                            print('with: ' + instr);
                            */

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
                for (var j = 0; j < mustReachCur.length; ++j)
                {
                    var rinstr = mustReachCur[j];
                    if (getValNo(rinstr) == valNo)
                    {
                        // Note that the instruction reaches here
                        reachInstr[instr.instrId] = rinstr;

                        // Don't add the current instruction to the reach set
                        continue INSTR_LOOP;
                    }
                }

                // Add the instruction to the set of reaching values
                arraySetAdd(mustReachCur, instr);
            }
            
            // If the must reach set has changed for this block
            if (!arraySetEqual(mustReachCur, mustReachOut[block.blockId]))
            {
                // Update the sets for this block
                mustReachOut[block.blockId] = mustReachCur;

                // Add the successors of this block to the work list
                for (var i = 0; i < block.succs.length; ++i)
                    workList.push(block.succs[i]);
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
                rinstr && !arraySetHas(remSet, rinstr))
            {
                /*                
                print('********************');
                print(instr);
                print(rinstr);
                */

                // Replace uses of the instruction
                for (var i = 0; i < instr.dests.length; ++i)
                {
                    var dest = instr.dests[i];
                    dest.replUse(instr, rinstr);
                    rinstr.addDest(dest);
                }

                // Remove the instruction
                cfg.remInstr(itr, rinstr);

                // If this is a call instruction in a branch position,
                // add a jump to the call continuation block
                if (instr instanceof CallInstr && instr.isBranch())
                {
                    //print('*** replacing branch call: ' + instr);
                    //print('*** by: ' + rinstr);
                    var callBlock = instr.parentBlock;
                    callBlock.addInstr(
                        new JumpInstr(instr.getContTarget()),
                        undefined,
                        callBlock.instrs.length - 1
                    );
                }

                // Add the instruction to the set of removed instructions
                arraySetAdd(remSet, instr);

                // Set the changed flag
                changed = true;
            }
        }
    }
}

