/**
@fileOverview
Code related to common/redundant code elimination.

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010 Maxime Chevalier-Boisvert, All Rights Reserved
*/

/*

Want to identify and eliminate redundant ops:
- Including loads
- Including get_ctx/set_ctx

Memory:
- Ops that write to memory kill ops that read from memory
- Ops that write to memory (and funcs that write) may not be combined
- In the case of get_ctx
  - Only set_ctx can kill this
- In the case of load/store
  - Non-overlapping store to same ptr doesn't kill... Ignore for now
  - Assume all writes kill reads
- Assume writes are never equivalent?
  - Writes kill all other writes
  - Unless exact same store... Ignore for now

Discovering two instrs congruent can possibly make their uses congruent as well
  - Should add their uses to work list?
  - Could handle this with congruence classes
    - Test if operands are same based on cong. classes/val numbers

Optimizing:
- Need work list on CFG to compute avail exprs
- Can only replace/simplify if we have must-reach
  - Can compute must reach out/in as analyzing
  - Can only replace use of a by b if b must reach
- If same expr on both side of branches, would like to move to end of pred
  - Can't if op reads and intervening writes
  - Can't if op writes
  - Can't if op has branches

Two questions:
- Which instrs computing the same thing?
- Which instrs must reach me

If ops computing same thing along all sides of branch, may want to try moving
them up to first common predecessor. This would reduce the amount of code.
- Try to move up instrs along all branches to end of pred
- Complicated if op has branches!
  - Can't really move to end of pred
- Basic case: op is in both immediate successors
- Must-be-computed relationship?
  - If equivalent op in all immediate successors, try to pull to pred?
  - Can merge on reach out set of succs?
  - Must add pred, succs to work list

Attempt #1

Algorithm:
- Uses CFG work list
- Traverse basic blocks, identify common exprs based on must-reach and equivalence
  - Simple must reach of exprs
  - For current op in current bb, does an equivalent op reach?
  - Does an op with the same val number reach?
  - Could have both must reach and cur reach hashes
- If expr must reach you, can immediately replace redundant exprs in current block
  - We know it must reach us at this point
  - Changes reach out set, must push succs on work list?
  - Its our dests that change
  - Info will trickle down to dests, we're no longer reaching them
  - Dests have new uses, will define new equiv classes
- If redundant exprs exist in succ, can try pulling up
  - Among ops defined *by succs*, are any the same in all succs
  - Set of congruence classes defined by all succs... Intersection

Need hash map of congruence classes to earliest instrs defining them
- Could possibly have global value number set
- Func takes instr as input, produces val number, manages equivalence classes
- Want to avoid recomputing val number, can map instrs by id to val nums

PROBLEM: not true that must reach at one point means must
reach before fixed point is over... We're using intersection here... GFP, not LFP

Attempt #2

1. Compute dominators using GFP first?
Essentially, must reach *blocks*
- Then, given an instr, can tell if it must reach from its parent block

2. Compute initial set of congruence classes
- Array mapping GVNs to sets of instrs

3. Can then use SSA work list
- Add all nodes to list that are in a class > 1

4. Take instr i out of work list
- Compute its current congruence class
- For all instrs in class, if any dominates, replace i by dominator
- Remove instr from its class
- Update all dests

PROBLEM: does not account for intervening kills of available expressions...

Attempt #3

Can only perform replacements once must reach instrs has stabilized
- Need to have inner loop operating on CFG
- Want to compute must reach at each instruction...
- Only care about the instrs in the same equivalence classes that reach

Possibly, we can compute value numbers for dests that are the same if they
use operands with the same equivalence classes.

ex:

c = a + b
d = a + b

e = c + 1
f = d + 1

e and f can get same equivalence class

Could avoid ever recomputing this information. Do fixed-point once only.

Can then add instrs to work list that have more than one in equiv class.

***PROBLEM: if two instrs use load p, 4, it doesn't mean they're using the
same value....

Should compute val nums based on immediate uses, not their val nos.
Must do FP of must reach...

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

    for (var itrCount = 0; changed && itrCount < maxItrs; ++itrCount)
    {
        //print('******************ITR*****************');

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
            var mustReachCur = (block.preds.length > 0)? fullReachSet.slice(0):[];
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

        // Set or removed/replaced instructions
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

