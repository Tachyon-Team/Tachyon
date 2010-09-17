/**
@fileOverview
Implementation of constant propagation.

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010 Maxime Chevalier-Boisvert, All Rights Reserved
*/

/**
Perform sparse conditional constant propagation on a CFG
*/
function constProp(cfg)
{
    // Constant for values not yet known
    var TOP = 'TOP';

    // Constant for value known to be non-constant
    var BOT = 'BOT';

    // List of CFG blocks to be processed
    var cfgWorkList = [cfg.entry];

    // List of SSA edges to be processed *** edges, pairs?
    var ssaWorkList = [];

    // Reachable blocks, indexed by block id
    var reachable = [];

    // Instruction values, indexed by instr id
    var instrVals = [];

    // Initialize all instruction values to top
    for (var itr = cfg.getInstrItr(); itr.valid(); itr.next())
        instrVals[itr.get().instrId] = TOP;

    // Until a fixed point is reached
    while (cfgWorkList.length > 0 || ssaWorkList.length > 0)
    {
        // Until the CFG work list is processed
        while (cfgWorkList.length > 0)
        {
            // Remove a block from the CFG work list
            var b = cfgWorkList.pop();

            // Mark b as reachable
            reachable[b.blockId] = true;

            // For each instruction in b
            for (var i = 0; i < b.instrs.length; ++i)
            {
                var instr = b.instrs[i];

                // Evaluate the instruction for the first time
                evalInstr(instr);

                // For each dest of the instruction
                for (var j = 0; j < instr.dests; ++j)
                {
                    var dest = instr.dests[j];

                    // If the block of the destination is reachable
                    if (reachable[dest.parentBlock.blockId])
                    {
                        // Add the dest to the SSA work list
                        ssaWorkList.push(dest);
                    }
                }
            }
        }

        // Until the SSA work list is processed
        while (ssaWorkList.length > 0)
        {
            // Remove an edge from the SSA work list
            var v = ssaWorkList.pop();

            // Evaluate the value of the edge dest
            var t = evalInstr(v);

            print(t);

            // If the instruction value has changed
            if (t !== instrVals[v.instrId])
            {
                // Update the value for this instruction
                instrVals[v.instrId] = t;
                
                // For each dest of v
                for (var i = 0; i < v.dests.length; ++i)
                {
                    var dest = v.dests[i];

                    // If the block of the destination is reachable
                    if (reachable[dest.parentBlock.blockId])
                    {
                        // Add the dest to the SSA work list
                        ssaWorkList.push(dest);
                    }
                }
            }
        }
    }

    // Add a block to the CFG work list
    function processBlock(block)
    {
        if (!reachable[block.blockId])
            cfgWorkList.push(block);
    }

    // Evaluate an SSA instruction
    function evalInstr(instr)
    {
        // When evaluating branches, must put targets on work list as appropriate

        /*
        // If this is an if instruction
        if (instr instanceof IfInstr)
        {
            var test = instrVals[instr.uses[0].instrId];

            // If test is non-constant, both branches are reachable
            if (test === BOT)
            {
                processBlock(instr.targets[0]);
                processBlock(instr.targets[1]);
            }


            // TODO: evaluate truthiness of the argument


            // TODO: for top, put no branches on the list, do nothing

        }
        */

        // If this instruction is a generic branch
        /*else*/ if (instr.isBranch())
        {
            // Put all branches on the CFG work list
            for (var i = 0; i < instr.targets.length; ++i)
            {
                if (instr.targets[i])
                    processBlock(instr.targets[i]);
            }
        }

        
        // TODO: evaluate phi functions
        // Take reachability of preds into account




        // Return the non-constant value
        return BOT;
    }
    

    //
    // TODO: integrate some peephole patterns into evalInstr?
    //

    // TODO:
    // At end, optimize code using instruction values
    // Can use replInstr to replace instructions by constant values
}

