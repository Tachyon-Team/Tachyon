/**
@fileOverview
Implementation of high-level IR lowering and specialization in preparation
for code generation.

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010 Maxime Chevalier-Boisvert, All Rights Reserved
*/

/**
Apply peephole optimization patterns to a CFG
*/
function applyPatternsCFG(cfg, maxItrs)
{
    // If no maximum iteration count is set, there is no maximum
    if (!maxItrs)
        maxItrs = Infinity;

    // Flag to indicate that changes occurred in the CFG
    var changed = true;

    // Repeat until no more changes occur or the max iteration count is reached
    for (var itrCount = 0; changed && itrCount < maxItrs; ++itrCount)
    {
        //print('*** itr ' + itrCount);

        // No changes in this iteration yet
        changed = false;

        // For each block in the CFG
        for (var i = 0; i < cfg.blocks.length; ++i)
        {
            var block = cfg.blocks[i];

            // Apply block-level patterns to the block
            var result = applyPatternsBlock(cfg, block);

            // If any changes occurred, set the changed flag
            if (result)
                changed = true;
        }
    }

    // Return the iteration count
    return itrCount;
}

/**
Apply block-level optimization patterns to a block
*/
function applyPatternsBlock(cfg, block)
{
    //
    // Eliminate blocks of the form:
    // t = phi x1,x2,x3
    // if t b1 b2
    //

    // If this block contains only an if instruction using the
    // value of an immediately preceding phi
    if (
        block.instrs.length == 2 &&
        block.instrs[0] instanceof PhiInstr &&
        block.instrs[0].dests.length == 1 &&
        block.instrs[1] instanceof IfInstr &&
        block.instrs[1].uses[0] === block.instrs[0]
    )
    {
        var phiInstr = block.instrs[0];
        var ifInstr = block.instrs[1];

        var phiPreds = phiInstr.preds.slice(0);
        var phiUses = phiInstr.uses.slice(0);

        // Flag to indicate the CFG was changed
        var changed = false;

        for (var j = 0; j < phiPreds.length; ++j)
        {
            var pred = phiPreds[j];
            var use = phiUses[j];

            if (pred.instrs[pred.instrs.length - 1] instanceof JumpInstr)
            {
                //print('eliminating phi node pred for ' + phiInstr + ' in ' + block.getBlockName());

                // Replace the jump by an if instruction
                pred.replInstrAtIndex(
                    pred.instrs.length - 1,
                    new IfInstr(
                        use,
                        ifInstr.targets[0],
                        ifInstr.targets[1]
                    )
                );

                // For each if target. add an incoming phi value for the
                // predecessor block matching that of the current block
                for (var k = 0; k < ifInstr.targets.length; ++k)
                {
                    var target = ifInstr.targets[k];

                    for (var l = 0; l < target.instrs.length; ++l)
                    {
                        var instr = target.instrs[l];

                        if (!(instr instanceof PhiInstr))
                            break;

                        var inc = instr.getIncoming(block);
                        instr.addIncoming(inc, pred);
                    }
                }

                // Set the changed flag
                changed = true;
            }
        }

        if (changed)
            return true;
    }

    //
    // Aggregate phi nodes used only by other phi nodes
    //

    // If this block contains only a phi node used only by another phi node
    if (
        block.instrs.length == 2 &&
        block.instrs[0] instanceof PhiInstr &&
        block.instrs[0].dests.length == 1 &&
        block.instrs[1] instanceof JumpInstr &&
        block.instrs[0].dests.length == 1 &&
        block.instrs[0].dests[0] instanceof PhiInstr
    )
    {
        var origPhi = block.instrs[0];
        var destPhi = origPhi.dests[0];
        var destBlock = block.succs[0];

        // Flag to indicate the CFG was changed
        var changed = false;

        // For each incoming value of the origin phi
        for (var i = 0; i < origPhi.uses.length; ++i)
        {
            var use = origPhi.uses[i];
            var pred = origPhi.preds[i];

            // If the destination block has this predecessor, skip it
            if (arraySetHas(destBlock.preds, pred))
                continue;

            // Remove the phi and block predecessor-successor links
            origPhi.remPred(pred);
            block.remPred(pred);
            pred.remSucc(block);

            // Make the pred branch to the dest block
            var branch = pred.getLastInstr();
            for (var j = 0; j < branch.targets.length; ++j)
                if (branch.targets[j] === block)
                    branch.targets[j] = destBlock;

            // Add a new incoming value to the dest phi
            destPhi.addIncoming(use, pred);

            // Add the block predecessor-successor links
            destBlock.addPred(pred);
            pred.addSucc(destBlock);

            // Move back one phi predecessor index
            --i;

            // Set the changed flag
            changed = true;
        }

        if (changed)
            return true;
    }

    //
    // Eliminate blocks with no predecessors
    //

    // If this block has no predecessors, and it is not the entry block
    if (
        block.preds.length == 0 &&
        block !== cfg.entry
    )
    {
        //print('eliminating block with no predecessors: ' + block.getBlockName());

        // Remove all instructions in the block
        while (block.instrs.length > 0)
            block.remInstrAtIndex(0);

        // Remove the block from the CFG
        cfg.remBlock(block);

        // The CFG was changed
        return true;
    }

    //
    // Merge blocks with only one destination
    //

    // If this block has only one successor, which has only one predecessor
    // and the block is not terminated by an exception-producing instruction
    if (
        block.succs.length == 1 && 
        block.succs[0].preds.length == 1 &&
        block !== block.succs[0] &&
        !(block.getLastInstr() instanceof ExceptInstr)
    )
    {
        //print('merging block with one succ: ' + block.getBlockName());
        //print('successor: ' + block.succs[0].getBlockName());

        var succ = block.succs[0];

        // For each instruction of the successor
        for (var iIndex = 0; iIndex < succ.instrs.length; ++iIndex)
        {
            var instr = succ.instrs[iIndex];
            
            // If the instruction is a phi node
            if (!(instr instanceof PhiInstr))
                break
 
            // Ensure that this phi node has only one predecessor
            assert (
                instr.preds.length == 1, 
                'phi node in merged block should have one pred:\n' +
                instr
            );

            var phiIn = instr.uses[0];

            // Remove the edge from the use to the phi node
            if (phiIn instanceof IRInstr)
                phiIn.remDest(instr);

            // For each dest of the phi node
            for (var k = 0; k < instr.dests.length; ++k)
            {
                // Replace all uses of the phi by uses of its input
                instr.dests[k].replUse(instr, phiIn);
                if (phiIn instanceof IRInstr)
                    phiIn.addDest(instr.dests[k]);
            }
        }

        // Remove the final branch instruction from the predecessor
        block.remInstrAtIndex(block.instrs.length - 1);

        // For each non-phi instruction of the successor
        for (; iIndex < succ.instrs.length; ++iIndex)
        {
            var instr = succ.instrs[iIndex];

            // Add the instruction to the predecessor
            block.addInstr(instr);
        }

        //print('Number of succ succs: ' + succ.succs.length);

        // For each successor of the successor
        for (var j = 0; j < succ.succs.length; ++j)
        {
            var succSucc = succ.succs[j];

            // For each instruction of the successor's successor
            for (var k = 0; k < succSucc.instrs.length; ++k)
            {
                var instr = succSucc.instrs[k];
                
                // If the instruction is not a phi node, stop
                if (!(instr instanceof PhiInstr))
                    break;

                // Change references to the successor to references
                // to the predecessor instead
                instr.replPred(succ, block);
            }
        }

        // Remove the successor block from the CFG
        cfg.remBlock(succ);

        // The CFG was changed
        return true;
    }

    //
    // Jump over blocks with no instructions and a single successor
    //

    // If this block has only one successor, no instructions but a branch, 
    // and the block is not terminated by an exception-producing instruction
    if (
        block.succs.length == 1 && 
        block.succs[0] !== block &&
        block.instrs.length == 1 &&
        !(block.getLastInstr() instanceof ExceptInstr)
    )
    {
        var succ = block.succs[0];
    
        //print('got block: ' + block.getBlockName());

        // Copy the predecessor list for this block
        var preds = block.preds.slice(0);

        // Flag to indicate the CFG was changed
        var changed = false;

        // For each predecessor
        PRED_LOOP:
        for (var j = 0; j < preds.length; ++j)
        {
            var pred = preds[j];

            //print('got pred: ' + pred.getBlockName());

            // For each instruction of the successor
            for (var k = 0; k < succ.instrs.length; ++k)
            {
                var instr = succ.instrs[k];

                // If this instruction is not a phi node, stop
                if (!(instr instanceof PhiInstr))
                    break;                        

                // If the phi node already has this predecessor, skip the predecessor
                if (arraySetHas(instr.preds, pred))
                    continue PRED_LOOP;
            }

            //print('simplifying no instructions, single successor: ' + block.getBlockName());

            // Remove the predecessor from our predecessor list
            block.remPred(pred);

            // Get the predecessor's branch instruction
            var branchInstr = pred.getLastInstr();

            // Replace the predecessor's branch target by the successor
            for (var k = 0; k < branchInstr.targets.length; ++k)
                if (branchInstr.targets[k] === block)
                    branchInstr.targets[k] = succ;

            // Replace the predecessor's successor block by the successor
            pred.remSucc(block);
            pred.addSucc(succ);

            // Add the predecessor as a predecessor of the successor
            succ.addPred(pred);

            // For each instruction of the successor
            for (var k = 0; k < succ.instrs.length; ++k)
            {
                var instr = succ.instrs[k];

                // If this instruction is not a phi node, stop
                if (!(instr instanceof PhiInstr))
                    break;

                // Add an incoming value for the predecessor
                var inVal = instr.getIncoming(block);
                instr.addIncoming(inVal, pred);
            }

            // Set the changed flag
            changed = true;
        }

        if (changed)
            return true;
    }

    // If no block-level patterns applied
    else
    {
        // Flag to indicate the CFG was changed
        var changed = false;

        // For each instruction of the block
        for (var i = 0; i < block.instrs.length; ++i)
        {
            var instr = block.instrs[i];

            // Apply instruction-level patterns to this instruction
            var result = applyPatternsInstr(cfg, block, instr, i);

            if (result)
                changed = true;
        }

        if (changed)
            return true;
    }

    // The CFG was not changed
    return false;
}

/**
Apply instruction-level optimization patterns to an instruction
*/
function applyPatternsInstr(cfg, block, instr, index)
{
    // If the instruction's value is not used and the instruction
    // has no side effects and is not a branch
    if (
        instr.dests.length == 0 &&
        instr.writesMem() == false &&
        instr.isBranch() == false
    )
    {
        //print('Removing dead: ' + instr);

        // Remove the instruction
        block.remInstrAtIndex(index);

        // A change was made
        return true;
    }
     
    // If the instruction is a phi node
    if (instr instanceof PhiInstr)
    {
        // Delete all phi-assignments of the form:
        // Vi <- phi(...Vi...Vi...)
        //
        // If a phi-assignment has the form:
        // Vi <- phi(...Vi...Vj...Vi...Vj...)
        // 0 or more Vi and 1 or more Vj
        //
        // Then delete the assignment and rename
        // all occurences of Vi to Vj

        var numVi = 0;
        var numVj = 0;
        var Vj = null;

        // Count the kinds of uses of the phi node
        for (var j = 0; j < instr.uses.length; ++j)
        {
            var use = instr.uses[j];

            if (use === instr)
            {
                numVi++;
            }
            else if (use === Vj || Vj === null)
            {
                numVj++;
                Vj = use;
            }
        }

        // If this phi node has the form:
        // Vi <- phi(...Vi...Vi...)
        if (numVi == instr.uses.length)
        {
            //print('Removing phi: ' + instr);

            // Remove the phi node
            block.remInstr(instr);

            // A change was made
            return true;
        }
        
        // If this phi-assignment has the form:
        // Vi <- phi(...Vi...Vj...Vi...Vj...)
        // 0 or more Vi and 1 or more Vj
        else if (numVi + numVj == instr.uses.length)        
        {
            //print('Renaming phi: ' + instr);

            // Rename all occurences of Vi to Vj
            for (var k = 0; k < instr.dests.length; ++k)
            {
                var dest = instr.dests[k];
                dest.replUse(instr, Vj);
                if (Vj instanceof IRInstr)
                    Vj.addDest(dest);
            }
            if (Vj instanceof IRInstr)
                Vj.remDest(instr);

            // Remove the phi node
            block.remInstr(instr);

            // A change was made
            return true;
        }
    }

    //
    // Strength reduction patterns
    //

    // If this is a multiplication
    if (instr instanceof MulInstr)
    {
        // If the left operand is a power of 2
        if (instr.uses[1] instanceof ConstValue &&
            instr.uses[1].isInt() &&
            isPowerOf2(instr.uses[1].value))
        {
            // Replace the multiplication by a left shift
            block.replInstrAtIndex(
                index,
                new LsftInstr(
                    instr.uses[0],
                    ConstValue.getConst(
                        highestBit(instr.uses[1].value),
                        instr.type
                    )
                )
            );

            // A change was made
            return true;
        }

        // If the right operand is a power of 2
        else if (instr.uses[0] instanceof ConstValue &&
                 instr.uses[0].isInt() &&
                 isPowerOf2(instr.uses[0].value))
        {
            // Replace the multiplication by a left shift
            block.replInstrAtIndex(
                index,
                new LsftInstr(
                    instr.uses[1],
                    ConstValue.getConst(
                        highestBit(instr.uses[0].value),
                        instr.type
                    )
                )
            );

            // A change was made
            return true;
        }
    }

    // If this is a multiplication with overflow handling
    if (instr instanceof MulOvfInstr)
    {
        // If the left operand is a power of 2
        if (instr.uses[1] instanceof ConstValue &&
            instr.uses[1].isInt() &&
            isPowerOf2(instr.uses[1].value))
        {
            // Replace the multiplication by a left shift
            block.replBranch(
                new LsftOvfInstr(
                    instr.uses[0],
                    ConstValue.getConst(
                        highestBit(instr.uses[1].value),
                        instr.type
                    ),
                    instr.targets[0],
                    instr.targets[1]
                )
            );

            // A change was made
            return true;
        }

        // If the right operand is a power of 2
        else if (instr.uses[0] instanceof ConstValue &&
                 instr.uses[0].isInt() &&
                 isPowerOf2(instr.uses[0].value))
        {
            // Replace the multiplication by a left shift
            block.replBranch(
                new LsftOvfInstr(
                    instr.uses[1],
                    ConstValue.getConst(
                        highestBit(instr.uses[0].value),
                        instr.type
                    ),
                    instr.targets[0],
                    instr.targets[1]
                )
            );

            // A change was made
            return true;
        }
    }

    // If this is a division by a power of 2
    if (instr instanceof DivInstr && 
        instr.uses[1] instanceof ConstValue &&
        instr.uses[1].isInt() &&
        isPowerOf2(instr.uses[1].value))
    {
        // Replace the division by a right shift
        var shiftInstr = instr.type.isUnsigned()? UrsftInstr:RsftInstr;
        block.replInstrAtIndex(
            index,
            new shiftInstr(
                instr.uses[0],
                ConstValue.getConst(
                    highestBit(instr.uses[1].value),
                    instr.type
                )
            )
        );

        // A change was made
        return true;
    }

    // If this is a modulo of a power of 2
    if (instr instanceof ModInstr && 
        instr.uses[1] instanceof ConstValue &&
        instr.uses[1].isInt() &&
        isPowerOf2(instr.uses[1].value))
    {
        // Replace the modulo by a bitwise AND instruction
        block.replInstrAtIndex(
            index,
            new AndInstr(
                instr.uses[0],
                ConstValue.getConst(
                    instr.uses[1].value - 1,
                    instr.type
                )
            )
        );

        // A change was made
        return true;
    }

    // No changes were made
    return false;
}

