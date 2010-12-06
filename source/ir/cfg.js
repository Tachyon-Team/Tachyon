/**
@fileOverview
Implementation of control-flow graphs and basic blocks

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010 Maxime Chevalier-Boisvert, All Rights Reserved
*/

/**
@class Class to represent control-flow graph
*/
function ControlFlowGraph(ownerFunc)
{
    /**
    Basic blocks contained in this CFG
    */
    this.blocks = [];

    /**
    List of free instruction ids
    @field
    */
    this.freeInstrIds = [];

    /**
    Next instruction id to allocate
    @field
    */
    this.nextInstrId = 1;

    /**
    List of free basic block ids
    @field
    */
    this.freeBlockIds = [];

    /**
    Next block id to allocate
    @field
    */
    this.nextBlockId = 1;

    /**
    Instruction output names used in the CFG
    @field
    */
    this.instrNames = [];

    /**
    Block names used in the CFG
    @field
    */
    this.blockNames = [];

    /**
    IR function to which this CFG belongs
    @field
    */
    this.ownerFunc = ownerFunc;

    /**
    Entry basic block
    @field
    */
    this.entry = null;
}
ControlFlowGraph.prototype = {};

/**
Construct a string representation
*/
ControlFlowGraph.prototype.toString = function (blockOrderFn, outFormatFn, inFormatFn, lnPfxFormatFn)
{
    // Function to order blocks according to a depth-first traversal
    function DFSOrder(blocks)
    {
        var unvisited = blocks.slice(0);
        var stack = [blocks[0]];
        var order = [];

        while (stack.length > 0)
        {
            var b = stack.pop();

            if (arraySetHas(order, b))
                continue;

            order.push(b);
            arraySetRem(unvisited, b);

            for (var i = b.succs.length - 1; i >= 0; --i)
                stack.push(b.succs[i]);
        }

        order = order.concat(unvisited);

        return order;
    }

    // If no ordering function is supplied, use the depth-first order
    if (!blockOrderFn)
        blockOrderFn = DFSOrder;

    if (!lnPfxFormatFn)
        lnPfxFormatFn = function () { return ""; };

    // Order the blocks according to the supplied ordering function
    var blockList = blockOrderFn(this.blocks);

    var output = "";

    for (var i = 0; i < blockList.length; ++i)
    {
        var block = blockList[i];

        output += block.toString(outFormatFn, inFormatFn, lnPfxFormatFn);

        if (i !== blockList.length - 1)
            output += "\n\n";
    }

    return output;
};

/**
Make a deep-copy of the control-flow graph
*/
ControlFlowGraph.prototype.copy = function ()
{
    // Create a new control flow graph
    newCFG = new ControlFlowGraph(this.ownerFunc);

    // Copy information about free block/instruction names and ids
    newCFG.freeInstrIds = this.freeInstrIds.slice(0);
    newCFG.nextInstrId  = this.nextInstrId;
    newCFG.freeBlockIds = this.freeBlockIds.slice(0);
    newCFG.nextBlockId  = this.nextBlockId;
    newCFG.instrNames   = this.instrNames.slice(0);
    newCFG.blockNames   = this.blockNames.slice(0);

    // Create a map from old blocks to new blocks
    blockMap = [];

    // Create a map from old instruction ids to new instructions
    instrMap = [];

    // Remove the entry block from the new CFG
    newCFG.blocks = [];

    // For each basic block
    for (var i = 0; i < this.blocks.length; ++i)
    {
        var block = this.blocks[i];

        // Create a copy of the block
        var newBlock = block.copy(newCFG);

        // If this block is the entry, store the new CFG's entry
        if (block === this.entry)
            newCFG.entry = newBlock;

        // Add the new block to the new CFG
        newCFG.blocks.push(newBlock);

        // Add the new block to the block map
        blockMap[block.blockId] = newBlock;

        // For each instruction in the new basic block
        for (var j = 0; j < newBlock.instrs.length; ++j)
        {
            // Add the instruction copy to the instruction map
            var instr = block.instrs[j];
            var newInstr = newBlock.instrs[j];
            instrMap[instr.instrId] = newInstr;

            // Make a shallow copy of the instruction's dests
            newInstr.dests = instr.dests.slice(0);
        }
    }

    // For each new basic block
    for (var i = 0; i < newCFG.blocks.length; ++i)
    {
        // Create a copy and add it to the block map
        var block = newCFG.blocks[i];

        // Remap the block's predecessor and successors to new blocks
        for (var j = 0; j < block.preds.length; ++j)
            block.preds[j] = blockMap[block.preds[j].blockId];
        for (var j = 0; j < block.succs.length; ++j)
            block.succs[j] = blockMap[block.succs[j].blockId];

        // For each new instruction in the new basic block
        for (var j = 0; j < block.instrs.length; ++j)
        {
            // Create a copy and add it to the instruction map
            var instr = block.instrs[j];

            // Remap the uses
            for (var k = 0; k < instr.uses.length; ++k)
            {
                var use = instr.uses[k];
                if (use instanceof IRInstr)
                    instr.uses[k] = instrMap[use.instrId];                    
            }

            // Remap the dests
            for (var k = 0; k < instr.dests.length; ++k)
            {
                var dest = instr.dests[k];
                instr.dests[k] = instrMap[dest.instrId];
            }

            // If this is a phi instruction
            if (instr instanceof PhiInstr)
            {
                // Remap the predecessor blocks
                for (var k = 0; k < instr.preds.length; ++k)
                {
                    var pred = instr.preds[k];
                    instr.preds[k] = blockMap[pred.blockId];
                }
            }

            // Otherwise, if this is a branch instruction
            else if (instr.isBranch())
            {
                // Remap the target blocks
                for (var k = 0; k < instr.targets.length; ++k)
                {
                    var target = instr.targets[k];
                    instr.targets[k] = blockMap[target.blockId];
                }
            }
        }
    }

    // Return the new CFG
    return newCFG;
};

/**
Assign a free id number to an instruction
*/
ControlFlowGraph.prototype.assignInstrId = function (instr)
{
    assert (instr instanceof IRInstr);

    if (this.freeInstrIds.length > 0)
        instr.instrId = this.freeInstrIds.pop();
    else
        instr.instrId = this.nextInstrId++;
};

/**
Free an instruction id number
*/
ControlFlowGraph.prototype.freeInstrId = function (instr)
{
    assert (instr instanceof IRInstr);

    this.freeInstrIds.push(instr.instrId);
};

/**
Assign a free id number to a basic block
*/
ControlFlowGraph.prototype.assignBlockId = function (block)
{
    assert (block instanceof BasicBlock);

    if (this.freeBlockIds.length > 0)
        block.blockId = this.freeBlockIds.pop();
    else
        block.blockId = this.nextBlockId++;
};

/**
Free a block id number
*/
ControlFlowGraph.prototype.freeBlockId = function (block)
{
    assert (block instanceof BasicBlock);

    this.freeBlockIds.push(block.blockId);
};

/**
Assign a free output name to an instruction
*/
ControlFlowGraph.prototype.assignInstrName = function (instr, outName)
{
    assert (instr instanceof IRInstr);

    if (outName == undefined || outName == '')
    {
        instr.outName = '';
        return;
    }

    if (!arraySetHas(this.instrNames, outName))
    {
        instr.outName = outName;
    }
    else
    {
        var idx = 1;

        while (arraySetHas(this.instrNames, outName + '_' + idx))
            idx++;

        instr.outName = outName + '_' + idx;
    }

    arraySetAdd(this.instrNames, instr.outName);
};

/**
Free an instruction output name
*/
ControlFlowGraph.prototype.freeInstrName = function (instr)
{
    assert (instr instanceof IRInstr);

    arraySetRem(this.instrNames, instr.outName);
};

/**
Assign a free label name to a block
*/
ControlFlowGraph.prototype.assignBlockName = function (block, labelName)
{
    assert (block instanceof BasicBlock);

    if (labelName == undefined || labelName == '')
    {
        block.label = '';
        return;
    }

    if (!arraySetHas(this.blockNames, labelName))
    {
        block.label = labelName;
    }
    else
    {
        var idx = 1;

        while (arraySetHas(this.blockNames, labelName + '_' + idx))
            idx++;

        block.label = labelName + '_' + idx;
    }

    arraySetAdd(this.blockNames, block.label);
};

/**
Free a block label name
*/
ControlFlowGraph.prototype.freeBlockName = function (block)
{
    assert (block instanceof BasicBlock);

    arraySetRem(this.blockNames, block.label);
};

/**
Create a block in this CFG
*/
ControlFlowGraph.prototype.getNewBlock = function (label)
{
    var block = new BasicBlock(this);

    this.blocks.push(block);

    // Assign an id number to this block
    this.assignBlockId(block);

    // Assign a name to the block
    this.assignBlockName(block, label);

    return block;
};

/**
Get the entry block for this CFG
*/
ControlFlowGraph.prototype.getEntryBlock = function ()
{
    // If there is no entry block yet, create one
    if (!this.entry)
        this.entry = this.getNewBlock('entry');

    return this.entry;
};

/**
Add a basic block to the CFG
*/
ControlFlowGraph.prototype.addBlock = function (block)
{
    // Assign a name and id to each instruction of the block
    for (var i = 0; i < block.instrs.length; ++i)
    {
        var instr = block.instrs[i];
        this.assignInstrId(instr);
        this.assignInstrName(instr, instr.outName);
    }

    // Set the block's parent CFG
    block.parentCFG = this;

    // Add the block to our block list
    this.blocks.push(block);

    // Assign an id number to this block
    this.assignBlockId(block);

    // Assign a name to the block
    this.assignBlockName(block, block.getBlockName());
};

/**
Remove a basic block from this CFG
*/
ControlFlowGraph.prototype.remBlock = function (block)
{
    //print('Removing block: ' + block.label);
    //print('num succs: ' + block.succs.length);

    // Remove this block from the successors of our predecessors
    for (var i = 0; i < block.preds.length; ++i)
        block.preds[i].remSucc(this);
    
    // Remove this block from the predecessors of our successors
    for (var i = 0; i < block.succs.length; ++i)
        block.succs[i].remPred(block);

    // For each successor of the block
    for (var i = 0; i < block.succs.length; ++i)
    {
        var succ = block.succs[i];

        //print('succ: ' + succ.getBlockName());

        // For each instruction of the successor
        for (var j = 0; j < succ.instrs.length; ++j)
        {
            var instr = succ.instrs[j];
            
            // If the instruction is not a phi node, stop
            if (!(instr instanceof PhiInstr))
                break;

            // For each predecessor of the phi node
            for (var k = 0; k < instr.preds.length; ++k)
            {
                // If this is a reference to the block
                if (instr.preds[k] === block)
                {
                    // Get the corresponding use
                    var use = instr.uses[k];

                    // Remove this value
                    instr.preds.splice(k, 1);
                    instr.uses.splice(k, 1);

                    // If the value is no longer used, remove the dest link
                    if (!arraySetHas(instr.uses, use) && use instanceof IRInstr)
                        use.remDest(instr);                    

                    // Move back to the previous index
                    k--;
                }
            }
        }
    }
    
    // Remove the block from the list
    arraySetRem(this.blocks, block);

    // Free this block's id number
    this.freeBlockId(block);

    // Free this block's label name
    this.freeBlockName(block);
};

/**
Validate that this CFG is properly formed
*/
ControlFlowGraph.prototype.validate = function ()
{
    //
    // Verify that the CFG is properly formed
    //

    // For each block in the CFG
    for (var i = 0; i < this.blocks.length; ++i)
    {
        var block = this.blocks[i];

        // Verify that the block has this CFG as its parent
        if (block.parentCFG !== this)
            error('parent CFG link broken');

        // For each predecessor
        for (var j = 0; j < block.preds.length; ++j)
        {
            var pred = block.preds[j];
            
            // Verify that the predecessor is a basic block
            if (!(pred instanceof BasicBlock))
                error('predecessor is not valid basic block for:\n' + block);

            // Verify that our predecessors have us as a successor
            if (!arraySetHas(block.preds[j].succs, block))
                error(
                    'predecessor:\n' +  block.preds[j] + 
                    '\nmissing successor link to:\n' + block
                );
        }

        // For each successor
        for (var j = 0; j < block.succs.length; ++j)
        {
            var succ = block.succs[j];

            // Verify that the successor is a basic block
            if (!(succ instanceof BasicBlock))
                error(
                    'successor is not valid basic block for:\n' + block
                );

            // Verify that our successors have us as a predecessor
            if (!arraySetHas(block.succs[j].preds, block))
                error(
                    'successor missing predecessor link to:\n' + block
                );
        }

        // Get a reference to the last instruction in the block
        var lastInstr = block.instrs[block.instrs.length - 1];

        // Verify that the block is terminated with a branch instruction
        if (!lastInstr || !(lastInstr.isBranch()))
            error(
                'block does not terminate in a branch:\n' + block
            );

        // Verify that the branch targets match our successor set
        for (var j = 0; j < block.succs.length; ++j)
        {
            if (!arraySetHas(lastInstr.targets, block.succs[j]))
                error(
                    'successors do not match branch targets for:\n' + block
                );
        }
        for (var j = 0; j < lastInstr.targets.length; ++j)
        {
            if (!arraySetHas(block.succs, lastInstr.targets[j]))
                error(
                    'successors do not match branch targets for:\n' + block
                );
        }

        // For each instruction in the block
        for (var j = 0; j < block.instrs.length; ++j)
        {
            var instr = block.instrs[j];

            // Verify that the instruction has this block as its parent
            if (instr.parentBlock !== block)
                error('parent block link broken:\n' + instr);

            // If this is a phi instruction
            if (instr instanceof PhiInstr)
            {
                // Verify that it appears at the start of the block
                if (j != 0 && !(block.instrs[j-1] instanceof PhiInstr))
                   error('phi node after non-phi instruction');

                // Verify that each immediate predecessor has a corresponding use
                for (var k = 0; k < block.preds.length; ++k)
                    if (!arraySetHas(instr.preds, block.preds[k]))
                        error(
                            'phi node:\n' + instr + '\ndoes not cover ' +
                            'immediate predecessor:\n' + 
                            block.preds[k].getBlockName()
                        );

                // Verify that there is exactly one predecessor for each use
                if (instr.preds.length != instr.uses.length)
                    error(
                        'phi node does not have one predecessor for each use'
                    );

                // Verify that there are no more phi uses than block predecessors
                if (instr.preds.length != block.preds.length)
                    error(
                        'phi node:\n' + instr + '\nin:\n' +
                        block.getBlockName() +
                        '\nhas more uses than predecessors'
                    );
            }

            // Verify that no branches appear before the last instruction
            if (instr.isBranch() && j != block.instrs.length - 1)
                error('branch before last block instruction');

            // For each use of this instruction
            for (var k = 0; k < instr.uses.length; ++k)
            {
                var use = instr.uses[k];

                // Verify that the use is valid
                if (!(use instanceof IRValue))
                    error('invalid use found');

                // Verify that the use is in this CFG
                if (use instanceof IRInstr && use.parentBlock.parentCFG != this)
                    error('use not in CFG');

                // Verify that our uses have us as a dest
                if (use instanceof IRInstr)
                    if (!arraySetHas(instr.uses[k].dests, instr))
                        error(
                            'missing dest link, from:\n' + 
                            instr.uses[k] + 
                            '\nto:\n' +
                            instr
                        );
            }

            // For each dest of this instruction
            for (var k = 0; k < instr.dests.length; ++k)
            {
                var dest = instr.dests[k];

                // Verify that the dest is valid
                if (!(dest instanceof IRValue))
                    error('invalid dest found');

                // Verify that the dest is in this CFG
                if (dest.parentBlock.parentCFG != this)
                    error('dest not in CFG');

                // Verify that our dests have us as a use
                if (!arraySetHas(dest.uses, instr))
                    error(
                        'missing use link, from:\n' + 
                        instr.dests[k] +
                        '\nto:\n' + 
                        instr
                    );
            }
        }
    }

    //
    // Verify proper use of phi nodes and instruction values
    //

    // Work list for the analysis
    var workList = [this.entry];

    // Array to store must reach sets for each block
    var mustReachOut = [];

    // Compute the set of all definitions in the CFG
    var fullReachSet = [];
    for (var i = 0; i < this.blocks.length; ++i)
    {
        var block = this.blocks[i];
        for (var j = 0; j < block.instrs.length; ++j)
            fullReachSet.push(block.instrs[j]);
    }

    // Initialize the reaching def sets for all blocks
    for (var i = 0; i < this.blocks.length; ++i)
    {
        var block = this.blocks[i];
        mustReachOut[block.blockId] = fullReachSet;
    }

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
        for (var i = 0; i < block.instrs.length; ++i)
        {
            // Add the instruction to the set of reaching values
            var instr = block.instrs[i];
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

    // For each basic block
    for (var i = 0; i < this.blocks.length; ++i)
    {
        var block = this.blocks[i];

        // Compute the must and may reach sets at this block's entry
        var mustReachCur = (block.preds.length > 0)? fullReachSet.slice(0):[];
        for (var j = 0; j < block.preds.length; ++j)
        {
            var pred = block.preds[j];
            mustReachCur = arraySetIntr(mustReachCur, mustReachOut[pred.blockId]);
        }

        // For each instruction
        for (var j = 0; j < block.instrs.length; ++j)
        {
            var instr = block.instrs[j];

            // For each use of the instruction
            for (var k = 0; k < instr.uses.length; ++k)
            {
                var use = instr.uses[k];

                // If the use isn't in the CFG, ignore it
                if (!(use instanceof IRInstr))
                    continue;

                if (instr instanceof PhiInstr)
                {
                    var phiPred = instr.preds[k];
                    if (!arraySetHas(mustReachOut[phiPred.blockId], use))
                        error(
                            'phi node:\n' + instr +
                            '\nuses non-reaching value:\n' + use
                        );
                }
                else
                {
                    if (!arraySetHas(mustReachCur, use))
                        error(
                            'instruction:\n' + instr + '\nin block:\n' + 
                            instr.parentBlock.getBlockName() +
                            '\nuses non-reaching value:\n' + use
                        );
                }
            }

            // Add the instruction to the must reach set
            arraySetAdd(mustReachCur, instr);
        }
    }

    // The CFG is valid
    return true;
};

/** 
    Returns a block iterator. Depending on the given type, the order of
    visited blocks might have certain properties.

    basic: blocks might be returned in any order
    strict: Predecessor blocks always appear before their successors and
            blocks part of a loop are contiguous

    @param {String} type 
 */ 
ControlFlowGraph.prototype.getBlockItr = function (type)
{
    if (type === undefined)
    {
        type = "basic";
    }

    if (type === "basic")
    {
        return new ArrayIterator(this.blocks);
    } 
    else if (type === "strict")
    {
        // TODO: Migrate allocator.orderBlocks here
        error("strict mode unimplemented");

        // TODO: When asked for a strict iterator, try to update the previous
        //       calculated order instead of computing it from scratch.
    } 
    else 
    {
        error("unrecognized option: '" + type + "'");
    }
};

/** 
Returns an instruction iterator. Depending on the given type, the order of
visited instructions might have certain properties.
@class Instruction iterator

basic: instructions might be returned in any order
strict: instructions part of predecessor blocks always appear before 
        instructions part of their successors and
        instructions part of a loop are always contiguous

@param {String} type 
@augments Iterator
*/ 
ControlFlowGraph.prototype.getInstrItr = function (type)
{
    var it = Object.create(this.getInstrItr.prototype);
    it.blockIt = this.getBlockItr(type);
    it.instrIt = it.blockIt.get().getInstrItr();
    return it;
};

ControlFlowGraph.prototype.getInstrItr.prototype = new Iterator();

/** 
Ensure iterator is still on a valid item.  Ex: Not at the end 
*/
ControlFlowGraph.prototype.getInstrItr.prototype.valid = function ()
{
    return this.blockIt.valid() && this.instrIt.valid();
};

/**
Move the iterator to the next item
*/
ControlFlowGraph.prototype.getInstrItr.prototype.next = function ()
{
    this.instrIt.next();
    while (!this.instrIt.valid())
    {
        this.blockIt.next();
        if (this.blockIt.valid())
        {
            this.instrIt = this.blockIt.get().getInstrItr();
        } 
        else
        {
            break;
        }
    }
};

/**
Returns the current item being visited
*/
ControlFlowGraph.prototype.getInstrItr.prototype.get = function ()
{
    return this.instrIt.get();
};

/**
Returns an edge iterator.  Edges might be returned in any order.
@class Edge Iterator
@augments Iterator
*/
ControlFlowGraph.prototype.getEdgeItr = function ()
{
    var it = Object.create(this.getEdgeItr.prototype); 
    it.predIt = this.getBlockItr();
    if (it.predIt.valid())
    {
        it.succIt = new ArrayIterator(it.predIt.get().succs);
    }
    else
    {
        it.succIt = null;
    }
    return it;
};

ControlFlowGraph.prototype.getEdgeItr.prototype = new Iterator();

/** 
Ensure iterator is still on a valid item.  Ex: Not at the end 
*/
ControlFlowGraph.prototype.getEdgeItr.prototype.valid = function ()
{
    return this.predIt.valid() && this.succIt.valid(); 
};

/**
Move the iterator to the next item
*/
ControlFlowGraph.prototype.getEdgeItr.prototype.next = function ()
{
    this.succIt.next();

    while (!this.succIt.valid())
    {
        this.predIt.next();
        if (this.predIt.valid())
        {
            this.succIt = new ArrayIterator(this.predIt.get().succs);
        }
        else
        {
            break;
        }
    }
};

/**
Returns the current item being visited
*/
ControlFlowGraph.prototype.getEdgeItr.prototype.get = function ()
{
    return {pred:this.predIt.get(), succ:this.succIt.get()};
};

/**
Add an instruction before the iterator position
*/
ControlFlowGraph.prototype.addInstr = function (instrItr, newInstr)
{
    var block = instrItr.get().parentBlock;
    var index = instrItr.instrIt.getIndex();

    block.addInstr(newInstr, undefined, index);
}

/**
Remove an instruction from the CFG
*/
ControlFlowGraph.prototype.remInstr = function (instrItr)
{
    var block = instrItr.get().parentBlock;
    var index = instrItr.instrIt.getIndex();

    block.remInstrAtIndex(index);
}

/**
Replace an instruction by another
*/
ControlFlowGraph.prototype.replInstr = function (instrItr, newInstr)
{
    var block = instrItr.get().parentBlock;
    var index = instrItr.instrIt.getIndex();

    block.replInstrAtIndex(index, newInstr);
}

/**
Insert a new block along the edge in between two blocks.
*/
ControlFlowGraph.prototype.insertOnEdge = function (edgeItr, block)
{
    var edge = edgeItr.get();

    this.insertBetween(edge.pred, edge.succ, block);
}

/**
Insert a new block in between two blocks.
*/
ControlFlowGraph.prototype.insertBetween = function (pred, succ, block)
{
    // Unlink the predecessor and successor
    succ.remPred(pred);
    pred.remSucc(succ);

    // Link the predecessor and the new block
    pred.addSucc(block);
    block.addPred(pred);

    // Make the new block jump to the successor
    block.addInstr(new JumpInstr(succ));

    // Ensure that the predecessor has a branch instruction
    assert (
        pred.hasBranch(),
        'predecessor has no branch instruction'
    );

    // Get the branch instruction of the predecessor
    var branch = pred.getLastInstr();

    // Update the branch instruction targets
    for (var i = 0; i < branch.targets.length; ++i)
    {
        if (branch.targets[i] === succ)
            branch.targets[i] = block;
    }

    // Update the phi node predecessors
    for (var i = 0; i < succ.instrs.length; ++i)
    {
        var instr = succ.instrs[i];

        if (!(instr instanceof PhiInstr))
            continue;

        instr.replPred(pred, block);
    }
}

/**
@class Class to represent a basic block
*/
function BasicBlock(cfg)
{
    /**
    Label name string for this basic block
    @field
    */
    this.label = '';

    /**
    Id number for this basic block
    @field
    */
    this.blockId = 0;

    /**
    List of IR instructions
    @field
    */
    this.instrs = [];

    /**
    Blocks that have edges going to this block
    @field
    */
    this.preds = [];

    /**
    Blocks that we have edges going to
    @field
    */
    this.succs = [];

    /**
    Parent control flow graph
    @field
    */
    this.parentCFG = cfg;
}
BasicBlock.prototype = {};

/**
Produce a string representation
*/
BasicBlock.prototype.toString = function (outFormatFn, inFormatFn, lnPfxFormatFn)
{
    if (!lnPfxFormatFn)
        lnPfxFormatFn = function() { return ''; }

    var output = lnPfxFormatFn(this) + this.getBlockName() + ':\n';

    for (var i = 0; i < this.instrs.length; ++i)
    {
        var instr = this.instrs[i];

        output += lnPfxFormatFn(instr) + instr.toString(outFormatFn, inFormatFn) + ';';

        if (instr !== this.instrs[this.instrs.length - 1])
            output += '\n';
    }

    /*
    // Print predecessors
    output += '\npreds: ';
    for (var i = 0; i < this.preds.length; ++i)
        output += this.preds[i].getBlockName() + ((i != this.preds.length - 1)? ', ':'');

    // Print successors
    output += '\nsuccs: ';
    for (var i = 0; i < this.succs.length; ++i)
        output += this.succs[i].getBlockName() + ((i != this.succs.length - 1)? ', ':'');
    */

    return output;
};

/**
Get the name for this basic block
*/
BasicBlock.prototype.getBlockName = function ()
{
    // If the label for this block is set
    if (this.label)
    {
        // Return the label
        return this.label;
    }
    else
    {
        // Return a name based on the block id number
        return 'block_' + this.blockId;
    }
};

/**
Make a copy of the basic block
*/
BasicBlock.prototype.copy = function (cfg)
{
    // Create a new basic block
    newBlock = new BasicBlock(cfg, this.label);

    // Copy the block id
    newBlock.blockId = this.blockId;

    // Copy the block label
    newBlock.label = this.label;
    
    // Set the parent block for the new instructions
    for (var i = 0; i < this.instrs.length; ++i)
    {
        // Make a shallow copy of the instruction
        var instr = this.instrs[i];
        var newInstr = instr.copy();
        newBlock.instrs.push(newInstr);

        // Set the parent link for the new instruction
        newInstr.parentBlock = newBlock;
    }

    // Make shallow copies of the predecessor and successor lists
    newBlock.preds = this.preds.slice(0);
    newBlock.succs = this.succs.slice(0);

    // Set the parent CFG link
    newBlock.parentCFG = cfg;

    // Return the new basic block
    return newBlock;
};

/**
Insert an instruction into the block
@param instr instruction to insert
@param outName output name of the instruction
@param index before which to insert the new instruction
*/
BasicBlock.prototype.addInstr = function (instr, outName, index)
{
    // Ensure that the instruction is valid
    assert (
        instr instanceof IRInstr,
        'cannot add non-instruction to basic block'
    );

    // If the index is undefined, insert at the end of the block
    if (index == undefined)
        index = this.instrs.length;

    // Ensure that the index is valid
    assert (
        index <= this.instrs.length,
        'invalid instruction insertion index'
    );

    // For all uses
    for (var i = 0; i < instr.uses.length; ++i)
    {
        var use = instr.uses[i];

        // If this use is an instruction, add a reverse dest edge
        if (use instanceof IRInstr)
            use.addDest(instr);
    }

    // If this is a branch instruction
    if (instr.isBranch())
    {
        // For each possible destination of this instruction
        for (var i = 0; i < instr.targets.length; ++i)
        {
            var target = instr.targets[i];

            // Add an edge to the potential target block
            this.addSucc(target);

            // Add an incoming edge to the potential target block
            target.addPred(this);
        }
    }

    // Set the parent block for the instruction
    instr.parentBlock = this;

    // Insert the instruction before the index
    this.instrs.splice(index, 0, instr);

    // Assign an id number to the instruction
    this.parentCFG.assignInstrId(instr);

    // Assign a free name to the instruction
    this.parentCFG.assignInstrName(instr, outName);

    return instr;
};

/**
Remove an instruction from this basic block by reference
*/
BasicBlock.prototype.remInstr = function (instr)
{
    for (var i = 0; i < this.instrs.length; ++i)
        if (this.instrs[i] === instr)
            return this.remInstrAtIndex(i);

    assert (false, 'Instruction not found in basic block');
};

/**
Remove an instruction from this basic block by index
*/
BasicBlock.prototype.remInstrAtIndex = function (index)
{
    //print('Removing instr: ' + this.instrs[index]);

    // Get a reference to the instruction
    var instr = this.instrs[index]

    // Remove the instruction from the list
    this.instrs.splice(index, 1);

    // Remove reverse edges from all uses to this instruction
    for (var i = 0; i < instr.uses.length; ++i)
        if (instr.uses[i] instanceof IRInstr)
            instr.uses[i].remDest(instr);

    // If this is a branch instruction
    if (instr.isBranch())
    {
        // For each possible destination of this instruction
        for (var i = 0; i < instr.targets.length; ++i)
        {
            var target = instr.targets[i];

            // Remove the edge to the potential target block
            this.remSucc(target);

            // Remove the incoming edge to the potential target block
            target.remPred(this);

            // For each instruction of the target
            for (var j = 0; j < target.instrs.length; ++j)
            {
                var tinstr = target.instrs[j];
                
                // If the instruction is not a phi node, stop
                if (!(tinstr instanceof PhiInstr))
                    break;

                // Remove the phi predecessor
                tinstr.remPred(this);
            }
        }
    }

    // Free this instruction's id number
    this.parentCFG.freeInstrId(instr);

    // Free this instruction's output name
    this.parentCFG.freeInstrName(instr);
};

/**
Replace an instruction from this basic block by index
*/
BasicBlock.prototype.replInstrAtIndex = function (index, newVal)
{
    assert (
        index < this.instrs.length,
        'invalid instruction index'
    );

    assert (
        newVal instanceof IRValue,
        'invalid replacement value'
    );

    // Get a reference to the old instruction
    var oldInstr = this.instrs[index];

    // Remove reverse edges from all uses to this instruction
    for (var i = 0; i < oldInstr.uses.length; ++i)
        if (oldInstr.uses[i] instanceof IRInstr)
            oldInstr.uses[i].remDest(oldInstr);

    // Replace uses of the old instruction
    for (var i = 0; i < oldInstr.dests.length; ++i)
    {
        var dest = oldInstr.dests[i];

        dest.replUse(oldInstr, newVal);

        if (newVal instanceof IRInstr)
            newVal.addDest(dest);
    }

    // If the new value is an instruction
    if (newVal instanceof IRInstr)
    {
        // Replace the instruction in the list
        this.instrs[index] = newVal;

        // For all uses
        for (var i = 0; i < newVal.uses.length; ++i)
        {
            var use = newVal.uses[i];

            // If this use is an instruction, add a reverse dest edge
            if (use instanceof IRInstr)
                use.addDest(newVal);
        }

        // Ensure that a branch is not replaced by a non-branch or vice-versa
        assert (
            oldInstr.isBranch() == newVal.isBranch(),
            'branches must be replaced by branches'
        );

        // If this is a branch instruction
        if (oldInstr.isBranch())
        {
            // For each possible target of the old instruction
            for (var i = 0; i < oldInstr.targets.length; ++i)
            {
                var target = oldInstr.targets[i];

                // If the new branch does not have this target
                if (!arraySetHas(newVal.targets, target))
                {
                    // Remove the predecessor-successor link
                    this.remSucc(target);
                    target.remPred(this);

                    // Remove phi node predecessor links
                    for (var j = 0; j < target.instrs.length; ++j)
                    {
                        var instr = target.instrs[j];

                        if (!(instr instanceof PhiInstr))
                            break;

                        instr.remPred(this);
                    }
                }
            }

            // For each possible target of the new instruction
            for (var i = 0; i < newVal.targets.length; ++i)
            {
                var target = newVal.targets[i];

                // Add a successor-predecessor link
                this.addSucc(target);
                target.addPred(this);
            }
        }

        // Reuse the old instruction's name and instruction id
        newVal.instrId = oldInstr.instrId;
        newVal.outName = oldInstr.outName;

        // Set the parent block for the instruction
        newVal.parentBlock = this;
    }

    // The new value is not an instruction
    else
    {
        // Remove the instruction from the list
        this.instrs.splice(index, 1);

        assert (
            !oldInstr.isBranch(),
            'cannot replace a branch by a non-instruction value'
        );

        // Free the old instruction's id number and output name
        this.parentCFG.freeInstrId(oldInstr);
        this.parentCFG.freeInstrName(oldInstr);
    }
}

/**
Test if this block is terminated by a branch instruction
*/
BasicBlock.prototype.hasBranch = function ()
{
    return this.instrs.length > 0 && this.getLastInstr().isBranch();
}

/**
Replace the branch instruction terminating a block
*/
BasicBlock.prototype.replBranch = function (newBranch)
{
    assert (
        this.hasBranch(), 'cannot remove branch, none present'
    );

    assert (
        newBranch.isBranch(),
        'cannot replace branch by non-branch'
    );

    this.replInstrAtIndex(this.instrs.length - 1, newBranch);
}

/**
Get the last instruction in the block
*/
BasicBlock.prototype.getLastInstr = function ()
{
    assert (this.instrs.length > 0, 'cannot get last instruction, none present');

    return this.instrs[this.instrs.length - 1];
}

/**
Add a predecessor block
*/
BasicBlock.prototype.addPred = function (pred)
{
    arraySetAdd(this.preds, pred);
};

/**
Remove a predecessor block
*/
BasicBlock.prototype.remPred = function (pred)
{
    arraySetRem(this.preds, pred);
};

/**
Add a successor block
*/
BasicBlock.prototype.addSucc = function (succ)
{
    arraySetAdd(this.succs, succ);
};

/**
Remove a successor block
*/
BasicBlock.prototype.remSucc = function (succ)
{
    arraySetRem(this.succs, succ);
};

/** 
Returns instructions in their order of appearance.
*/
BasicBlock.prototype.getInstrItr = function ()
{
    return new ArrayIterator(this.instrs);
};
