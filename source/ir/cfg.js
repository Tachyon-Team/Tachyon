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
    Entry basic block, created on CFG creation
    @field
    */
    this.entry = null;

    /**
    Function argument values
    @field
    */
    this.argVals = [];

    // Add the function arguments
    this.argVals.push(new ArgValInstr('this'));
    this.argVals.push(new ArgValInstr('argObj'));
    for (var i = 0; i < this.ownerFunc.getNumArgs(); ++i)
        this.argVals.push(new ArgValInstr('arg' + i));

    // Assign instruction ids to the function arguments
    for (var i = 0; i < this.argVals.length; ++i)
        this.assignInstrId(this.argVals[i]);
}
ControlFlowGraph.prototype = {};

/**
Construct a string representation
*/
ControlFlowGraph.prototype.toString = function ()
{
    var output = "";

    for (var i = 0; i < this.blocks.length; ++i)
    {
        var block = this.blocks[i];

        output += block;

        if (block !== this.blocks[this.blocks.length - 1])
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

    // Map the function arguments
    for (var i = 0; i < this.argVals.length; ++i)
        instrMap[this.argVals[i].instrId] = newCFG.argVals[i];

    // For each basic block
    for (var i = 0; i < this.blocks.length; ++i)
    {
        var block = this.blocks[i];

        // Create a copy and add it to the block map
        var newBlock = block.copy(newCFG);
        blockMap[block.blockId] = newBlock;

        // If this is the entry block, store the entry block reference
        if (block === this.entry)
            newCFG.entry = newBlock;

        // Add the block to the new CFG
        newCFG.blocks.push(newBlock);

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

            // Remap the use instructions
            if (instr.uses != undefined)
            {
                for (var k = 0; k < instr.uses.length; ++k)
                {
                    var use = instr.uses[k];
                    if (instrMap[use.instrId] != undefined)
                        instr.uses[k] = instrMap[use.instrId];                    
                }
            }

            // Remap the dest instructions
            if (instr.dests != undefined)
            {
                for (var k = 0; k < instr.dests.length; ++k)
                {
                    var dest = instr.dests[k];
                    if (instrMap[dest.instrId] != undefined)
                        instr.dests[k] = instrMap[dest.instrId];                  
                }
            }

            // Remap the target blocks
            if (instr.targets != undefined)
            {
                for (var k = 0; k < instr.targets.length; ++k)
                {
                    var target = instr.targets[k];
                    instr.targets[k] = blockMap[target.blockId];
                }
            }
        }
    }

    // Remap the function argument dests
    for (var i = 0; i < this.argVals.length; ++i)
    {
        var dests = this.argVals[i].dests;
        for (var j = 0; j < dests.length; ++j)
        {
            var dest = dests[j];
            newCFG.argVals[i].addDest(instrMap[dest.instrId]);
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
    this.freeInstrIds.push(instr.instrId);
};

/**
Assign a free id number to a basic block
*/
ControlFlowGraph.prototype.assignBlockId = function (block)
{
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
    this.freeBlockIds.push(block.blockId);
};

/**
Assign a free output name to an instruction
*/
ControlFlowGraph.prototype.assignInstrName = function (instr, outName)
{
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

        while (arraySetHas(this.instrNames, outName + idx))
            idx++;

        instr.outName = outName + idx;
    }

    arraySetAdd(this.instrNames, instr.outName);
};

/**
Free an instruction output name
*/
ControlFlowGraph.prototype.freeInstrName = function (instr)
{
    arraySetRem(this.instrNames, instr.outName);
};

/**
Assign a free label name to a block
*/
ControlFlowGraph.prototype.assignBlockName = function (block, labelName)
{
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

        while (arraySetHas(this.blockNames, labelName + idx))
            idx++;

        block.label = labelName + idx;
    }

    arraySetAdd(this.blockNames, block.label);
};

/**
Free a block label name
*/
ControlFlowGraph.prototype.freeBlockName = function (block)
{
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
    if (this.entry === null)
        this.entry = this.getNewBlock('entry');

    return this.entry;
};

/**
Remove a basic block from this CFG
*/
ControlFlowGraph.prototype.remBlock = function (block)
{
    // Remove this block from the successors of our predecessors
    for (var i = 0; i < block.preds.length; ++i)
        block.preds[i].remSucc(this);

    // Remove this block from the predecessors of our successors
    for (var i = 0; i < block.succs.length; ++i)
        block.succs[i].remPred(block);

    // Remove the block from the list
    arraySetRem(this.blocks, block);

    // Free this block's id number
    this.freeBlockId(block);

    // Free this block's label name
    this.freeBlockName(block);
};

/**
Get the this argument value
*/
ControlFlowGraph.prototype.getThisArg = function ()
{
    return this.argVals[0];
};

/**
Get the argument object value
*/
ControlFlowGraph.prototype.getArgObj = function ()
{
    return this.argVals[1];
};

/**
Get a function argument value
*/
ControlFlowGraph.prototype.getArgVal = function (index)
{
    assert (index < this.ownerFunc.getNumArgs(), 'invalid argument index');

    return this.argVals[index + 2];
};

/**
Simplify the CFG
*/
ControlFlowGraph.prototype.simplify = function ()
{
    //
    // Merge blocks with only one destination
    //

    // Until the merging is complete
    for (;;)
    {
        // Copy the CFG blocks list
        var blocks = this.blocks.slice(0);

        var merged = false;

        // For each block in the original CFG
        for (var i = 0; i < blocks.length; ++i)
        {
            var block = blocks[i];

            // If this block has only one successor, which has only one predecessor
            if (block.succs.length == 1 && block.succs[0].preds.length == 1)
            {
                var succ = block.succs[0];

                // Remove the final branch instruction
                block.remInstrAtIndex(block.instrs.length - 1);                    

                // Add the successor instructions to the predecessor block
                for (var j = 0; j < succ.instrs.length; ++j)
                    block.addInstr(succ.instrs[j]);

                // Remove the successor block from the CFG
                this.remBlock(succ);

                merged = true;
            }
        }

        // If no merges occurred, stop
        if (merged == false)
            break;
    }

    //
    // Iteratively eliminate phi nodes
    //
    // Delete all phi-assignments of the form:
    // Vi <- phi(...Vi...Vi...)
    //
    // If a phi-assignment has the form:
    // Vi <- phi(...Vi...Vj...Vi...Vj...)
    // 0 or more Vi and 1 or more Vj
    //
    // Then delete the assignment and rename
    // all occurences of Vi to Vj
    //

    var phiNodes = [];

    // Build a list of phi nodes in the CFG
    for (var i = 0; i < this.blocks.length; ++i)
    {
        var block = this.blocks[i];

        for (var j = 0; j < block.instrs.length; ++j)
        {
            var instr = block.instrs[j];

            if (instr instanceof PhiInstr)
                phiNodes.push(instr);
        }
    }

    // Until the simplification is complete        
    for (;;)
    {
        var phiCopies = phiNodes.slice(0);

        var simplified = false;

        // For each phi node
        for (var i = 0; i < phiCopies.length; ++i)
        {
            var phiNode = phiCopies[i];

            var numVi = 0;
            var numVj = 0;
            var Vj = null;

            // Count the kinds of uses of the phi node
            for (var j = 0; j < phiNode.uses.length; ++j)
            {
                var use = phiNode.uses[j];

                if (use === phiNode)
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
            if (numVi == phiNode.uses.length)
            {
                // Remove the phi node
                phiNode.parentBlock.remInstr(phiNode);
                arraySetRem(phiNodes, phiNode);
            }
            
            // If this phi-assignment has the form:
            // Vi <- phi(...Vi...Vj...Vi...Vj...)
            // 0 or more Vi and 1 or more Vj
            else if (numVi + numVj == phiNode.uses.length)        
            {
                // Rename all occurences of Vi to Vj
                for (var k = 0; k < phiNode.dests.length; ++k)
                    phiNode.dests[k].replUse(phiNode, Vj);

                print(phiNode);

                // Remove the phi node
                phiNode.parentBlock.remInstr(phiNode);
                arraySetRem(phiNodes, phiNode);
            }
        }

        // If no simplification occurred, stop
        if (simplified == false)
            break;
    }
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
            return 'parent CFG link broken';

        // Verify that our predecessors have us as a successor
        for (var j = 0; j < block.preds.length; ++j)
        {
            if (!arraySetHas(block.preds[j].succs, block))
                return 'predecessor missing successor link';
        }

        // Verify that our successors have us as a predecessor
        for (var j = 0; j < block.succs.length; ++j)
        {
            if (!arraySetHas(block.succs[j].preds, block))
                return 'successor missing predecessor link';
        }

        // Get a reference to the last instruction in the block
        var lastInstr = block.instrs[block.instrs.length - 1];

        // Verify that the block is terminated with a branch instruction
        if (!(lastInstr instanceof BranchInstr))
            return 'block does not terminate in a branch:\n' + block;

        // Verify that the branch targets match our successor set
        if (block.succs.length != lastInstr.targets.length)
            return 'successors do not match branch targets';
        for (var j = 0; j < block.succs.length; ++j)
        {
            if (!arraySetHas(lastInstr.targets, block.succs[j]))
                return 'successors do not match branch targets';
        }

        // For each instruction in the block
        for (var j = 0; j < block.instrs.length; ++j)
        {
            var instr = block.instrs[j];

            // Verify that the instruction has this block as its parent
            if (instr.parentBlock !== block)
            {
                print(instr.parentBlock);
                return 'parent block link broken:\n' + instr;
            }

            // Verify that no branches appear before the last instruction
            if (instr instanceof BranchInstr && j != block.instrs.length - 1)
                return 'branch before last block instruction';

            // Verify that our uses have us as a dest
            for (var k = 0; k < instr.uses.length; ++k)
            {
                if (instr.uses[k] instanceof IRInstr)
                    if (!arraySetHas(instr.uses[k].dests, instr))
                        return 'use missing dest link:\n' + instr.uses[k];
            }

            // Verify that our dests have us as a use
            for (var k = 0; k < instr.dests.length; ++k)
            {
                if (!arraySetHas(instr.dests[k].uses, instr))
                    return 'dest missing use link';
            }
        }
    }

    //
    // Verify proper use of phi nodes and instruction values
    //

    // Work list for the analysis
    var workList = [this.entry];

    // Arrays to store must and may reach sets for each block
    var mustReachOut = [];
    var mayReachOut = [];

    // Compute the set of all definitions in the CFG
    var fullReachSet = [];
    for (var i = 0; i < this.argVals.length; ++i)
    {
        fullReachSet.push(this.argVals[i]);
    }
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
        mustReachOut[block.blockId] = fullReachSet.slice(0);
        mayReachOut[block.blockId] = [];
    }

    // Until the work list is empty
    while (workList.length != 0)
    {
        var block = workList.pop();

        // Compute the must and may reach sets at this block's entry
        var mustReachCur = (block.preds.length > 0)? fullReachSet.slice(0):this.argVals.slice(0);
        var mayReachCur = (block.preds.length == 0)? this.argVals.slice(0):[];
        for (var i = 0; i < block.preds.length; ++i)
        {
            var pred = block.preds[i];
            mustReachCur = arraySetIntr(mustReachCur, mustReachOut[pred.blockId]);
            mayReachCur = arraySetUnion(mayReachCur, mayReachOut[pred.blockId]);
        }

        // For each instruction
        for (var i = 0; i < block.instrs.length; ++i)
        {
            // Add the instruction to both sets of reaching values
            var instr = block.instrs[i];
            arraySetAdd(mustReachCur, instr);
            arraySetAdd(mayReachCur, instr);
        }
        
        // If the must or may reach sets have changed for this block
        if (!arraySetEqual(mustReachCur, mustReachOut[block.blockId]) ||
            !arraySetEqual(mayReachCur, mayReachOut[block.blockId])
        )
        {
            // Update the sets for this block
            mustReachOut[block.blockId] = mustReachCur;
            mayReachOut[block.blockId] = mayReachCur;

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
        var mayReachCur = [];
        for (var j = 0; j < block.preds.length; ++j)
        {
            var pred = block.preds[j];
            mustReachCur = arraySetIntr(mustReachCur, mustReachOut[pred.blockId]);
            mayReachCur = arraySetUnion(mayReachCur, mayReachOut[pred.blockId]);
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

                var mustReach = arraySetHas(mustReachCur, use);
                var mayReach = arraySetHas(mayReachCur, use);

                if (instr instanceof PhiInstr)
                {
                    if (!mayReach)
                        return 'phi node uses non-reaching value:\n' + use;
                }
                else
                {
                    if (mayReach && !mustReach)
                        return 'instr uses value that may not reach\n' + use;
                    else if (!mustReach)
                        return 'instr uses non-reaching value\n' + use;
                }
            }

            // Add the instruction to both sets of reaching values
            arraySetAdd(mustReachCur, instr);
            arraySetAdd(mayReachCur, instr);
        }
    }

    // The CFG is valid
    return true;
};

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
BasicBlock.prototype.toString = function ()
{
    var output = this.getBlockName() + ':\n';

    for (var i = 0; i < this.instrs.length; ++i)
    {
        var instr = this.instrs[i];

        output += instr + ';';

        if (instr !== this.instrs[this.instrs.length - 1])
            output += '\n';
    }

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
Add an instruction at the end of the block
*/
BasicBlock.prototype.addInstr = function(instr, outName)
{
    // Ensure that other instructions are not added after branches
    assert (
        !(this.instrs[this.instrs.length - 1] instanceof BranchInstr),
        'cannot add instruction after branch'
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
    if (instr instanceof BranchInstr)
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

    // Add the instruction to the list
    this.instrs.push(instr);

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
    // Get a reference to the instruction
    var instr = this.instrs[index]

    // Remove the instruction from the list
    this.instrs.splice(index, 1);

    // Remove reverse edges from all uses to this instruction
    for (var i = 0; i < instr.uses.length; ++i)
        instr.uses[i].remDest(instr);

    // If this is a branch instruction
    if (instr instanceof BranchInstr)
    {
        // For each possible destination of this instruction
        for (var i = 0; i < instr.targets.length; ++i)
        {
            var target = instr.targets[i];

            // Remove the edge to the potential target block
            this.remSucc(target);

            // Remove the incoming edge to the potential target block
            target.remPred(this);
        }
    }

    // Free this instruction's id number
    this.parentCFG.freeInstrId(instr);

    // Free this instruction's output name
    this.parentCFG.freeInstrName(instr);
};

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

