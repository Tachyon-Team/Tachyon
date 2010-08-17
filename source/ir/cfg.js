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

    // Create the entry block
    this.entry = this.getNewBlock('entry');

    // Function to add an argument instruction to the entry block
    var that = this;
    function addArg(argName)
    {
        var argInstr = new ArgValInstr(argName, that.argVals.length);
        that.argVals.push(argInstr);
        that.entry.addInstr(argInstr, argName);
    }

    // Add the function arguments
    addArg('this');
    addArg('funcObj');
    addArg('argObj');
    var argNames = this.ownerFunc.getArgNames();
    for (var i = 0; i < argNames.length; ++i)
        addArg(argNames[i]);

    // Add the global object value
    var globalObj = new GetGlobalInstr(this.argVals[1])
    this.argVals.push(globalObj);
    this.entry.addInstr(globalObj, 'global');
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

    // Remove the entry block from the new CFG
    newCFG.blocks = [];

    // For each basic block
    for (var i = 0; i < this.blocks.length; ++i)
    {
        var block = this.blocks[i];

        // Create a copy of the block
        var newBlock = block.copy(newCFG);

        // If this is the entry block, set the entry block for the new CFG
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

    // Map the function arguments
    for (var i = 0; i < this.argVals.length; ++i)
        newCFG.argVals[i] = instrMap[this.argVals[i].instrId];

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
                if (instrMap[use.instrId] != undefined)
                    instr.uses[k] = instrMap[use.instrId];                    
            }

            // Remap the dests
            for (var k = 0; k < instr.dests.length; ++k)
            {
                var dest = instr.dests[k];
                if (instrMap[dest.instrId] != undefined)
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
            else if (instr instanceof BranchInstr)
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
    return this.entry;
};

/**
Remove a basic block from this CFG
*/
ControlFlowGraph.prototype.remBlock = function (block)
{
    print('Removing block: ' + block.label);

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
                    // Remove this value
                    instr.preds.splice(k, 1);
                    instr.uses.splice(k, 1);
                    
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
Get the this argument value
*/
ControlFlowGraph.prototype.getThisArg = function ()
{
    return this.argVals[0];
};

/**
Get the function object value
*/
ControlFlowGraph.prototype.getFuncObj = function ()
{
    return this.argVals[1];
};

/**
Get the argument object value
*/
ControlFlowGraph.prototype.getArgObj = function ()
{
    return this.argVals[2];
};

/**
Get a function argument value
*/
ControlFlowGraph.prototype.getArgVal = function (index)
{
    assert (index < this.ownerFunc.getNumArgs(), 'invalid argument index');

    return this.argVals[index + 3];
};

/**
Get the global object value
*/
ControlFlowGraph.prototype.getGlobalObj = function ()
{
    return this.argVals[this.argVals.length - 1];
};

/**
Simplify the CFG
*/
ControlFlowGraph.prototype.simplify = function ()
{
    // Build a list of phi nodes in the CFG
    var phiNodes = [];
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

    // Iterate until the simplification is complete
    for (;;)
    {
        // No simplifications made in this iteration yet
        var simplified = false;

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

        // Copy the list of phi nodes in the CFG
        var phiCopies = phiNodes.slice(0);

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
                //print('Removing: ' + phiNode);

                // Remove the phi node
                phiNode.parentBlock.remInstr(phiNode);
                arraySetRem(phiNodes, phiNode);

                // Set the simplified flag
                simplified = true;
            }
            
            // If this phi-assignment has the form:
            // Vi <- phi(...Vi...Vj...Vi...Vj...)
            // 0 or more Vi and 1 or more Vj
            else if (numVi + numVj == phiNode.uses.length)        
            {
                //print('Renaming: ' + phiNode);

                // Rename all occurences of Vi to Vj
                for (var k = 0; k < phiNode.dests.length; ++k)
                {
                    var dest = phiNode.dests[k];
                    dest.replUse(phiNode, Vj);
                    if (Vj instanceof IRInstr)
                        Vj.addDest(dest);
                }

                // Remove the phi node
                phiNode.parentBlock.remInstr(phiNode);
                arraySetRem(phiNodes, phiNode);

                // Set the simplified flag
                simplified = true;
            }
        }

        // For each block in the CFG
        for (var i = 0; i < this.blocks.length; ++i)
        {
            var block = this.blocks[i];

            //
            // Eliminate blocks with no predecessors
            //

            // If this block has no predecessors, and it is not the entry block
            if (
                block.preds.length == 0 &&
                block !== this.entry
            )
            {
                // Remove the block from the CFG
                this.remBlock(block);

                // Set the simplified flag
                simplified = true;
            }

            //
            // Merge blocks with only one destination
            //

            // If this block has only one successor, which has only one predecessor
            // and the block is not terminated by a throw instruction
            else if (
                block.succs.length == 1 && 
                block.succs[0].preds.length == 1 &&
                block !== block.succs[0] &&
                !(block.getLastInstr() instanceof ThrowInstr)
            )
            {
                var succ = block.succs[0];

                // Remove the final branch instruction
                block.remInstrAtIndex(block.instrs.length - 1);                    

                // For each instruction of the successor
                for (var j = 0; j < succ.instrs.length; ++j)
                {
                    var instr = succ.instrs[j];
                    
                    // If the instruction is a phi node
                    if (instr instanceof PhiInstr)
                    {
                        // Ensure that this phi node has only one use
                        assert (
                            instr.uses.length == 1, 
                            'phi node in merged block should have only one use'
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
                    else
                    {
                        // Add the instruction to the predecessor
                        block.addInstr(instr);
                    }
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

                        var phiPreds = instr.preds;

                        //print('Fixing up phi node: ' + instr);

                        // For each predecessor of the phi node
                        for (var l = 0; l < phiPreds.length; ++l)
                        {
                            // If this is a reference to the successor, make it a reference to the predecessor instead
                            if (phiPreds[l] === succ)
                                phiPreds[l] = block;
                        }

                        //print('Fixed up: ' + instr);
                    }
                }

                // Remove the successor block from the CFG
                this.remBlock(succ);

                // Set the simplified flag
                simplified = true;
            }

            //
            // Jump over blocks with no instructions and a single successor
            //

            // If this block has only one successor, no instructions but a branch, 
            // and the block is not terminated by a throw instruction
            else if (
                block.succs.length == 1 && 
                block.instrs.length == 1 &&
                !(block.getLastInstr() instanceof ThrowInstr)
            )
            {
                var succ = block.succs[0];
            
                //print('got block: ' + block.getBlockName());

                // Copy the predecessor list for this block
                var preds = block.preds.slice(0);

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

                    //print('*** simplifying ***');

                    // Remove the predecessor from our predecessor list
                    arraySetRem(block.preds, pred);

                    // Get the predecessor's branch instruction
                    var branchInstr = pred.getLastInstr();

                    // Replace the predecessor's branch target by the successor
                    for (var k = 0; k < branchInstr.targets.length; ++k)
                        if (branchInstr.targets[k] === block)
                            branchInstr.targets[k] = succ;

                    // Replace the predecessor's successor block by the successor
                    for (var k = 0; k < pred.succs.length; ++k)
                        if (pred.succs[k] === block)
                            pred.succs[k] = succ;

                    // Add the predecessor as a predecessor of the successor
                    succ.preds.push(pred);

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

                    // Set the simplified flag
                    simplified = true;
                }
            }
        }

        // If no simplifications occurred, stop
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
            throw 'parent CFG link broken';

        // Verify that our predecessors have us as a successor
        for (var j = 0; j < block.preds.length; ++j)
        {
            if (!arraySetHas(block.preds[j].succs, block))
                throw 'predecessor missing successor link to:\n' + block;
        }

        // Verify that our successors have us as a predecessor
        for (var j = 0; j < block.succs.length; ++j)
        {
            if (!arraySetHas(block.succs[j].preds, block))
                throw 'successor missing predecessor link to:\n' + block;
        }

        // Get a reference to the last instruction in the block
        var lastInstr = block.instrs[block.instrs.length - 1];

        // Verify that the block is terminated with a branch instruction
        if (!(lastInstr instanceof BranchInstr))
            throw 'block does not terminate in a branch:\n' + block;

        // Verify that the branch targets match our successor set
        if (block.succs.length != lastInstr.targets.length)
            throw 'successors do not match branch targets';
        for (var j = 0; j < block.succs.length; ++j)
        {
            if (!arraySetHas(lastInstr.targets, block.succs[j]))
                throw 'successors do not match branch targets';
        }

        // For each instruction in the block
        for (var j = 0; j < block.instrs.length; ++j)
        {
            var instr = block.instrs[j];

            // Verify that the instruction has this block as its parent
            if (instr.parentBlock !== block)
                throw 'parent block link broken:\n' + instr;

            // If this is a phi instruction
            if (instr instanceof PhiInstr)
            {
                // Verify that it appears at the start of the block
                if (j != 0 && !(block.instrs[j-1] instanceof PhiInstr))
                   throw 'phi node after non-phi instruction';

                // Verify that each immediate predecessor has a corresponding use
                for (var k = 0; k < block.preds.length; ++k)
                    if (!arraySetHas(instr.preds, block.preds[k]))
                        throw 'phi node does not cover all immediate predecessors';

                // Verify that there is exactly one predecessor for each use
                if (instr.preds.length != instr.uses.length)
                    throw 'phi node does not have one predecessor for each use';

                // Verify that there are no more phi uses than block predecessors
                if (instr.preds.length != block.preds.length)
                    throw 'phi node has more uses than predecessors';
            }

            // Verify that no branches appear before the last instruction
            if (instr instanceof BranchInstr && j != block.instrs.length - 1)
                throw 'branch before last block instruction';

            // Verify that our uses have us as a dest
            for (var k = 0; k < instr.uses.length; ++k)
            {
                if (instr.uses[k] instanceof IRInstr)
                    if (!arraySetHas(instr.uses[k].dests, instr))
                        throw 'use missing dest link:\n' + instr.uses[k];
            }

            // Verify that our dests have us as a use
            for (var k = 0; k < instr.dests.length; ++k)
            {
                if (!arraySetHas(instr.dests[k].uses, instr))
                    throw 'dest missing use link:\n' + instr.dests[k] + ' using ' + instr;
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
        mustReachOut[block.blockId] = fullReachSet.slice(0);
    }

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

        // For each instruction
        for (var i = 0; i < block.instrs.length; ++i)
        {
            // Add the instruction to both sets of reaching values
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
                        throw 'phi node uses non-reaching value';
                }
                else
                {
                    if (!arraySetHas(mustReachCur, use))
                        throw 'instr uses non-reaching value\n' + use;
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
ControlFlowGraph.prototype.getBlockIterator = function (type)
{
    if (type === undefined)
    {
        type = "basic";
    }

    var it = Object.create(this.getBlockIterator.prototype);
    it.index = -1;
    it.next();

    if (type === "basic")
    {
        it.blocks = this.blocks;
    } else if (type === "strict")
    {
        // TODO: Migrate allocator.orderBlocks here
        error("strict mode unimplemented");

        // TODO: When asked for a strict iterator, try to update the previous
        //       calculated order instead of computing it from scratch.
    } else 
    {
        error("unrecognized option: '" + type + "'");
    }

    return it;
};

/** 
Test whether the iterator has visited all blocks of the cfg 
*/
ControlFlowGraph.prototype.getBlockIterator.prototype.end = function ()
{
    return this.index >= this.blocks.length;
};

/** 
Move the iterator to the next item 
*/
ControlFlowGraph.prototype.getBlockIterator.prototype.next = function ()
{
    this.index++;
};

/** 
Returns the current item being visited 
*/
ControlFlowGraph.prototype.getBlockIterator.prototype.get = function ()
{
    return this.blocks[this.index];
};

/** 
Returns an instruction iterator. Depending on the given type, the order of
visited instructions might have certain properties.

basic: instructions might be returned in any order
strict: instructions part of predecessor blocks always appear before 
        instructions part of their successors and
        instructions part of a loop are always contiguous

@param {String} type 
*/ 
ControlFlowGraph.prototype.getInstrIterator = function (type)
{
    var it = Object.create(this.getInstrIterator.prototype);
    it.blockIt = this.getBlockIterator(type);
    it.instrIt = it.blockIt.get().getInstrIterator();
    return it;
};

/** Test whether the iterator has visited all instructions of the cfg */
ControlFlowGraph.prototype.getInstrIterator.prototype.end = function ()
{
    return this.blockIt.end() && this.instrIt.end();
};

/**
Move the iterator to the next item
*/
ControlFlowGraph.prototype.getInstrIterator.prototype.next = function ()
{
    this.instrIt.next();
    if (this.instrIt.end())
    {
        this.blockIt.next();
        if (!this.blockIt.end())
        {
            this.instrIt = this.blockIt.get().getInstrIterator();
        }
    }
};

/**
Returns the current item being visited
*/
ControlFlowGraph.prototype.getInstrIterator.prototype.get = function ()
{
    return this.instrIt.get();
};

/**
Returns an edge iterator.  Edges might be returned in any order.
*/
ControlFlowGraph.prototype.getEdgeIterator = function ()
{
    var it = Object.create(this.getEdgeIterator.prototype); 
    it.succIndex = -1;
    it.blockIt = this.getBlockIterator();
    it.next();
    return it;
};

/**
Test whether the iterator has visited all edges of the cfg
*/
ControlFlowGraph.prototype.getEdgeIterator.prototype.end = function ()
{
    return this.blockIt.end(); 
};

/**
Move the iterator to the next item
*/
ControlFlowGraph.prototype.getEdgeIterator.prototype.next = function ()
{
    this.succIndex++; 

    while (!this.blockIt.end() && 
           this.succIndex >= this.blockIt.get().succs.length)
    {
        this.blockIt.next();
        this.succIndex = 0;
    }
};

/**
Returns the current item being visited
*/
ControlFlowGraph.prototype.getEdgeIterator.prototype.get = function ()
{
    return {pred:this.blockIt.get(), 
            succ:this.blockIt.get().succs[this.succIndex]};
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
Insert an instruction into the block
@param instr instruction to insert
@param outName output name of the instruction
@param index before which to insert the new instruction
*/
BasicBlock.prototype.addInstr = function(instr, outName, index)
{
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

    // If we are inserting at the end of the block
    if (index == this.instrs.length)
    {
        // Add the instruction to the end of the list
        this.instrs.push(instr);
    }
    else
    {
        // Insert the instruction before the index
        var ls = this.instrs.slice(0, index);
        var rs = this.instrs.slice(index, this.instrs.length + 1);
        this.instrs = ls.concat(instr, rs);
    }

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
    print('Removing instr: ' + this.instrs[index]);

    // Get a reference to the instruction
    var instr = this.instrs[index]

    // Remove the instruction from the list
    this.instrs.splice(index, 1);

    // Remove reverse edges from all uses to this instruction
    for (var i = 0; i < instr.uses.length; ++i)
        if (instr.uses[i] instanceof IRInstr)
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
Test if this block is terminated by a branch instruction
*/
BasicBlock.prototype.hasBranch = function ()
{
    return this.instrs.length > 0 && this.getLastInstr() instanceof BranchInstr;
}

/**
Remove the branch instruction terminating a block
*/
BasicBlock.prototype.remBranch = function ()
{
    assert (this.hasBranch(), 'cannot remove branch, none present');

    this.remInstrAtIndex(this.instrs.length - 1);
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
BasicBlock.prototype.getInstrIterator = function ()
{
    var it = Object.create(this.getInstrIterator.prototype);
    it.index = -1;
    it.instrs = this.instrs;
    it.next();
    return it;
};

/**
Test whether the iterator has visited all instructions of the block
*/
BasicBlock.prototype.getInstrIterator.prototype.end = function ()
{
    return this.index >= this.instrs.length;
};

/**
Move the iterator to the next item
*/
BasicBlock.prototype.getInstrIterator.prototype.next = function ()
{
    this.index++;
};

/**
Returns the current item being visited
*/
BasicBlock.prototype.getInstrIterator.prototype.get = function ()
{
    return this.instrs[this.index];
};

