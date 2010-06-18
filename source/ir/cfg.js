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
function ControlFlowGraph()
{
    /**
    Construct a string representation
    */
    this.toString = function ()
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
    }

    /**
    Make a deep-copy of the control-flow graph
    */
    this.copy = function ()
    {
        // TODO

        // Must create an isomorphic copy
        // - Implies map from old blocks to new blocks

        // Tricky part is copying instructions
        // - Also need a map from old instructions to new instructions
        // - Can have a default clone for BaseInstr
        //   - Should take instr copy map as input...

        // Create a new control flow graph
        newCFG = new ControlFlowGraph();

        // Create a map from old blocks to new blocks
        blockMap = [];

        // Create a map from old instruction ids to new instructions
        instrMap = [];

        // For each basic block
        for (var i = 0; i < this.blocks.length; ++i)
        {
            // Create a copy and add it to the map
            var block = this.blocks[i];
            var newBlock = block.copy();
            blockMap[block.blockId] = newBlock;

            // For each instruction in the new basic block
            for (var j = 0; j < newBlock.instrs.length; ++j)
            {
                var instr = newBlock.instrs[j];
                var newInstr = instr.copy();
                instrMap[instr.instrId] = newInstr;


            }
        }

        
        // TODO: argN instruction?

        // TODO: PROBLEM: constant values not copied, not in CFG per-se
        // Should instructions simply do their own deep copy?
        // Could have deep/shallow copy flag?

        // Deep recursive copy could cause stack overflow
        // - Can get away without copying immutable instructions?
        //    - Not if they have dests

        // Need to do instruction graph traversal with queue?
        // Just copy all instructions, and for uncopied instructions, copy when needed...
        // BUT, non-graph instructions have no graph id....
        // - Need to attach them to a CFG instance
        //   - eg: getArgN()



        // Return the new CFG
        return newCFG;
    }

    /**
    Assign a free id number to an instruction
    */
    this.assignInstrId = function (instr)
    {
        if (this.freeInstrIds.length > 0)
            instr.instrId = this.freeInstrIds.pop();
        else
            instr.instrId = this.nextInstrId++;
    }

    /**
    Free an instruction id number
    */
    this.freeInstrId = function (instr)
    {
        this.freeInstrIds.push(instr.instrId);
    }

    /**
    Assign a free id number to a basic block
    */
    this.assignBlockId = function (block)
    {
        if (this.freeBlockIds.length > 0)
            block.blockId = this.freeBlockIds.pop();
        else
            block.blockId = this.nextBlockId++;
    }

    /**
    Free a block id number
    */
    this.freeBlockId = function (block)
    {
        this.freeBlockIds.push(block.blockId);
    }

    /**
    Create a block in this CFG
    */
    this.getNewBlock = function (label)
    {
        var block = new BasicBlock(this, label);

        this.blocks.push(block);

        // Assign an id number to this block
        this.assignBlockId(block);

        return block;
    }

    /**
    Get/create the entry block for this CFG
    */
    this.getEntryBlock = function ()
    {
        if (!this.entry)
            this.entry = this.getNewBlock('entry');

        return this.entry;
    }

    /**
    Remove a basic block from this CFG
    */
    this.remBlock = function (block)
    {
        // Remove this block from the successors of our predecessors
        for (var i = 0; i < block.preds.length; ++i)
            block.preds[i].remSucc(this);

        // Remove this block from the predecessors of our successors
        for (var i = 0; i < block.succs.length; ++i)
            block.succs[i].remPred(this);

        // Remove the block from the list
        arraySetRem(this.blocks, block);

        // Free this block's id number
        this.freeBlockId(block);
    }

    /**
    Simplify the CFG
    */
    this.simplify = function ()
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
    }

    /**
    Validate that this CFG is properly formed
    */
    this.validate = function ()
    {
        // TODO: verify that edges match
        // use edges match dest edges
        // out edges match edges in last instr
        // block in edges match block out edges
        // blocks are appropriately terminated

        // TODO: verify proper use of phi nodes
        // uses must be reachable on every incoming path
        // can do forward traversal, maintain avail defs?
        // - if def not present on one path, eliminate at merge
        // this does not need to be efficient, can do back traversal for all nodes...
    }

    /**
    Entry basic block
    @field
    */
    this.entry = null;

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

}

/**
@class Class to represent a basic block
*/
function BasicBlock(cfg, label)
{
    /**
    Produce a string representation
    */
    this.toString = function ()
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
    }

    /**
    Get the name for this basic block
    */
    this.getBlockName = function ()
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
    }

    /**
    Make a shallow copy of the basic block
    */
    this.copy = function (cfg)
    {
        // Create a new basic block
        newBlock = new BasicBlock(cfg, this.label);

        // Copy the block id
        newBlock.blockId = this.blockId;
        
        // Make a shallow copy of the instruction list
        newBlock.instrs = this.instrs.slice(0);

        // Make shallow copies of the predecessor and successor lists
        newBlock.preds = this.preds.slice(0);
        newBlock.succs = this.succs.slice(0);

        // Return the new basic block
        return newBlock;
    }

    /**
    Add an instruction at the end of the block
    */
    this.addInstr = function(instr)
    {
        // Ensure that other instructions are not added after branches
        assert (
            !(this.instrs[this.instrs.length - 1] instanceof BranchInstr),
            'cannot add instruction after branch'
        );

        // Add reverse edges from all uses to this instruction
        for (var i = 0; i < instr.uses.length; ++i)
            instr.uses[i].addDest(instr);

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
    }

    /**
    Remove an instruction from this basic block by reference
    */
    this.remInstr = function (instr)
    {
        for (var i = 0; i < this.instrs.length; ++i)
            if (this.instrs[i] === instr)
                return this.remInstrAtIndex(i);

        assert (false, 'Instruction not found in basic block');
    }

    /**
    Remove an instruction from this basic block by index
    */
    this.remInstrAtIndex = function (index)
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
    }

    /**
    Add a predecessor block
    */
    this.addPred = function (pred)
    {
        arraySetAdd(this.preds, pred);
    }

    /**
    Remove a predecessor block
    */
    this.remPred = function (pred)
    {
        arraySetRem(this.preds, pred);
    }

    /**
    Add a successor block
    */
    this.addSucc = function (succ)
    {
        arraySetAdd(this.succs, succ);
    }

    /**
    Remove a successor block
    */
    this.remSucc = function (succ)
    {
        arraySetRem(this.succs, succ);
    }

    // If no label was specified, use the empty string
    if (label == undefined || label == null)
        label = '';

    /**
    Label name string for this basic block
    @field
    */
    this.label = label;

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

cfg = new ControlFlowGraph();

entry = cfg.getEntryBlock();
l1 = cfg.getNewBlock('left1');
l2 = cfg.getNewBlock('left2');
r1 = cfg.getNewBlock('right1');
merge = cfg.getNewBlock('merge');

entry.addInstr(new IfInstr(new BoolConst(true), l1, r1));

l1.addInstr(new ArithInstr(ArithOp.ADD, new IntConst(1), new IntConst(2)));
l1.addInstr(new GetPropValInstr(new IntConst(1), new IntConst(2)));
l1.addInstr(new JumpInstr(l2));

l2.addInstr(new PhiInstr([l1.instrs[1]]));
l2.addInstr(new ArithInstr(ArithOp.MOD, l1.instrs[1], new IntConst(7)));
l2.addInstr(new ArithInstr(ArithOp.SUB, new IntConst(3), new IntConst(4)));
l2.addInstr(new ArithInstr(ArithOp.SUB, new IntConst(3), new IntConst(4)));
l2.addInstr(new JumpInstr(merge));

r1.addInstr(new ArithInstr(ArithOp.MUL, new IntConst(7), new IntConst(8)));
r1.addInstr(new JumpInstr(merge));

merge.addInstr(new PhiInstr([l1.instrs[0], r1.instrs[0]]));
merge.addInstr(new SetPropValInstr(new IntConst(1), new IntConst(2)));

print('ORIGINAL CFG: \n-------------\n');

print(cfg + '\n');

cfg.simplify();

print('SIMPLIFIED CFG: \n---------------\n');

print(cfg + '\n');


cfg2 = cfg.copy();




print("done");

