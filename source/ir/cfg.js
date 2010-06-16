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
            block = this.blocks[i];

            output += block;

            if (block !== this.blocks[this.blocks.length - 1])
                output += "\n\n";
        }

        return output;
    }

    /**
    Get a temporary name that is free inside the cfg
    */
    this.getTmpName = function ()
    {
        return "$t_" + this.nextTmpIdx++;
    }

    /**
    Get a basic block name that is free inside the cfg
    */
    this.getBlockName = function ()
    {
        return "block_" + this.nextBlockIdx++;
    }

    /**
    Create a block in this CFG
    */
    this.getNewBlock = function ()
    {
        block = new BasicBlock(this);

        this.blocks.push(block);

        return block;
    }

    /**
    Get/create the entry block for this CFG
    */
    this.getEntryBlock = function ()
    {
        if (!this.entry)
        {
            this.entry = this.getNewBlock();
            this.entry.label = "entry";
        }

        return this.entry;
    }

    /**
    Remove a basic block from this CFG
    */
    this.remBlock = function (block)
    {
        // Remove each of the block's instructions
        while (block.instrs.length > 0)
            block.remInstr(block.instrs[0]);

        // Remove the block from the list
        arraySetRem(this.blocks, block);
    }

    /**
    Verify that this CFG is in proper SSA form
    */
    this.verify = function ()
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
    Simplify the CFG
    */
    this.simplify = function ()
    {
        //
        // Merge blocks with only one destination
        //

        // Copy the CFG blocks list
        var blocks = this.blocks.slice(0);

        // Until the merging is complete
        for (;;)
        {
            var merged = false;

            // For each block in the original CFG
            for (var i = 0; i < blocks.length; ++i)
            {
                var block = blocks[i];

                // If this block has only one successor
                if (block.succs.length == 1)
                {
                    var succ = block.succs[0];

                    // Remove the final branch instruction
                    block.remInstr(block.instrs[block.instrs.length - 1]);                    

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
        // TODO: iteratively eliminate phi nodes?
        //





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
    Next free temp name index
    @field
    */
    this.nextTmpIdx = 0;

    /**
    Next free block name index
    @field
    */
    this.nextBlockIdx = 0;
}

/**
@class Class to represent a basic block
*/
function BasicBlock(cfg)
{
    /**
    Produce a string representation
    */
    this.toString = function()
    {
        var output = this.label + ":\n";

        for (var i = 0; i < this.instrs.length; ++i)
        {
            var instr = this.instrs[i];

            // If the instruction is unnamed and read, give it a free name
            if (instr.hasDests() && !instr.outName)
                instr.outName = this.parentCFG.getTmpName();

            output += instr + ";";

            if (instr !== this.instrs[this.instrs.length - 1])
                output += "\n";
        }

        return output;
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
        {
            var use = instr.uses[i];
            arraySetAdd(use.dests, instr);
        }

        // If this is a branch instruction
        if (instr instanceof BranchInstr)
        {
            // For each possible destination of this instruction
            for (var i = 0; i < instr.targets.length; ++i)
            {
                target = instr.targets[i];

                // Add an edge to the potential target block
                arraySetAdd(this.succs, target);

                // Add an incoming edge to the potential target block
                arraySetAdd(target.preds, this);
            }
        }

        // Set the parent block for the instruction
        instr.parentBlock = this;

        // Add the instruction to the list
        this.instrs.push(instr);
    }

    /**
    Remove an instruction from this basic block
    */
    this.remInstr = function (instr)
    {
        // Remove the instruction from the list
        arraySetRem(this.instrs, instr);

        // Remove reverse edges from all uses to this instruction
        for (var i = 0; i < instr.uses.length; ++i)
        {
            var use = instr.uses[i];
            arraySetRem(use.dests, instr);
        }

        // If this is a branch instruction
        if (instr instanceof BranchInstr)
        {
            // For each possible destination of this instruction
            for (var i = 0; i < instr.targets.length; ++i)
            {
                target = instr.targets[i];

                // Remove the edge to the potential target block
                arraySetRem(this.succs, target);

                // Remove the incoming edge to the potential target block
                arraySetRem(target.preds, this);
            }
        }        
    }

    /**
    Label name string for this basic block
    @field
    */
    this.label = "";

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
block2 = cfg.getNewBlock();
block2.label = 'fooblock';
block3 = cfg.getNewBlock();
block3.label = 'barblock';

entry.addInstr(new JumpInstr(block2));

block2.addInstr(new ArithInstr(ArithOp.ADD, new IntConst(1), new IntConst(2)));
i1 = new GetPropValInstr(new IntConst(1), new IntConst(2));
//i2 = new IfInstr(i1, block2, block2);
block2.addInstr(i1);
//block2.addInstr(i2);
block2.addInstr(new JumpInstr(block3));

block3.addInstr(new ArithInstr(ArithOp.ADD, new IntConst(3), new IntConst(4)));

print(cfg + '\n');

cfg.simplify();

print("Printing");

print(cfg + '\n');

print("done");

