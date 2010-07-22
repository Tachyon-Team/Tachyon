/**
@fileOverview
Computation of live ranges for linear scan register allocation.

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010 Maxime Chevalier-Boisvert, All Rights Reserved
*/

// TODO: weigh down exception blocks in block ordering

/**
Produce a linear order for the blocks in a CFG
*/
function orderBlocks(cfg)
{
    // Initialize the register allocation information for this block
    for (var i = 0; i < cfg.blocks.length; ++i)
    {
        cfg.blocks[i].regAlloc = {
            numForwBranch   : 0,    // Number of forward branches to this block
            numBackBranch   : 0,    // Number of backward branches to this block
            loopIndex       : -1,   // Loop identifier index
            loopDepth       : 0,    // Loop nesting depth
            loopHeader      : null, // For loop ends, corresponding loop header
            lastLoopEnd     : null, // For loop headers, last corresponding loop end block
            loops           : [],   // For loop blocks, loops to which it belongs
            weight          : 0,    // Weight of a block in the block ordering
            from            : -1,   // Operation number at the start of the block
            to              : -1,   // Operation number at the end of the block
            liveIn          : null  // Live set at the block entry
        };
    }

    // Next loop index to assign
    var nextLoopIndex = 0;
    
    // List of loop end blocks
    var loopEnds = [];

    // Stack for the CFG traversal
    var stack = [cfg.getEntryBlock()];

    // Arrays to mark visited and active blocks
    var visited = [];
    var active = [];

    // Until the stack is empty
    while (stack.length > 0)
    {
        var block = stack[stack.length - 1];

        // If we are done visiting this block
        if (visited[block.blockId])
        {
            active[block.blockId] = undefined;
            stack.pop();
            continue;
        }

        // Mark the block as visited and active
        visited[block.blockId] = true;
        active[block.blockId] = true;

        // For each successor
        for (var i = 0; i < block.succs.length; ++i)
        {
            var succ = block.succs[i];

            if (!visited[succ.blockId])
                stack.push(succ);

            // If this is a backward branch
            if (active[succ.blockId])
            {
                // Assign the loop header a loop index
                succ.regAlloc.loopIndex = nextLoopIndex++;

                // Increment the number of backward branches to the block
                succ.regAlloc.numBackBranch++;

                // Set the loop header for this loop end
                block.regAlloc.loopHeader = succ;

                // Add this block to the loop end list
                loopEnds.push(block);
            }
            else
            {
                // Increment the number of forward branches to the block
                succ.regAlloc.numForwBranch++;
            }
        }
    }

    // For each loop end block
    for (var i = 0; i < loopEnds.length; ++i)
    {
        var loopEnd = loopEnds[i];

        // Get the loop header for this loop
        var loopHeader = loopEnd.regAlloc.loopHeader;

        // Array to mark visited blocks
        var visited = [];

        // Stack for the CFG traversal
        var stack = [loopEnd];

        // Until the stack is empty
        while (stack.length > 0)
        {
            var block = stack.pop();

            // Mark this block as visited
            visited[block.blockId] = true;

            // Update the loop set for this block
            block.regAlloc.loops.push(loopHeader);

            // Update the loop depth for this block
            block.regAlloc.loopDepth = block.regAlloc.loops.length;

            // If this is the loop header, don't visit predecessors
            if (block === loopHeader)
                continue;

            // For each predecessor
            for (var j = 0; j < block.preds.length; ++j)
            {
                var pred = block.preds[j];

                if (!visited[pred.blockId])
                    stack.push(pred);
            }
        }
    }

    // Function to compute a block's weight for the block ordering
    function blockWeight(block)
    {
        return block.regAlloc.loopDepth;
    }

    // Assign a weight to each block
    for (var i = 0; i < cfg.blocks.length; ++i)
    {
        var block = cfg.blocks[i];
        block.regAlloc.weight = blockWeight(block);
    }

    // Function to sort blocks for the block order computation
    function blockSortFunc(b1, b2)
    {
        return b1.regAlloc.weight >= b2.regAlloc.weight;
    }

    // Final block order list
    var blockOrder = [];

    // Work list for the block order computation
    var workList = new LinkedList();

    // Add the entry block to the work list
    workList.addLast(cfg.entry);

    // Number of forward branches seen for each block
    var numForwSeen = [];

    // Until the work list is empty
    while (!workList.isEmpty())
    {
        // Remove the block with the highest weight
        var block = workList.remFirst();

        // Append the block to the block ordering
        blockOrder.push(block);

        // For each successor of the block
        for (var i = 0; i < block.succs.length; ++i)
        {
            var succ = block.succs[i];

            // Increment the number of incoming forward branches seen for this successor
            if (!numForwSeen[succ.blockId])
                numForwSeen[succ.blockId] = 1;
            else
                numForwSeen[succ.blockId]++;

            // If all forward branches have been seen
            if (numForwSeen[succ.blockId] == succ.regAlloc.numForwBranch)
            {
                // Add the block to the work list, sorted by decreasing weight
                workList.addSorted(succ, blockSortFunc);
            }
        }
    }

    // Compute the last loop end for header blocks
    for (var i = 0; i < blockOrder.length; ++i)
    {
        var block = cfg.blocks[i];

        if (block.regAlloc.loopHeader)
            block.regAlloc.loopHeader.regAlloc.lastLoopEnd = block;
    }

    /*
    print('Loop depth:');
    for (var i = 0; i < cfg.blocks.length; ++i)
    {
        var block = cfg.blocks[i];

        print(block.getBlockName());
        print(block.regAlloc.loopDepth);
    }

    print('Final block order:')
    for (var i = 0; i < blockOrder.length; ++i)
    {
        print(blockOrder[i].getBlockName());
        
    }
    */

    // Return the produced block order and the block information computed
    return blockOrder
}

/**
Perform instruction numbering on a control flow graph
*/
function numberInstrs(cfg, order)
{
    var nextNo = 1;

    // For each block in the order
    for (var i = 0; i < order.length; ++i)
    {
        var block = order[i];

        // Set the operation number at the block start
        block.regAlloc.from = nextNo++;

        for (var j = 0; j < block.instrs.length; ++j)
        {
            var instr = block.instrs[j];

            // Create a register allocation info object for the instruction and
            // assign the instruction an operation number
            instr.regAlloc = {
                id : nextNo++,              // Operation number
                interval : new Interval()   // Live interval
            }

            //print(instr);
            //print(instr.regAlloc.id);
        }

        // Set the operation number at the block end
        block.regAlloc.to = nextNo++;
    }
}


// TODO: live range computation
// Typed intervals, depending on temp type
// One interval can be assigned one or two physical registers (eg: 64 bit vals, lo/hi)
// Intervals have a list of range [from, to[

/*
The use positions of an interval store the id numbers of operations where the according
virtual register is used. This information is required later on to decide which interval is
split and spilled when no more registers are available, and when a spilled interval must be
reloaded into a register.

Each use position has a flag use_kind denoting whether a register is required at this
position or not: If the use position must have a register, the register allocator must
guarantee that the interval has a register assigned at this position. If the interval was spilled
to memory before this position, it is reloaded to a register. If the use position should have a
register, then the interval may be spilled. This allows the modeling of machine instructions
of the IA-32 architecture that can handle memory operands.
*/


/**
@class Represents the live interval of a temporary, with lifetime holes
*/
function Interval()
{
    /**
    List of live ranges [a, b[ in the interval
    */
    this.ranges = new LinkedList();
}
Interval.prototype = {}

/**
Set the starting point of a live interval
*/
Interval.prototype.setFrom = function (from)
{
    if (this.ranges.isEmpty())
        return;

    this.ranges.getFirst().from = from;
}

/**
Add a live range to a live interval
*/
Interval.prototype.addRange = function (from, to)
{
    var newRange = { from: from, to: to };

    if (this.ranges.isEmpty())
    {
        this.ranges.addFirst(newRange);
    }

    // Find the range before which this should be inserted
    var itr = this.ranges.find(
        newRange,
        function (r1, r2) { r1.from >= r2.from }
    );

    // Add the new range before this one
    this.ranges.addBefore(newRange, itr);

    // If there is a next range
    if (itr.isValid())
    {
        var nextRange = itr.getItem();

        // If this range and the next should be merged
        if (newRange.to >= nextRange.from)
        {
            newRange.start = Math.min(newRange.from, nextRange.from);
            newRange.to = Math.max(newRange.to, nextRange.to);

            this.ranges.remItr(itr);
        }
    }
}

/**
Compute the live intervals for the temporaries of a CFG
*/
function liveIntervals(cfg, order)
{
    // For each block in the order, in reverse order
    for (var i = order.length - 1; i >= 0; --i)
    {
        var block = order[i];

        print('Cur block: ' + block.getBlockName());

        // Variable for the currently live set
        var live = [];

        // For each successor
        for (var j = 0; j < block.succs.length; ++j)
        {
            var succ = block.succs[j];

            // If this is a loop header, skip it
            if (succ.regAlloc.lastLoopEnd)
                continue;

            /*
            print('Succ: ' + succ.getBlockName());
            print('Block id: ' + succ.blockId);
            print('Last loop end: ' + succ.regAlloc.lastLoopEnd);
            */

            // Add all live temps at the ssuccessor input to the live set
            live = arraySetUnion(live, succ.regAlloc.liveIn);

            // For each instruction of the successor
            for (var k = 0; k < succ.instrs.length; ++k)
            {
                var instr = succ.instrs[k];

                // If this is not a phi instruction, stop
                if (!(instr instanceof PhiInstr))
                    break;

                // Add the phi node's input from this block to the live set
                arraySetAdd(live, instr.getIncoming(block));
            }
        }
        
        // For each instruction in the live set
        for (var j = 0; j < live.length; ++j)
        {
            var instr = live[j];

            // Add a live range spanning this block to its interval
            instr.regAlloc.interval.addRange(
                block.regAlloc.from,
                block.regAlloc.to  
            );
        }

        // For each instruction of the block, in reverse order
        for (var j = block.instrs.length - 1; j >= 0; --j)
        {
            var instr = block.instrs[j];

            // The output of the instruction starts being live here
            instr.regAlloc.interval.setFrom(instr.regAlloc.id);

            // Remove the instruction from the live set
            arraySetRem(live, instr);

            // For each input operand of the instruction
            for (var k = 0; k < instr.uses.length; ++k)
            {
                var use = instr.uses[k];

                if (!(use instanceof IRInstr))
                    continue;

                // Make the use live from the start of the block to this instruction
                use.regAlloc.interval.addRange(
                    block.regAlloc.from, instr,
                    instr.regAlloc.id
                );

                // Add this input operand to the live set
                arraySetAdd(live, use);
            }
        }

        // For each instruction of the block
        for (var j = 0; j < block.instrs.length; ++j)
        {
            var instr = block.instrs[j];

            // If this is not a phi instruction, stop
            if (!(instr instanceof PhiInstr))
                break;

            // Remove the phi function from the live set
            arraySetRem(live, instr);
        }

        // Get the last loop end associated with this block, if any
        var lastLoopEnd = block.regAlloc.lastLoopEnd;

        // If this block is a loop header
        if (lastLoopEnd)
        {
            // For each temp in the live set at the block entry (live before the block)
            for (var j = 0; j < live.length; ++j)
            {
                var instr = live[j];

                // Add a live range spanning the whole loop
                instr.regAlloc.interval.addRange(
                    block.regAlloc.from,
                    lastLoopEnd.regAlloc.to
                );
            }
        }

        // Store the live temp set at the block entry
        block.regAlloc.liveIn = live;
    }
}

