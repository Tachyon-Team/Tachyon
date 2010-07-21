/**
@fileOverview
Computation of live ranges for linear scan register allocation.

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010 Maxime Chevalier-Boisvert, All Rights Reserved
*/



// TODO: linear block ordering

// TODO: instruction numbering

// TODO: live range computation



/**
Produce a linear order for the blocks in a CFG
*/
function orderBlocks(cfg)
{
    // Array mapping blocks to block information objects
    var blockInfo = [];

    // Initialize the block information for this block
    for (var i = 0; i < cfg.blocks.length; ++i)
    {
        blockInfo[cfg.blocks[i].blockId] = {
            loopIndex       : -1,
            loopDepth       : 0,
            numForwBranch   : 0,
            numBackBranch   : 0,
            loopHeader      : null,
            loopSet         : [],
            weight          : 0
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
        var info = blockInfo[block.blockId];

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
            var succInfo = blockInfo[succ.blockId];

            if (!visited[succ.blockId])
                stack.push(succ);

            // If this is a backward branch
            if (active[succ.blockId])
            {
                // Assign the loop header a loop index
                succInfo.loopIndex = nextLoopIndex++;

                // Increment the number of backward branches to the block
                succInfo.numBackBranch++;

                // Set the loop header for this loop
                info.loopHeader = succ;

                // Add this block to the loop end list
                loopEnds.push(block);
            }
            else
            {
                // Increment the number of forward branches to the block
                succInfo.numForwBranch++;
            }
        }
    }

    // For each loop end block
    for (var i = 0; i < loopEnds.length; ++i)
    {
        var loopEnd = loopEnds[i];
        var endInfo = blockInfo[loopEnd.blockId];

        // Get the loop header for this loop
        var loopHeader = endInfo.loopHeader;

        // Array to mark visited blocks
        var visited = [];

        // Stack for the CFG traversal
        var stack = [loopEnd];

        // Until the stack is empty
        while (stack.length > 0)
        {
            var block = stack.pop();
            var info = blockInfo[block.blockId];

            // Mark this block as visited
            visited[block.blockId] = true;

            // Update the loop set for this block
            arraySetAdd(info.loopSet, loopHeader);

            // Update the loop depth for this block
            info.loopDepth = info.loopSet.length;

            // If this is the loop header, don't visit predecessors
            if (block === loopHeader)
                continue;

            // For each predecessor
            for (var j = 0; j < block.preds.length; ++j)
            {
                var pred = block.preds[j];
                var predInfo = blockInfo[pred.blockId];

                if (!visited[pred.blockId])
                    stack.push(pred);
            }
        }
    }

    // Function to compute a block's weight from its block info
    function blockWeight(info)
    {
        return info.loopDepth;
    }

    // Assign a weight to each block
    for (var i = 0; i < cfg.blocks.length; ++i)
    {
        var block = cfg.blocks[i];
        var info = blockInfo[block.blockId];
        info.weight = blockWeight(info);
    }

    // Function to sort blocks for the block order computation
    function blockSortFunc(b1, b2)
    {
        return blockInfo[b1.blockId].weight >= blockInfo[b2.blockId].weight;
    }

    // Final block order list
    var blockOrder = [];

    // Work list for the block order computation
    var workList = new LinkedList();

    // Add the entry block to the work list
    workList.addLast(cfg.entry);

    // Until the work list is empty
    while (!workList.isEmpty())
    {
        // Remove the block with the highest weight
        var block = workList.remFirst();
        var info = blockInfo[block.blockId];

        // Append the block to the block ordering
        blockOrder.push(block);

        // For each successor of the block
        for (var i = 0; i < block.succs.length; ++i)
        {
            var succ = block.succs[i];
            var succInfo = blockInfo[succ.blockId];

            // Decrement the number of incoming forward branches
            succInfo.numForwBranch--;

            // If all forward branches have been seen
            if (succInfo.numForwBranch == 0)
            {
                // Add the block to the work list, sorted by decreasing weight
                workList.addSorted(succ, blockSortFunc);
            }
        }
    }

    /*
    print('Loop depth:');
    for (var i = 0; i < cfg.blocks.length; ++i)
    {
        var block = cfg.blocks[i];
        var info = blockInfo[block.blockId];

        print(block.getBlockName());
        print(info.loopDepth);
    }

    print('Final block order:')
    for (var i = 0; i < blockOrder.length; ++i)
    {
        print(blockOrder[i].getBlockName());
        
    }
    */

    // Return the produced block order
    return blockOrder;
}





