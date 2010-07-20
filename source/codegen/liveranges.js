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
            visited         : false,
            active          : false,
            loopIndex       : -1,
            loopDepth       : 0,
            numForwBranch   : 0,
            numBackBranch   : 0,
            loopHeader      : null,
            loopSet         : []
        };
    }

    // Next loop index to assign
    var nextLoopIndex = 0;
    
    // List of loop end blocks
    var loopEnds = [];

    // Stack for the CFG traversal
    var stack = [cfg.getEntryBlock()];

    // Until the stack is empty
    while (stack.length > 0)
    {
        var block = stack[stack.length - 1];
        var info = blockInfo[block.blockId];

        // If we are done visiting this block
        if (info.visited)
        {
            info.active = false;
            stack.pop();
            continue;
        }

        info.visited = true;
        info.active = true;

        // For each successor
        for (var i = 0; i < block.succs.length; ++i)
        {
            var succ = block.succs[i];
            var succInfo = blockInfo[succ.blockId];

            if (!succInfo.visited)
                stack.push(succ);

            // If this is a backward branch
            if (succInfo.active)
            {
                // Assign the loop header a loop index
                succInfo.loopIndex = nextLoopIndex++;

                // Increment the number of backward branches to the block
                succInfo.numBackBranch++;

                // Set the loop header for this loop
                info.loopHeader = succ;

                print('Found loop end: ' + block.getBlockName());

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

        print('Loop header: ' + loopHeader.getBlockName());

        // Array to mark visited blocks
        var visited = [];

        // Stack for the CFG traversal
        var stack = [loopEnd];

        // Until the stack is empty
        while (stack.length > 0)
        {
            var block = stack.pop();
            var info = blockInfo[block.blockId];

            print('Visiting: ' + block.getBlockName());

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


    for (var i = 0; i < cfg.blocks.length; ++i)
    {
        var block = cfg.blocks[i];
        var info = blockInfo[block.blockId];

        print(block.getBlockName());
        print(info.loopDepth);
    }


    // Function to compute a block's weight from its block info
    function blockWeight(info)
    {
        return info.loopDepth;
    }


    // TODO: linked list?





}





