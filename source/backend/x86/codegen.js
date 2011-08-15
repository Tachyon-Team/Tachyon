/* _________________________________________________________________________
 *
 *             Tachyon : A Self-Hosted JavaScript Virtual Machine
 *
 *
 *  This file is part of the Tachyon JavaScript project. Tachyon is
 *  distributed at:
 *  http://github.com/Tachyon-Team/Tachyon
 *
 *
 *  Copyright (c) 2011, Universite de Montreal
 *  All rights reserved.
 *
 *  This software is licensed under the following license (Modified BSD
 *  License):
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions are
 *  met:
 *    * Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *    * Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions and the following disclaimer in the
 *      documentation and/or other materials provided with the distribution.
 *    * Neither the name of the Universite de Montreal nor the names of its
 *      contributors may be used to endorse or promote products derived
 *      from this software without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 *  IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 *  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 *  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL UNIVERSITE DE
 *  MONTREAL BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 *  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 *  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 *  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 *  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 *  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 *  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * _________________________________________________________________________
 */

/**
@fileOverview
x86 code generation.

@author
Maxime Chevalier-Boisvert
*/

/**
x86 namespace
*/
var x86 = x86 || {};

/**
Generate the assembly code for one function
*/
x86.genCode = function (irFunc, blockOrder, liveness, backend, params)
{
    // Assembler object to create instructions into
    var asm = new x86.Assembler(backend.x86_64);

    // Export a label for the function's default entry point
    asm.addInstr(new x86.Label('ENTRY_DEFAULT', true));

    // Map of block ids to allocation map at block entries
    // This is used to store allocations at blocks with
    // multiple predecessors
    var allocMaps = [];

    // Allocation maps at block exits
    var exitAllocMaps = [];

    // Get a reference to the entry block
    var entryBlock = irFunc.virginCFG.entry;

    // Create the register allocation map for the entry block
    // Maps temporaries and constants to register and stack locations
    // Note that constants will not be spilled on the stack
    var entryMap = new x86.RegAllocMap(backend.regSizeBytes, backend.spReg);

    // Map the entry block to its allocation map
    allocMaps[entryBlock.blockId] = entryMap;

    // Get the calling convention for this function
    var callConv = params.backend.getCallConv(irFunc.cProxy? 'c':'tachyon');

    // Get the number of function arguments
    var numArgs = irFunc.argVars.length + (irFunc.cProxy? 0:2);

    // Map the arguments on the stack
    if (callConv.argsOrder === 'LTR')
    {
        // For each function argument, in left-to-right order
        for (var i = callConv.argRegs.length; i < numArgs; ++i)
            entryMap.allocArg(i);
    }
    else
    {
        // For each function argument, in left-to-right order
        for (var i = numArgs - 1; i >= callConv.argRegs.length; --i)
            entryMap.allocArg(i);
    }

    // Map the return address on the stack
    entryMap.allocRetAddr();

    // Save the callee-save registers (if any) on the stack
    for (var i = 0; i < callConv.calleeSave.length; ++i)
    {
        var reg = callConv.calleeSave[i];

        // Get a spill slot for the register
        var slotIdx = entryMap.spillValue(reg, undefined, asm);

        // Store the register value on the stack
        x86.moveValue(
            entryMap,
            slotIdx,
            reg,
            asm,
            params
        );
    }

    //
    // TODO
    // Callee pops stack frame & args... Ideally want stack frame
    // normalization stub.
    //

    // List of block labels
    var blockLabels = [];

    // Map of CFG edges to edge transition labels
    var edgeLabels = new CfgEdgeMap();

    // For each predecessor block in the merge move map
    for (var i = 0; i < blockOrder.length; ++i)
    {
        var pred = blockOrder[i];

        // Create a label for this block
        blockLabels[pred.blockId] = new x86.Label(pred.getBlockName());

        // For each successor
        for (var j = 0; j < pred.succs.length; ++j)
        {
            var succ = pred.succs[j];

            var edge = {pred:pred, succ:succ};

            var edgeLabel = new x86.Label(
                pred.getBlockName() + '_' + 
                succ.getBlockName()
            );

            // Create a label for this CFG edge transition
            edgeLabels.addItem(
                edge,
                edgeLabel
            );
        }
    }

    // Code generation info object
    var genInfo = {
        irFunc: irFunc,
        callConv: callConv,
        edgeLabels: edgeLabels,
        allocMap: entryMap,
        backend: backend,
        params: params
    };

    // For each block in the ordering
    for (var i = 0; i < blockOrder.length; ++i)
    {
        var block = blockOrder[i];

        log.debug('processing block: ' + block.getBlockName());

        // If this block only has one predecessor, put the transition stub
        // right before it. Code has necessarily already been generated for
        // the only predecessor.
        if (block.preds.length === 1)
        {
            var pred = block.preds[0];

            // Insert the edge transition stub
            x86.genEdgeTrans(
                pred, 
                block,
                exitAllocMaps[pred.blockId],
                liveness.blockIn[block.blockId],
                allocMaps,
                blockLabels,
                edgeLabels,
                asm,
                params
            );
        }

        // Get a copy of the allocation map at the start of this block
        var allocMap = allocMaps[block.blockId].copy();

        assert (
            allocMap instanceof x86.RegAllocMap,
            'invalid reg alloc map for: ' + block.getBlockName()
        );

        // Store the current alloc map on the code gen info object
        genInfo.allocMap = allocMap;

        // Add the label for this block
        asm.addInstr(blockLabels[block.blockId]);

        // For each instruction in the block
        for (var j = 0; j < block.instrs.length; ++j)
        {
            var instr = block.instrs[j];

            log.debug('processing: ' + instr);

            // Get the live set at the input and output of this instruction
            var instrLiveIn = liveness.instrIn[instr.instrId];
            var instrLiveOut = liveness.instrOut[instr.instrId];
            assert (
                instr instanceof PhiInstr || instrLiveIn instanceof HashMap,
                'invalid live in map for: ' + instr.getValName()
            );
            assert (
                instr instanceof PhiInstr || instrLiveOut instanceof HashMap,
                'invalid live out map for: ' + instr.getValName()
            );

            // If this is a phi node
            if (instr instanceof PhiInstr)
            {
                // Do nothing, phi nodes already mapped
            }

            // If this is an argument value instruction
            else if (instr instanceof ArgValInstr)
            {
                // Get the argument index
                var argIndex = instr.argIndex;

                // If the argument is in a register
                if (argIndex < callConv.argRegs.length)
                {
                    // Allocate the register to the argument
                    x86.allocReg(
                        allocMap,
                        instr,
                        callConv.argRegs[argIndex],
                        instrLiveIn, 
                        undefined,
                        asm,
                        params
                    );
                }
                else
                {
                    // Map the argument to its stack location
                    var stackSlot = allocMap.getArgSlot(argIndex);
                    allocMap.makeAlloc(instr, stackSlot);
                }
            }

            // If this is the argument number instruction
            else if (instr instanceof GetNumArgsInstr)
            {
                assert (
                    callConv.argCountReg instanceof x86.Register,
                    'num args instr but call conv does not have arg count reg'
                );

                // Allocate the register to the argument count
                x86.allocReg(
                    allocMap,
                    undefined,
                    instrLiveIn,
                    instr, 
                    callConv.argCountReg,
                    undefined
                );
            }

            // If this is the argument table instruction
            else if (instr instanceof GetArgTableInstr)
            {
                assert (
                    irFunc.usesArguments === true,
                    'arg table instr but function does not use arguments'
                );

                // TODO: get spill slot, note its stack loc?
                error('arg table instr not yet supported');
            }

            // For all other kinds of instructions
            else
            {
                assert (
                    instr.x86 !== undefined &&
                    'missing instr cfg for "' + instr.mnemonic + '"'
                );

                // Perform register allocation for instruction
                var allocInfo = x86.allocOpnds(
                    allocMap,
                    instr,
                    instrLiveIn,
                    instrLiveOut,
                    asm,
                    params
                );

                assert (
                    instr.x86.genCode !== undefined,
                    'no genCode method for "' + instr.mnemonic + '"'
                );

                // Generate code for the instruction
                instr.x86.genCode(
                    instr, 
                    allocInfo.opnds, 
                    allocInfo.dest, 
                    allocInfo.scratch,
                    asm,
                    genInfo
                );
            }
        }

        // For each successor
        for (var succIdx = 0; succIdx < block.succs.length; ++succIdx)
        {
            var succ = block.succs[succIdx];

            // If the successor only has one predecessor
            if (succ.preds.length === 1)
            {
                // Store the exit alloc map for this block
                exitAllocMaps[block.blockId] = allocMap;

                // Skip the successor
                continue;
            }

            // Insert the edge transition stub
            x86.genEdgeTrans(
                block, 
                succ,
                allocMap,
                liveness.blockIn[succ.blockId],
                allocMaps,
                blockLabels,
                edgeLabels,
                asm,
                params
            );
        }
    }

    if (config.verbosity >= log.DEBUG)
    {
        log.debug('');
        log.debug('assembly:')
        log.debug(asm.toString(true));
        log.debug('');
    }

    // Return the assembler object
    return asm;
}

/*
Generate code for CFG block edge transitions
*/
x86.genEdgeTrans = function (
    pred,
    succ,
    predAllocMap,
    succLiveIn,
    allocMaps,
    blockLabels,
    edgeLabels,
    asm,
    params
)
{
    // Add the label for the transition stub
    var transLabel = edgeLabels.getItem({pred:pred, succ:succ});
    asm.addInstr(transLabel);

    log.debug(
        'processing edge: ' + pred.getBlockName() + 
        ' -> ' + succ.getBlockName()
    );

    // Get the allocation map for the successor
    var succAllocMap = allocMaps[succ.blockId];

    // If there is no alloc map for the successor
    if (succAllocMap === undefined)
    {
        log.debug('using pred alloc map');

        // Use the current register allocation for the successor
        var succAllocMap = predAllocMap.copy();
        allocMaps[succ.blockId] = succAllocMap;

        // For each instruction of the successor
        for (var k = 0; k < succ.instrs.length; ++k)
        {
            var instr = succ.instrs[k];

            // If this is not a phi node, stop
            if (!(instr instanceof PhiInstr))
                break;

            // Get the incoming value for the predecessor
            var inc = instr.getIncoming(pred);

            // Get the allocation for the incoming value
            var incAlloc = x86.getBestAlloc(predAllocMap, inc);

            assert (
                !(inc instanceof IRInstr && incAlloc === undefined),
                'no allocation for incoming phi temporary:\n' +
                inc
            );

            // If this is an IR constant with no allocation
            if (incAlloc === undefined)
            {
                // Allocate a register for the phi node
                var reg = x86.allocReg(
                    succAllocMap,
                    instr,
                    undefined,
                    succLiveIn,
                    [],
                    asm,
                    params
                );

                // Move the incoming value into the register
                x86.moveValue(
                    succAllocMap,
                    reg,
                    inc,
                    asm,
                    params
                );
            }
            else
            {
                // Use the incoming temp allocation for the phi node
                succAllocMap.makeAlloc(instr, incAlloc);
            }
        }

        // For each value live after the phi nodes
        for (var itr = succLiveIn.getItr(); itr.valid(); itr.next())
        {
            // Get the value
            var value = itr.get().key;

            // If the value is a phi node from this block, skip it
            if (value instanceof PhiInstr && value.parentBlock === succ)
                continue;

            // Get the best allocation for this value
            var bestAlloc = x86.getBestAlloc(succAllocMap, value);

            /*
            log.debug('processing live value: ' + value.getValName());
            log.debug('best alloc: ' + bestAlloc);
            log.debug('pred allocs: ' + predAllocMap.getAllocs(value));
            */

            // Remove all existing allocations for this value
            succAllocMap.remAllocs(value);

            // Allocate the value to its best allocation only
            if (bestAlloc !== undefined)
                succAllocMap.makeAlloc(value, bestAlloc);
        }
    }

    // There is already a register allocation for the successor
    else
    {
        log.debug('merging pred alloc map');

        // Move graph nodes
        var graphNodes = [];

        // Map of allocations to nodes
        var nodeMap = new HashMap();

        //
        //Get a node from the move graph
        //
        function getGraphNode(val)
        {
            assert (
                !(val instanceof IRInstr),
                'cannot move from/to IR instr'
            );

            var node;

            if (nodeMap.hasItem(val) === true)
            {
                node = nodeMap.getItem(val);
            }
            else
            {
                node = {
                    val: val,
                    from: undefined,
                    toCount: 0
                };

                nodeMap.setItem(val, node);
                graphNodes.push(node);
            }

            return node;
        }

        //
        //Add an edge to the move graph
        //
        function addMoveEdge(srcVal, dstVal)
        {
            // Get the allocations for this value in both blocks
            var predAllocs = predAllocMap.getAllocs(srcVal);
            var succAllocs = succAllocMap.getAllocs(dstVal);
            var succAlloc = succAllocs[0];

            assert (
                !(predAllocs.length === 0 && srcVal instanceof IRInstr),
                'no allocation for live temporary:\n' +
                srcVal + '\n' +
                'in pred:\n' +
                pred.getBlockName()
            );

            assert (
                !(succAllocs.length === 0 && dstVal instanceof IRInstr),
                'no successor alloc for value:\n' +
                dstVal + '\n' +
                'in succ:\n' +
                succ.getBlockName()
            );

            assert (
                !(succAllocs.length > 1 && dstVal instanceof IRInstr),
                'too many succ allocs for value:\n' +
                dstVal + '\n' +
                'in succ:\n' +
                succ.getBlockName()
            );

            // If the locations already match, or there is no allocation
            // for the successor, do nothing
            if (succAlloc === undefined || 
                arraySetHas(predAllocs, succAlloc) === true)
                return;

            // Get the source allocation for the move
            var predAlloc = (predAllocs.length > 0)?
                x86.getBestAlloc(predAllocMap, srcVal):
                srcVal;

            var srcNode = getGraphNode(predAlloc);
            var dstNode = getGraphNode(succAlloc);

            assert (
                dstNode.from === undefined,
                'already have incoming for: ' + dstVal
            );

            assert (
                srcNode !== dstNode,
                'move from node to itself'
            );

            dstNode.from = srcNode;
            srcNode.toCount++;
        }

        // For each instruction of the successor
        for (var insIdx = 0; insIdx < succ.instrs.length; ++insIdx)
        {
            var instr = succ.instrs[insIdx];

            // If this is not a phi node, stop
            if (!(instr instanceof PhiInstr))
                break;

            // Get the incoming value for this predecessor
            var inc = instr.getIncoming(pred);

            // Add a move edge for this incoming value
            addMoveEdge(inc, instr);
        }

        // For each value live after the phi nodes
        for (var itr = succLiveIn.getItr(); itr.valid(); itr.next())
        {
            // Get the value
            var value = itr.get().key;

            // If the value is a phi node from this block, skip it
            if (value instanceof PhiInstr && value.parentBlock === succ)
                continue;

            // Add a move edge for this value
            addMoveEdge(value, value);
        }

        // Registers for use as temporaries
        const mtmReg = x86.regs.rax.getSubOpnd(params.backend.regSizeBits);
        const cycReg = x86.regs.rbx.getSubOpnd(params.backend.regSizeBits);

        // Memory to memory and cycle breaking temporary
        var mtmTmp = null;
        var cycTmp = null;

        // Spill slots for the temporary registers
        var mtmSpill = null;
        var cycSpill = null;

        /**
        Adjust the move graph to free a specific temporary register
        */
        function freeTmpReg(tmpReg)
        {
            // Allocate a spill slot for the temp
            var slotIdx = predAllocMap.allocSpill(asm);
            var slotOpnd = predAllocMap.getSlotOpnd(slotIdx);

            log.debug('freeing tmp reg ' + tmpReg + ' using spill ' + slotIdx);

            // Move the value into its spill slot
            asm.mov(slotOpnd, tmpReg);

            // Get the graph node for the temporary register
            var regNode = getGraphNode(tmpReg);

            // Get the graph node for the spill slot
            var spillNode = getGraphNode(slotIdx)

            // What went into the register now goes into the spill slot
            spillNode.from = regNode.from;
            regNode.from = undefined;

            //log.debug('original reg toCount: ' + regNode.toCount);

            // What came from the register now comes from the spill node
            spillNode.toCount = regNode.toCount;
            regNode.toCount = 0;
            for (var i = 0; i < graphNodes.length; ++i)
            {
                var node = graphNodes[i];
                if (node.from === regNode)
                {
                    //log.debug('adjusted node from tmp: ' + node.val);
                    node.from = spillNode;
                }
            }

            // Return the spill slot index
            return slotIdx;
        }

        /**
        Get the memory->memory temporary
        */
        function getMtmTmp()
        {
            if (mtmTmp !== null)
                return mtmTmp;

            mtmSpill = freeTmpReg(mtmReg);
            mtmTmp = mtmReg;

            return mtmTmp;
        }

        /**
        Get the cycle breaking temporary
        */
        function getCycTmp()
        {
            if (cycTmp !== null)
                return cycTmp;

            cycSpill = freeTmpReg(cycReg);
            cycTmp = cycReg;

            return cycTmp;
        }

        /**
        Execute a move, which may be memory to memory
        */
        function execMove(dst, src)
        {
            // If this is a memory to memory move, execute it using a temporary
            if (typeof src === 'number' && typeof dst === 'number')
            {
                var tmpReg = getMtmTmp();
                var srcLoc = predAllocMap.getSlotOpnd(src);
                var dstLoc = predAllocMap.getSlotOpnd(dst);

                /*
                print('tmp reg: ' + tmpReg);
                print('src loc: ' + srcLoc + ' (' + src + ')');
                print('dst loc: ' + dstLoc + ' (' + dst + ')');
                */

                log.debug(dstLoc + ' <== ' + srcLoc);

                asm.mov(tmpReg, srcLoc);
                asm.mov(dstLoc, tmpReg);
            }
            else
            {
                // Perform a generic move
                x86.moveValue(
                    predAllocMap,
                    dst,
                    src,
                    asm,
                    params
                );
            }
        }

        // Save the original number of predecessor spill slots
        var origPredSpills = predAllocMap.numSpillSlots;

        // If the successor has more spill slots, add more spill slots now
        if (succAllocMap.numSpillSlots > predAllocMap.numSpillSlots)
        {
            var newSlots = succAllocMap.numSpillSlots - predAllocMap.numSpillSlots;

            log.debug('adding ' + newSlots + ' new spill slots');

            predAllocMap.pushValues(newSlots, asm);
        }

        // Until all moves are resolved
        while (graphNodes.length !== 0)
        {
            // Until all simple move situations are resolved
            var changed = true;
            while (changed === true)
            {
                changed = false;


                print('\ngraph:');
                for (var nodeIdx = 0; nodeIdx < graphNodes.length; ++nodeIdx)
                {
                    var node = graphNodes[nodeIdx];
                    print(
                        node.val + ' from: ' + (node.from? node.from.val:undefined) + 
                        ', toCount: ' + node.toCount
                    );
                }
                print('');


                // For each node of the move graph
                for (var nodeIdx = 0; nodeIdx < graphNodes.length; ++nodeIdx)
                {
                    var node = graphNodes[nodeIdx];

                    // If this node has no destinations
                    if (node.toCount === 0)
                    {
                        // If this node has an incoming value
                        if (node.from !== undefined)
                        {
                            log.debug('executing ' + node.val + ' from ' + node.from.val);

                            // Execute the move
                            execMove(node.val, node.from.val);

                            assert (
                                node.from.toCount > 0,
                                'invalid toCount for ' + node.from.val + 
                                ': ' + node.from.toCount
                            );

                            // Update the nodes
                            node.from.toCount--;
                            node.from = undefined;
                        }

                        //print('removing node: ' + node.val);

                        // Remove the node
                        graphNodes[nodeIdx] = graphNodes[graphNodes.length-1];
                        graphNodes.length--;
                        nodeIdx--;
                        
                        changed = true;
                    }
                }
            }

            // For each node of the move graph
            for (var nodeIdx = 0; nodeIdx < graphNodes.length; ++nodeIdx)
            {
                var node = graphNodes[nodeIdx];

                // If this node is part of a cycle
                if (node.from !== undefined && node.toCount > 0)
                {
                    log.debug('breaking move cycle');

                    // Get the cycle breaking temporary
                    var tmpReg = getCycTmp();

                    // Get the graph node for the temporary
                    var tmpNode = getGraphNode(tmpReg);

                    // Move the value into the temporary
                    execMove(tmpReg, node.val);

                    // All values coming from this node now come from the temporary
                    for (var nIdx = 0; nIdx < graphNodes.length; ++nIdx)
                    {
                        var n = graphNodes[nIdx];
                        if (n.from === node)
                            n.from = tmpNode;
                    }

                    tmpNode.toCount = node.toCount;
                    node.toCount = 0;

                    // Only do one cycle breaking operation at a time
                    break;
                }
            }
        }

        // Restore the temporary register values
        if (mtmSpill !== null)
            asm.mov(mtmTmp, predAllocMap.getSlotOpnd(mtmSpill));
        if (cycSpill !== null)
            asm.mov(cycTmp, predAllocMap.getSlotOpnd(cycSpill));

        // If the successor has less spill slots, remove spill slots now
        if (succAllocMap.numSpillSlots < predAllocMap.numSpillSlots)
        {
            var remSlots = predAllocMap.numSpillSlots - succAllocMap.numSpillSlots;

            log.debug('removing ' + remSlots + ' spill slots');

            predAllocMap.popValues(remSlots, asm);
        }

        // Restore the original number of predecessor map spill slots
        predAllocMap.numSpillSlots = origPredSpills;
    }

    // Jump to the successor block
    var succLabel = blockLabels[succ.blockId];
    asm.jmp(succLabel);
}

/**
Insert code to move a value from one location to another
*/
x86.moveValue = function (
    allocMap,
    dst,
    src,
    asm,
    params
)
{
    if (typeof src === 'number')
        src = allocMap.getSlotOpnd(src);

    if (typeof dst === 'number')
        dst = allocMap.getSlotOpnd(dst);

    var strStr;
    if (src instanceof x86.Operand)
        srcStr = String(src);
    else
        srcStr = String(src.getValName());
    log.debug(dst + ' <== ' + srcStr);

    assert (
        src instanceof IRValue ||
        src instanceof x86.Register ||
        src instanceof x86.MemLoc   ||
        src instanceof x86.Immediate,
        'invalid move src: ' + src
    );

    assert (
        dst instanceof x86.Register ||
        dst instanceof x86.MemLoc,
        'invalid move dst: ' + dst
    );

    // If the source is a constant
    if (src instanceof ConstValue)
    {
        var immSize = x86.getImmSize(src, params);

        // If the constant can be encoded as an immediate
        if (immSize !== undefined && immSize <= params.backend.regSizeBits)
        {
            asm.mov(dst, src.getImmValue(params));
            return;
        }
    }

    // If this is a link-time value
    if (x86.isLinkValue(src) === true)
    {
        assert (
            dst instanceof x86.Register,
            'cannot move link value to memory'
        );

        var linkValue = new x86.LinkValue(src, params.backend.regSizeBits);
        asm.mov(dst, linkValue);
        return;
    }

    // If the source is an x86 operand
    if (src instanceof x86.Operand)
    {
        assert (
            !(src instanceof x86.MemLoc && dst instanceof x86.MemLoc),
            'memory to memory move'
        );
        
        // Do the move directly
        asm.mov(dst, src);
        return;
    }

    // Error, unimplemented move
    error('unsupported move: ' + dst + ', ' + src);
}

/**
Compute the size of a constant value if it were to be
used as an immediate.
*/
x86.getImmSize = function (value, params)
{
    // If the value is not a constant, stop
    if ((value instanceof ConstValue) === false)
        return undefined;

    // If the value is not an immediate integer, stop
    if (value.isInt() === false &&
        value.value !== undefined &&
        value.value !== true &&
        value.value !== false &&
        value.value !== null)
        return undefined;

    // Get the immediate bits for the value
    var immVal = value.getImmValue(params);

    // Compute the smallest size this immediate fits in
    var size;
    if (num_ge(immVal, getIntMin(8)) && num_le(immVal, getIntMax(8)))
        size = 8;
    else if (num_ge(immVal, getIntMin(16)) && num_le(immVal, getIntMax(16)))
        size = 16;
    else if (num_ge(immVal, getIntMin(32)) && num_le(immVal, getIntMax(32)))
        size = 32;
    else
        size = 64;

    // Return the size
    return size;
}

/**
Test if an IR value must be handled at link-time
*/
x86.isLinkValue = function (value)
{
    return (
        value instanceof IRFunction ||
        (value instanceof ConstValue && value.isString())
    );
}

