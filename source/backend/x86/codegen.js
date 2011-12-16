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
    // Get the calling convention for this function
    var callConv = params.backend.getCallConv(irFunc.cProxy? 'c':'tachyon');

    // Map of block ids to allocation map at block entries
    // This is used to store allocations at blocks with
    // multiple predecessors
    var allocMaps = [];

    // Allocation maps at block exits
    var exitAllocMaps = [];

    // Get a reference to the entry block
    var entryBlock = irFunc.lirCFG.entry;

    // Create the register allocation map for the entry block
    // Maps temporaries and constants to register and stack locations
    // Note that constants will not be spilled on the stack
    var entryMap = new x86.RegAllocMap(backend.regSizeBytes, backend.spReg);

    // Map the entry block to its allocation map
    allocMaps[entryBlock.blockId] = entryMap;

    // Get the number of hidden arguments
    var numHidden = irFunc.cProxy? 0:2;

    // Get the number of function arguments (including hidden arguments)
    var numArgs = irFunc.argVars.length + numHidden;

    // If the function uses the arguments object
    if (irFunc.usesArguments === true)
    {
        assert (
            callConv.argRegs.length >= numHidden + 2,
            'not enough arg regs for arguments object handling'
        );

        // Set the argument count and argument table operands
        var argCntOpnd = callConv.argRegs[numHidden + 0];
        var argTblOpnd = callConv.argRegs[numHidden + 1];
    }
    else
    {
        // Map the stack arguments
        if (callConv.argOrder === 'LTR')
        {
            // For each function argument, in left-to-right order
            for (var i = callConv.argRegs.length; i < numArgs; ++i)
                entryMap.allocArg(i);
        }
        else if (callConv.argOrder === 'RTL')
        {
            // For each function argument, in left-to-right order
            for (var i = numArgs - 1; i >= callConv.argRegs.length; --i)
                entryMap.allocArg(i);
        }
        else
        {
            error('invalid argument order');
        }
    }

    // Map the return address on the stack
    entryMap.allocRetAddr();

    // Assembler object to create instructions into
    var asm = new x86.Assembler(backend.x86_64);

    // Create labels for the default and fast entry points
    var ENTRY_DEFAULT = new x86.Label('ENTRY_DEFAULT', true);
    var ENTRY_FAST    = new x86.Label('ENTRY_FAST', true);

    // Export the function's default entry point
    asm.addInstr(ENTRY_DEFAULT);

    // If we are using a calle cleanup calling convention and this is
    // not a static function, add the stack frame normalization stub
    if (callConv.cleanup === 'CALLEE' &&
        irFunc.staticLink === false)
    {
        // If the function uses the arguments object
        if (irFunc.usesArguments === true)
        {
            // Generate the argument object creation stub
            x86.genArgObjStub(
                asm,
                entryMap,
                callConv,
                argCntOpnd,
                argTblOpnd,
                params
            );
        }
        else
        {
            // Get the number of arguments expected
            const numArgsExpect = irFunc.argVars.length;

            // Compare the argument count with the expected count
            asm.cmp(callConv.argCountReg, numArgsExpect);

            // Create a label for the argument normalization code
            var ARG_NORM = new x86.Label('ARG_NORM');

            // If the count is not as expected, jump to the normalization
            // code, which is kept out of line
            asm.jne(ARG_NORM);
        }
    }

    // Export the function's fast entry point
    asm.addInstr(ENTRY_FAST);

    if (backend.debugTrace === true)
        x86.genTracePrint(asm, params, 'entering "' + irFunc.funcName + '"');

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
            edgeLabels.set(
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
        liveOutFunc: liveOutFunc,
        backend: backend,
        params: params
    };

    // For each block in the ordering
    for (var i = 0; i < blockOrder.length; ++i)
    {
        var block = blockOrder[i];

        if (config.verbosity >= log.DEBUG)
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

        if (DEBUG === true && !(allocMap instanceof x86.RegAllocMap))
        {
            error('invalid reg alloc map for: ' + block.getBlockName());
        }

        // Store the current alloc map on the code gen info object
        genInfo.allocMap = allocMap;

        // Add the label for this block
        asm.addInstr(blockLabels[block.blockId]);

        // For each instruction in the block
        for (var j = 0; j < block.instrs.length; ++j)
        {
            var instr = block.instrs[j];

            //log.debug('processing: ' + instr);

            // Get the live set at output of this instruction
            var instrLiveOut = liveness.instrOut[instr.instrId];
            
            if (DEBUG === true && !(instr instanceof PhiInstr || instrLiveOut instanceof HashMap))
            {
                error('invalid live out map for: ' + instr.getValName());
            }

            /**
            Test liveness of values before the instruction
            */
            function liveInFunc(val)
            {
                if (arraySetHas(instr.uses, val) === true)
                    return true;

                return instrLiveOut.has(val);
            }

            /**
            Test liveness of values after the instruction
            */
            function liveOutFunc(val)
            {
                return instrLiveOut.has(val);
            }

            // Store the live out function in the code generation into
            genInfo.liveOutFunc = liveOutFunc;

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

                assert (
                    irFunc.usesArguments === false || argIndex < numHidden,
                    'arg val instr for non-hidden argument but ' +
                    'function uses arguments'
                );

                // If the argument is in a register
                if (argIndex < callConv.argRegs.length)
                {
                    // Get the argument register
                    var argReg = callConv.argRegs[argIndex];

                    // Allocate the register to the argument
                    var dstReg = x86.allocReg(
                        allocMap,
                        instr,
                        (argReg !== backend.ctxReg)? argReg:undefined,
                        liveOutFunc,
                        [],
                        asm,
                        params
                    );

                    // If a new register was allocated, move the value
                    if (dstReg !== argReg)
                        asm.mov(dstReg, argReg);
                }
                else
                {
                    // Map the argument to its stack location
                    var stackSlot = allocMap.getArgSlot(argIndex);
                    allocMap.makeAlloc(instr, stackSlot);
                }
            }

            // If this is the argument count instruction
            else if (instr instanceof GetNumArgsInstr)
            {
                assert (
                    irFunc.usesArguments === true,
                    'num args instr but function does not use arguments'
                );

                // Map the argument count to its operand
                x86.allocReg(
                    allocMap,
                    instr,
                    argCntOpnd,
                    liveOutFunc,
                    undefined,
                    asm,
                    params
                );
            }

            // If this is the argument table instruction
            else if (instr instanceof GetArgTableInstr)
            {
                assert (
                    irFunc.usesArguments === true,
                    'arg table instr but function does not use arguments'
                );

                // Map the argument table to its operand
                x86.allocReg(
                    allocMap,
                    instr,
                    argTblOpnd,
                    liveOutFunc,
                    undefined,
                    asm,
                    params
                );
            }

            // If this is a trace print instruction
            else if (instr instanceof TracePrintInstr)
            {
                assert (
                    instr.uses.length === 1 &&
                    typeof instr.uses[0].value === 'string'
                );

                var str = instr.uses[0].value;

                if (DEBUG === true)
                    x86.genTracePrint(asm, params, str);
            }

            // For all other kinds of instructions
            else
            {
                assert (
                    instr.x86 !== undefined,
                    'missing instr cfg for "' + instr.mnemonic + '"'
                );

                // Perform register allocation for the instruction
                var allocInfo = x86.allocOpnds(
                    allocMap,
                    instr,
                    liveInFunc,
                    liveOutFunc,
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

    // If we are using a callee cleanup calling convention, and the
    // function isn't static, and the function doesn't use arguments,
    // then we need to insert a stub to normalize the arguments
    if (callConv.cleanup === 'CALLEE' &&
        irFunc.staticLink === false &&
        irFunc.usesArguments === false)
    {
        // Add the label for the argument normalization code
        asm.addInstr(ARG_NORM);

        // Generate the argument normalization stub
        x86.genArgNormStub(
            asm,
            callConv,
            numArgsExpect,
            params
        );
       
        // Jump to the fast entry point
        asm.jmp(ENTRY_FAST);
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
    // Get a reference to the backend
    const backend = params.backend;

    // Add the label for the transition stub
    var transLabel = edgeLabels.get({pred:pred, succ:succ});
    asm.addInstr(transLabel);

    if (config.verbosity >= log.DEBUG)
    {
        log.debug(
            'processing edge: ' + pred.getBlockName() + 
            ' -> ' + succ.getBlockName()
        );
    }

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
        for (var i = 0; i < succ.instrs.length; ++i)
        {
            var instr = succ.instrs[i];

            // If this is not a phi node, stop
            if (!(instr instanceof PhiInstr))
                break;

            // Get the incoming value for the predecessor
            var inc = instr.getIncoming(pred);

            // Get the allocation for the incoming value
            var incAlloc = x86.getBestAlloc(succAllocMap, inc);

            if (DEBUG === true && !!(inc instanceof IRInstr && incAlloc === undefined))
            {
                error(
                    'no allocation for incoming phi temporary:\n' +
                    inc
                );
            }

            // Test the liveness of a value before the phi node
            function liveInFunc(val)
            {
                // If the value is live after the phi nodes, it is live
                if (succLiveIn.has(val) === true)
                    return true;

                // If any phi node after this one has this value as incoming
                // temp, it is live
                for (var j = i; j < succ.instrs.length; ++j)
                {
                    var phi = succ.instrs[j];

                    if (!(phi instanceof PhiInstr))
                        break;

                    if (phi.getIncoming(pred) === val)
                        return true;
                }
            }

            // If this is an IR constant with no allocation or the 
            // incoming value is live before this phi node
            if (incAlloc === undefined || liveInFunc(inc) === true)
            {
                log.debug('allocating reg for phi');

                // Allocate a register for the phi node
                var reg = x86.allocReg(
                    succAllocMap,
                    instr,
                    undefined,
                    liveInFunc,
                    [],
                    asm,
                    params
                );

                // Get the allocation for the incoming value, as it may
                // have been changed by the register allocation
                var incAlloc = x86.getBestAlloc(succAllocMap, inc);

                // Move the incoming value into the register
                x86.moveValue(
                    succAllocMap,
                    reg,
                    (incAlloc !== undefined)? incAlloc:inc,
                    asm,
                    params
                );
            }
            else
            {
                log.debug('*** using inc alloc for phi: ' + incAlloc);

                // Use the incoming temp allocation for the phi node
                succAllocMap.makeAlloc(instr, incAlloc);
            }
        }

        // For each value live after the phi nodes
        for (var itr = succLiveIn.getItr(); itr.valid(); itr.next())
        {
            // Get the value
            var value = itr.get().key;

            //print('live value: ' + value.getValName());

            // If the value is a phi node from this block, skip it
            if (value instanceof PhiInstr && value.parentBlock === succ)
                continue;

            // Get the best allocation for this value
            var bestAlloc = x86.getBestAlloc(succAllocMap, value);

            //print('best alloc: ' + bestAlloc);

            //log.debug('processing live value: ' + value.getValName());
            //log.debug('best alloc: ' + bestAlloc);
            //log.debug('pred allocs: ' + predAllocMap.getAllocs(value));

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

            var node = nodeMap.get(val)

            if (node === HashMap.NOT_FOUND)
            {
                node = {
                    val: val,
                    from: undefined,
                    toCount: 0
                };

                nodeMap.set(val, node);
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

            if (DEBUG === true &&
                (predAllocs.length === 0 && srcVal instanceof IRInstr) === true)
            {
                error(
                    'no allocation for live temporary:\n' +
                    srcVal + '\n' +
                    'in pred:\n' +
                    pred.getBlockName()
                );
            }

            if (DEBUG === true &&
                (succAllocs.length === 0 && dstVal instanceof IRInstr) === true)
            {
                error(
                    'no successor alloc for value:\n' +
                    dstVal + '\n' +
                    'in succ:\n' +
                    succ.getBlockName()
                );
            }

            if (DEBUG === true &&
                (succAllocs.length > 1 && dstVal instanceof IRInstr) === true)
            {
                error(
                    'too many succ allocs for value:\n' +
                    dstVal + '\n' +
                    'in succ:\n' +
                    succ.getBlockName()
                );
            }

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

            if (DEBUG === true && !(dstNode.from === undefined))
            {
                error(
                   'already have incoming for: ' + dstVal
                );
            }

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
        const mtmReg = x86.regs.rax.getSubOpnd(backend.regSizeBits);
        const cycReg = x86.regs.rbx.getSubOpnd(backend.regSizeBits);

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

                log.debug(dstLoc + ' <== ' + srcLoc);

                asm.mov(tmpReg, srcLoc);
                asm.mov(dstLoc, tmpReg);
            }

            // If this is a move of a link value to a memory location,
            // execute it using a temporary
            else if (typeof dst === 'number' && backend.x86_64 === true &&
                     x86.isLinkValue(src) === true)
            {
                var tmpReg = getMtmTmp();
                var dstLoc = predAllocMap.getSlotOpnd(dst);

                var linkValue = new x86.LinkValue(src, backend.regSizeBits);
                asm.mov(tmpReg, linkValue);
                asm.mov(dstLoc, tmpReg);
            }

            // Perform a generic move
            else
            {
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

                /*
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
                */

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

    if (config.verbosity >= log.DEBUG)
    {
        var strStr;
        if (src instanceof x86.Operand)
            srcStr = String(src);
        else
            srcStr = String(src.getValName());
        log.debug(dst + ' <== ' + srcStr);
    }

    if (DEBUG === true &&
        (src instanceof IRValue ||
         src instanceof x86.Register ||
         src instanceof x86.MemLoc   ||
         src instanceof x86.Immediate) === false)
        error('invalid move src: ' + src);

    if (DEBUG === true &&
        (dst instanceof x86.Register ||
         dst instanceof x86.MemLoc) === false)
        error('invalid move dst: ' + dst);

    // If this is a link-time value
    if (x86.isLinkValue(src) === true)
    {
        assert (
            dst instanceof x86.Register ||
            (dst instanceof x86.MemLoc && 
             dst.size === params.backend.regSizeBits),
            'invalid destination for link value'
        );

        var linkValue = new x86.LinkValue(src, params.backend.regSizeBits);
        asm.mov(dst, linkValue);
        return;
    }

    // If the source is a constant
    if (src instanceof IRConst)
    {
        var immSize = x86.getImmSize(src, params);

        // If the constant can be encoded as an immediate
        if (immSize !== undefined && immSize <= params.backend.regSizeBits)
        {
            asm.mov(dst, src.getImmValue(params));
            return;
        }
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
Encode information about the stack frame in the instruction stream.
Note: this is used for the GC and exceptions.

Stack info format (packed):
- Call align/pad space (16-bit)
- Num stack slots (16-bit)
- Return address slot index (16-bit)
- * [kind: other:0 rptr:1 ref:2 box:3 (2 bits)]

*/
x86.writeStackInfo = function (
    instr,
    asm,
    allocMap,
    liveOutFunc,
    dynAlign,
    varArgs,
    padSpace,
    backend
)
{
    // Label to skip the stack info
    var POST_INFO = new x86.Label('POST_INFO');

    // Encode a jump instruction so it has a fixed 5-byte length
    var postInfoRef = new x86.LabelRef(POST_INFO);
    postInfoRef.size = 32;
    postInfoRef.fixedSize = true;

    // Jump past the data block
    var jmp = new x86.instrs.jmp([postInfoRef], backend.x86_64);
    asm.addInstr(jmp);

    // Create a data block for the stack info
    var data = new x86.DataBlock(); 

    // Add 3 padding bytes, for a total of 8 bytes before the data
    data.writeInt(0, 24);

    // Write magic code
    data.writeInt(1337, 16);

    // If using dynamic alignment, encode a special value in the pad space
    if (dynAlign === true)
        padSpace = 0xFFFF;
    else
        padSpace = padSpace? padSpace:0;

    // Write the padding/alignment space
    data.writeInt(padSpace, 16);

    // If the argument count is variable, encode the number
    // of argument registers with a negative sign bit
    var numSlots;
    if (varArgs !== false)
        numSlots = -varArgs;
    else
        numSlots = allocMap.numSpillSlots + allocMap.numArgSlots + 1;

    assert (
        numSlots <= getIntMax(16, false),
        'too many stack frame slots: ' + numSlots
    );

    // Write the total number of stack slots in this frame
    data.writeInt(numSlots, 16);

    // Write the return address slot index (top to bottom indexing)
    var raSlot = numSlots - allocMap.retAddrSlot - 1;
    data.writeInt(raSlot, 16);

    // If the argument count is not variable
    if (varArgs === false)
    {    
        // Slot info, encoded as a bit vector
        var slotInfo = 0;
        var numBits = 0;

        /*
        if (funcName === 'test')
        {
            var funcName = instr.parentBlock.parentCFG.ownerFunc.funcName;
            print('\nencoding info for: ' + funcName);
            print('num slots: ' + numSlots);
            print('ra slot: ' + raSlot);

            print(instr.parentBlock.parentCFG.ownerFunc);
        }
        */

        // Loop through the slots, from bottom to top
        for (var i = 0; i < numSlots; ++i)
        {
            var kind = 0;

            var val = allocMap.getAllocVal(i);

            // If this is a valid, live value
            if (val !== undefined && liveOutFunc(val) === true)
            {
                if (val instanceof IRFunction)
                    kind = 1;
                else if (val.type === IRType.ref)
                    kind = 2;
                else if (val.type === IRType.box)
                    kind = 3;
            }

            assert (
                !(i === allocMap.retAddrSlot && kind !== 0),
                'invalid kind for return address slot'
            );

            // Encode the slot kind into our bit vector
            slotInfo = num_shift(slotInfo, 2);
            slotInfo = num_add(slotInfo, kind);
            numBits += 2;

            //if (val instanceof IRValue)
            //    print(val.getValName());

            //var disp = allocMap.getSlotOpnd(i).disp;
            //print('slot disp: ' + disp);
            //print('slot kind: '  + kind);
        }

        // Pad the slot info to the nearest byte
        var remBits = numBits % 8;
        if (remBits !== 0)
            numBits += 8 - remBits;

        //print('writing slot info: ' + slotInfo + ' in ' + numBits + ' bits');

        // Write the slot kind information
        data.writeInt(slotInfo, numBits);
    }

    // Add the stack info to the instruction stream
    asm.addInstr(data);

    // Add a label after the stack info
    asm.addInstr(POST_INFO);
}

/**
Generate the argument normalization stub
*/
x86.genArgNormStub = function (
    asm,
    callConv,
    numArgs,
    params
)
{
    assert (
        callConv.argOrder === 'LTR',
        'expected left-to-right arg order in arg norm stub'
    );

    // Number of hidden arguments
    const NUM_HIDDEN_ARGS = 2;

    // Get a reference to the backend object
    const backend = params.backend;

    // Get the argument count register
    const argCountReg = callConv.argCountReg;

    // Compute the set of free registers
    var freeRegs = backend.gpRegSet.slice(0);
    for (var i = 0; i < callConv.argRegs.length; ++i)
        arraySetRem(freeRegs, callConv.argRegs[i]);
    arraySetRem(freeRegs, callConv.argCountReg);

    assert (
        freeRegs.length >= 2,
        'insufficient free register count'
    );

    // Get the temporary registers
    var tr0 = freeRegs[0];
    var tr1 = freeRegs[1];

    // Compute the number of registers available for visible arguments
    var numArgRegs = Math.max(callConv.argRegs.length - NUM_HIDDEN_ARGS, 0);

    // Compute the number of stack arguments expected
    var stackArgs = Math.max(numArgs - numArgRegs, 0);

    // Get the immediate for the undefined value
    const undefImm = new x86.Immediate(
        IRConst.getConst(undefined).getImmValue(params)
    );

    // Label for the too many arguments case
    var TOO_MANY_ARGS = new x86.Label('TOO_MANY_ARGS');

    // Label for the argument count popping
    var POP_ARG_COUNT = new x86.Label('POP_ARG_COUNT');

    // Label for the extra argument removal
    var REM_STACK_ARGS = new x86.Label('REM_STACK_ARGS');

    // Label for when we are done adding missing arguments
    var MISSING_ARGS_DONE = new x86.Label('MISSING_ARGS_DONE');

    // Label for when we are done removing extra arguments
    var EXTRA_ARGS_DONE = new x86.Label('EXTRA_ARGS_DONE');

    // Label for when the normalization is complete
    var ARG_NORM_DONE = new x86.Label('ARG_NORM_DONE');

    if (params.backend.debugTrace === true)
        x86.genTracePrint(asm, params, 'normalizing argument count');

    // If the number of expected arguments is nonzero, there could be too few
    // arguments. Otherwise, there must be too many.
    if (numArgs > 0)
    {
        // If there are too many arguments, jump out of line
        asm.cmp(argCountReg, numArgs);
        asm.ja(TOO_MANY_ARGS);

        // If there are stack arguments, pop the return address into tr0
        if (stackArgs !== 0)
            asm.pop(tr0);

        // For each expected argument
        for (var i = numArgs - 1; i >= 0; --i)
        {
            // If this is a stack argument
            if (i >= numArgRegs)
            {
                // Push the undefined value on the stack
                asm.push(undefImm);
            }
            else
            {
                // Move the undefined value into the argument register
                var argReg = callConv.argRegs[i+NUM_HIDDEN_ARGS];
                asm.mov(argReg, undefImm);
            }

            // If this was not the last argument
            if (i > 0)
            {
                // If we've added enough arguments, we are done
                asm.cmp(argCountReg, i);
                asm.jge(MISSING_ARGS_DONE);
            }
        }

        // Done adding missing args
        asm.addInstr(MISSING_ARGS_DONE);

        // Set the argument count to the expected number
        asm.mov(argCountReg, numArgs);

        // If there are stack arguments, push the return address back on the stack
        if (stackArgs !== 0)
            asm.push(tr0);

        // We are done
        asm.jmp(ARG_NORM_DONE);
    }

    // Too many arguments handling
    asm.addInstr(TOO_MANY_ARGS);

    if (params.backend.debugTrace === true)
        x86.genTracePrint(asm, params, 'removing extra arguments');

    // Pop the return address into tr0
    asm.pop(tr0);

    // If the argument count is on the stack, pop it off
    asm.cmp(argCountReg, 255);
    asm.je(POP_ARG_COUNT);

    // Move the argument count into tr1
    asm.movzx(tr1, argCountReg);

    // Add the extra stack argument removal label
    asm.addInstr(REM_STACK_ARGS);

    // Number of arguments to be kept during stack argument removal
    var numArgsKeep = Math.max(numArgs, numArgRegs);

    // If there are no extra stack arguments, we are done
    asm.cmp(tr1, numArgsKeep);
    asm.jle(EXTRA_ARGS_DONE);

    if (params.backend.debugTrace === true)
        x86.genTracePrint(asm, params, 'removing stack arg');

    // Pop a stack argument
    asm.add(backend.spReg, backend.regSizeBytes);

    // Update the argument count
    asm.sub(tr1, 1);

    // Repeat the stack argument removal loop
    asm.jmp(REM_STACK_ARGS);

    // Add the argument count popping label
    asm.addInstr(POP_ARG_COUNT);

    if (params.backend.debugTrace === true)
        x86.genTracePrint(asm, params, 'popping argument count');

    // Pop the argument count into tr1
    asm.pop(tr1);

    // Continue extra argument removal
    asm.jmp(REM_STACK_ARGS);

    // Done removing extra args
    asm.addInstr(EXTRA_ARGS_DONE);

    // Set the argument count to the expected number
    asm.mov(argCountReg, numArgs);

    // Push the return adress back on the stack
    asm.push(tr0);

    // Add the done label at the end of the stub
    asm.addInstr(ARG_NORM_DONE);

    if (params.backend.debugTrace === true)
        x86.genTracePrint(asm, params, 'argument normalization done');
}

/**
Generate the argument object creation stub
*/
x86.genArgObjStub = function (
    asm,
    allocMap,
    callConv,
    argCntOpnd,
    argTblOpnd,
    params
)
{
    assert (
        callConv.argOrder === 'LTR',
        'expected left-to-right arg order in arg norm stub'
    );

    log.debug('generating arguments object creation stub');

    // Number of hidden arguments
    const NUM_HIDDEN_ARGS = 2;

    // Get a reference to the backend object
    const backend = params.backend;

    // Get the argument count register
    const argCountReg = callConv.argCountReg;

    // Get the argument registers
    const argRegs = callConv.argRegs;

    // Compute the set of free registers
    var freeRegs = backend.gpRegSet.slice(0);
    for (var i = 0; i < callConv.argRegs.length; ++i)
        arraySetRem(freeRegs, callConv.argRegs[i]);
    arraySetRem(freeRegs, callConv.argCountReg);

    assert (
        freeRegs.length >= 2,
        'insufficient free register count'
    );

    // Get the temporary registers
    var tr0 = freeRegs[0];
    var tr1 = freeRegs[1];

    // Get the immediate for the undefined value
    const undefImm = new x86.Immediate(
        IRConst.getConst(undefined).getImmValue(params)
    );

    // Get the displacement for the arguments table
    const tblDisp = params.memLayouts.arrtbl.getFieldOffset(["tbl", 0]);

    // Create a link value for the allocArgTable function
    const allocTblFn = new x86.LinkValue(
        params.staticEnv.getBinding('allocArgTable'),
        backend.regSizeBits
    );

    // Label: argument count popping
    var POP_ARG_COUNT = new x86.Label('POP_ARG_COUNT');

    // Label: argument table creation
    var CREATE_ARG_TBL = new x86.Label('CREATE_ARG_TBL');

    // Label: argument copying loop
    var ARG_COPY_LOOP = new x86.Label('ARG_COPY_LOOP');

    // Label: argument copying loop exit
    var ARG_COPY_DONE = new x86.Label('ARG_COPY_DONE');

    // Label: argument normalization complete
    var ARG_NORM_DONE = new x86.Label('ARG_NORM_DONE');

    if (params.backend.debugTrace === true)
        x86.genTracePrint(asm, params, 'arguments object creation stub');

    // If the argument count is on the stack, pop it off
    asm.cmp(argCountReg, 255);
    asm.je(POP_ARG_COUNT);

    // Move the argument count into tr0
    asm.movzx(tr0, argCountReg);

    // Create the argument table
    asm.jmp(CREATE_ARG_TBL);

    // Add the argument count popping label
    asm.addInstr(POP_ARG_COUNT);

    // Pop the argument count into tr0
    asm.pop(tr1);
    asm.pop(tr0);
    asm.push(tr1);

    // Argument table creation
    asm.addInstr(CREATE_ARG_TBL);

    // Save the argument registers
    for (var i = 0; i < argRegs.length; ++i)
        asm.push(argRegs[i]);

    // Save tr0 (argument count)
    asm.push(tr0);

    // Set the argument count argument
    asm.mov(argRegs[2], tr0);

    // Set the function object and this arguments to undefined
    asm.mov(argRegs[0], undefImm);
    asm.mov(argRegs[1], undefImm);

    // Set the argument count
    asm.mov(argCountReg, 1);

    // Move the function address into tr0
    asm.mov(tr0, allocTblFn);

    if (params.backend.debugTrace === true)
        x86.genTracePrint(asm, params, 'calling allocArgTable');

    // Call the allocArgTable function
    asm.call(tr0);

    assert (
        allocMap.numSpillSlots === 0,
        'cannot have spill slots in var arg frame'
    );

    // Encode the stack information
    x86.writeStackInfo(
        null,
        asm,
        allocMap,
        undefined,
        false,
        argRegs.length,
        0,
        backend
    );

    // Save the argument table pointer in tr1
    asm.mov(tr1, callConv.retReg);

    if (params.backend.debugTrace === true)
        x86.genTracePrint(asm, params, 'returned from allocArgTable');

    // Restore tr0 (argument count)
    asm.pop(tr0);

    // Temporary register allocation
    var stackIdx = argRegs[0];
    var tableIdx = argRegs[1];
    var mtmTmp = argRegs[2];

    // Get the number of non-hidden register arguments
    const numRegArgs = argRegs.length - NUM_HIDDEN_ARGS;

    // Initialize the iteration indices
    asm.mov(stackIdx, tr0);
    asm.mov(tableIdx, numRegArgs);

    // Argument copying loop
    asm.addInstr(ARG_COPY_LOOP);

    // If we are at the last stack index, stop
    asm.cmp(stackIdx, 0);
    asm.jle(ARG_COPY_DONE);

    // Move a stack value into the table
    asm.mov(
        mtmTmp,
        asm.mem(
            backend.regSizeBits,
            backend.spReg,
            (argRegs.length - numRegArgs) * backend.regSizeBytes,
            stackIdx,
            backend.regSizeBytes
        )
    );
    asm.mov(
        asm.mem(
            backend.regSizeBits,
            tr1,
            tblDisp,
            tableIdx,
            backend.regSizeBytes
        ),
        mtmTmp
    );

    // Increment the indices
    asm.sub(stackIdx, 1);
    asm.add(tableIdx, 1);

    // Repeat the argument copying loop
    asm.jmp(ARG_COPY_LOOP);

    // Argument copying loop done
    asm.addInstr(ARG_COPY_DONE);

    // Restore the argument registers
    for (var i = argRegs.length - 1; i >= 0; --i)
        asm.pop(argRegs[i]);

    // Copy the register arguments to the argument table
    for (var i = NUM_HIDDEN_ARGS; i < argRegs.length; ++i)
    {
        var argReg = argRegs[i];

        asm.mov(
            asm.mem(
                backend.regSizeBits,
                tr1,
                tblDisp + (i - NUM_HIDDEN_ARGS) * backend.regSizeBytes
            ),
            argReg
        );
    }

    // Move the argument table pointer and argument count to their final positions
    asm.mov(argCntOpnd, tr0);
    asm.mov(argTblOpnd, tr1);

    // Compute the space taken by the stack arguments
    asm.mov(tr0, argCntOpnd);
    asm.sub(tr0, numRegArgs);

    // If there are no extra stack arguments, skip this step
    asm.cmp(tr0, 0);
    asm.jle(ARG_NORM_DONE);

    if (backend.debugTrace === true)
        x86.genTracePrint(asm, params, 'removing stack arguments');

    // Remove the stack arguments
    asm.imul(tr0, tr0, backend.regSizeBytes);
    asm.pop(tr1);
    asm.add(backend.spReg, tr0);
    asm.push(tr1);

    // Argument normalization done
    asm.addInstr(ARG_NORM_DONE);
}

/**
Generate code to print trace information. This is for debugging purposes.
*/
x86.genTracePrint = function (asm, params, str)
{
    const backend = params.backend;
    const gpRegs = backend.gpRegSet;
    const spReg = backend.spReg;

    // Get the C calling convention
    var callConv = backend.getCallConv('c');

    // Get the puts function object
    var putsFunc = params.ffiFuncs['puts'];

    assert (
        putsFunc !== undefined,
        'puts function not defined'
    );

    // Push all GP registers
    for (var i = 0; i < gpRegs.length; ++i)
        asm.push(gpRegs[i]);
    asm.push(backend.ctxReg);
    if (backend.x86_64 === true)
        asm.pushfq();
    else
        asm.pushfd();

    // Move the current sp to another register
    asm.mov(gpRegs[0], spReg);

    // Allocate data for the string, the sp, the function address and
    // the string pointer
    var stackSpace = (str.length + 1) + 3 * backend.regSizeBytes;
    asm.sub(spReg, stackSpace);

    // Create memory locations for the sp, function address and string pointer
    var strDisp     = backend.regSizeBytes * 3;
    var spDisp      = backend.regSizeBytes * 2;
    var fnPtrDisp   = backend.regSizeBytes;
    var strPtrDisp  = 0
    var spLoc = new x86.MemLoc(
        backend.regSizeBits,
        spReg,
        spDisp
    );
    var fnPtrLoc = new x86.MemLoc(
        backend.regSizeBits,
        spReg,
        fnPtrDisp
    );
    var strPtrLoc = new x86.MemLoc(
        backend.regSizeBits,
        spReg,
        strPtrDisp
    );

    // Align the stack pointer
    const alignMask = ~(callConv.spAlign - 1);
    asm.and(spReg, alignMask);

    // Write the string on the stack using mov
    for (var i = 0; i <= str.length; ++i)
    {
        var ch = 0;
        if (i < str.length)
        {
            ch = str.charCodeAt(i);
            if (ch < 0 || ch > 255)
                ch = '_'.charCodeAt(0);
        }

        var chLoc = new x86.MemLoc(
            8,
            spReg,
            strDisp + i
        );

        asm.mov(chLoc, ch);
    }

    // Save the old sp on the stack
    asm.mov(spLoc, gpRegs[0]);

    // Write the function pointer on the stack
    var funcPtr = putsFunc.funcPtr;
    for (var i = 0; i < funcPtr.length; ++i)
    {
        var b = funcPtr[i];

        var bLoc = new x86.MemLoc(
            8,
            spReg,
            fnPtrDisp + i
        );

        asm.mov(bLoc, b);
    }

    // Get the string pointer
    asm.lea(gpRegs[1], new x86.MemLoc(8, spReg, strDisp));

    // Store the string pointer argument
    if (callConv.argRegs.length > 0)
        asm.mov(callConv.argRegs[0], gpRegs[1]);
    else
        asm.mov(strPtrLoc, gpRegs[1]);

    // Call the function
    asm.call(fnPtrLoc);

    // Restore the sp
    asm.mov(spReg, spLoc);

    // Pop all GP registers
    if (backend.x86_64 === true)
        asm.popfq();
    else
        asm.popfd();
    asm.pop(backend.ctxReg);
    for (var i = gpRegs.length - 1; i >= 0; --i)
        asm.pop(gpRegs[i]);
}

