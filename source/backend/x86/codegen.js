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
Register allocation and x86 code generation.

@author
Maxime Chevalier-Boisvert
*/





/*
TODO:
Merge code gen & reg alloc
- Pass code gen an object with reg alloc/code gen methods
- Generate code during reg alloc
- Use stack ref virtual operand during code generation

Add/sub can call get_r_rmi_opnds to get their operands?
- Auto manages dest is opnd0
- Auto manages immediate operands
- Can always define a function for arith ops that calls reg alloc primitives
- No crazy operand allocation loop
- No need to worry about max mem, max reg opnds
- Most instructions don't have write, spill regs
- For most instructions, this will probably be quite simple and fast.

Problem: can spill already allocated operand, or spill values of other
operands of the instruction.

We know what reg operands we already allocated for the current instruction.
Need to keep track of this. Can make list of existing (not yet allocated)
reg allocs for current instruction. We should try to avoid spilling these if
possible.

Issue:
- If we get a mem opnd for the first operand of add, can't get a second mem
opnd. Need method to alloc both at once for this.

Need method to specify to the reg allocator where the dest is?
- Could just tell the register allocator where the dest was written?
  - No. The register allocator needs to know to spill the value before it is overwritten...
- Could do alloc_dest() before doing the operation
  - Allocator will spill the operand if needed, if it is still live after the
    instruction.

alloc_r_opnd(use)
alloc_rm_opnd(use)
alloc_dest(opnd)

alloc r and alloc rm can be primitives
Can implement alloc_r_rm, alloc_r_rmi on top

Fixed registers:
alloc_fixed_reg(use)

Temp registers:
alloc_temp_reg()

Spill registers:
spill_reg(reg)

For division instruction, need to be able to exclude a reg from allocation
exclude_reg(reg)?

Excludes from allocation. spill_reg allows a reg to be used for arguments.
Only forces it to be saved.

--------------------------------------------------------------------------

Code Generator
--------------

Do we want a code generator object with methods?
===> Probably not

Current plan:
- Call codeGen, have nested functions to do register allocation
- Place methods and useful values on codeGen object passed to code
  generation functions
- Probably not slower than code gen object... Only creating one object

Alternative:
- Have code generator object with many fields. Create new instance when
  generating code for a method
- Maintain field values up to date as we proceed
- Pass this reference to code generation functions
- Disadvantag: always using this reference, everything on the code generator object...
- Disadvantage: too much info passed to code gen functions

--------------------------------------------------------------------------

Alloc Map
---------

x86.StackRef operand to replace later?
- Reference stack slots or arg slots

Do we need a stack frame map if we increment the stack pointer dynamically?

May not need to. Could map the stack frame data to offsets directly in the
alloc map.

Simpler code? Already maintain spill location indices in alloc map...
Could request mem locs for stack allocated objects directly...

argN
...
arg1
arg0
ret addr <-- initial SP
spill0
spill1
...
spillN <-- final SP


Need to maintain:
- total number of stack slots
- arg slot indices
- return value index
- save reg indices
- spill slot indices
- list of spill slot indices?

Probably want to keep stack alloc map! Don't want to uselessly copy all this
information for every block!

Map values to list of regs, stack loc indices

Should ideally count stack slots from the bottom to the top. eg: arg0 is
slot 0. This way, the stack slot indices do not change.


In reg alloc map: ***
- Want number of spill slots and total number of stack slots?
- Only need stack fame size? Increment it when we can't find a spill slot
- Modify stack frame map. No longer need to allocate spill slots?
  - Can compute spill index without maintaining a map?
- Not quite true! Saved registers are not in "normal" save slots...
- Would need to integrate them to eliminate spill map
- If spilled, remain live until end of function
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

        // Spill the register on the stack
        var slotIdx = entryMap.spillValue(reg, undefined, asm);
        var memLoc = entryMap.getSlotOpnd(slotIdx);
        asm.mov(memLoc, reg);
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

    // Set of general-purpose registers available for allocation
    const gpRegSet = backend.x86_64? [
        x86.regs.rax,
        // For now, used as context register
        //x86.regs.rcx,
        x86.regs.rdx,
        x86.regs.rbx,
        x86.regs.rbp,
        x86.regs.rsi,
        x86.regs.rdi,
        x86.regs.r8,
        x86.regs.r9,
        x86.regs.r10,
        x86.regs.r11,
        x86.regs.r12,
        x86.regs.r13,
        x86.regs.r14,
        x86.regs.r15
    ]:[
        x86.regs.eax,
        // For now, used as context register
        //x86.regs.ecx,
        x86.regs.edx,
        x86.regs.ebx,
        x86.regs.ebp,
        x86.regs.esi,
        x86.regs.edi
    ];

    // Code generation helper object
    var codeGen = {
        callConv: callConv,
        edgeLabels: edgeLabels,
        allocMap: entryMap,
        backend: backend,
        params: params
    };



    /*
    TODO: 

    Want:
    alloc_r_opnd(use)
    alloc_rm_opnd(use)
    alloc_dest(opnd)
    alloc_fixed_reg(use)
    alloc_temp_reg()
    spill_reg(reg)
    exclude_reg(reg)        // exclude a register from allocation (e.g: for division)
    alloc_dst(opnd)

    alloc r and alloc rm can be primitives
    Can implement alloc_r_rm, alloc_r_rmi on top?

    TODO: PROBLEM:
    Don't want to spill a previously register allocated operand...
    Not an issue for one-opnd instructions
    Need to have an exclusion set? Would be simpler.

    If allocating for add instruction, can have one immediate operand only,
    but it can be either of src or dst.

    TODO: start simple, alloc all to registers...

    Will need function to allocate multiple operands at once...
    such as alloc_r_rm, alloc_r_rmi

    This will require using an exclude set.

    Exclude set could be optional parameter to all primitive alloc functions.

    What about instructions like load/store? May require up to 4-5 operands.

    Normally, the alloc map should know what uses are allocated to a given
    register already... Exclude set could be handled automatically***
    - Still doesn't do manual exclusion

    Dest as opnd0...
    May need special optimization care but... Can handle this later.

    TODO PROBLEM:

    Once we have opnds to operate on... Stack slot indices cannot be used
    directly by assembler instructions, need to translate them to memory
    operands...

    Options:
    - Translate memory operands manually
      - May not be so bad, not that many...
    - Pre-store allocated operands, translate automatically
      - codeGen.get_opnd(idx)
    - Auto-translate from higher-level allocation functions
    */




    /**
    Function to allocate a register
    @param allocMap allocation map
    @param liveSet set of live temporaries
    @param value value to be allocated, may be undefined
    @fixedReg fixed register we must allocate to, undefined
    @excludeSet set of registers to exclude from allocation
    */
    function allocReg(allocMap, liveSet, value, fixedReg, excludeSet)
    {
        // Best register found so far
        var bestReg = undefined;
        var bestRegWeight = MAX_FIXNUM;

        // If we must allocate to a fixed register
        if (fixedReg !== undefined)
        {
            // Allocate to the fixed register
            bestReg = fixedReg;
        }
        else
        {
            // Set of registers to allocate from
            var regSet = gpRegSet;

            // For each register in the register set
            for (var i = 0; i < regSet.length; ++i)
            {
                var reg = regSet[i];

                // Get the sub-register corresponding to the value size
                var subReg;
                if (value !== undefined)
                    subReg = reg.getSubOpnd(value.type.getSizeBits(params));
                else
                    subReg = reg;
                
                // If a REX prefix is needed and we aren't in 64-bit, skip it
                if (subReg.rexNeeded === true && backend.x86_64 !== true)
                    continue;

                // If this register is in the excluded set, skip it
                if (arraySetHas(excludeSet, reg) === true)
                    continue;

                // Get the value the register currently maps to
                var regVal = allocMap.getRegVal(reg);

                // Weight for the value currently mapped to the register
                var weight = 0;

                // If the register is mapped to a live value
                if (regVal !== undefined && liveSet.hasItem(regVal) === true)
                {
                    weight += 1;

                    if (regVal instanceof IRInstr)
                        weight += regVal.uses.length;
                }

                // Update the best register choice
                if (weight < bestRegWeight)
                {
                    bestReg = reg;
                    bestRegWeight = weight;
                }
            }
        }

        assert (
            bestReg !== undefined,
            'could not allocate register'
        );

        // Free the register, if needed
        freeOpnd(allocMap, liveSet, bestReg, excludeSet);

        // Update the allocation map
        if (value !== undefined)
            allocMap.makeAlloc(value, bestReg);

        // Return the allocated register
        return bestReg;
    }

    /**
    Free a register or memory operand if needed. 
    This may cause moves and spills.
    */
    function freeOpnd(allocMap, liveSet, opnd, excludeSet)
    {
        // Get the current value for this operand
        var curVal = allocMap.getAllocVal(opnd);

        // If the operand isn't mapped to a value, do nothing
        if (curVal === undefined)
            return;

        // Remove the allocation of the operand to the value
        allocMap.remAlloc(curVal, opnd);

        // If the operand is not mapped to a live value, do nothing
        if (liveSet.hasItem(curVal) === false)
            return;

        // if the value is a constant, do nothing
        if ((curVal instanceof IRInstr) === false)
            return;

        // Get the current allocations for the value
        var allocs = allocMap.getAllocs(curVal);

        // If there is already another allocation for the value, do nothing
        if (allocs.length > 0)
            return;

        // New operand to move the value into
        var newOpnd;
    
        // If the operand is a register
        if (opnd instanceof x86.Register)
        {
            // Map the value to a stack spill slot
            var stackSlot = allocMap.spillValue(curVal, liveSet, asm);

            // Get the memory operand for the spill slot
            newOpnd = allocMap.getSlotOpnd(stackSlot);
        }
        else
        {
            // Allocate the value to a register
            newOpnd = allocReg(
                allocMap,
                liveSet,
                curVal,
                undefined,
                excludeSet
            );
        }

        // Move the value to the spill slot
        asm.mov(newOpnd, opnd);
    }

    /**
    Get the best current allocation for a value
    */
    function getBestAlloc(allocMap, value)
    {
        var allocs = allocMap.getAllocs(value);

        var bestAlloc = undefined;

        for (var i = 0; i < allocs.length; ++i)
        {
            var alloc = allocs[i];

            if (alloc instanceof x86.Register)
                bestAlloc = alloc;

            else if (bestAlloc === undefined)
                bestAlloc = alloc;
        }

        return bestAlloc;
    }

    /**
    Insert code to move a value from one location to another
    */
    function moveValue(allocMap, dst, src)
    {
        if (typeof src === 'number')
            src = allocMap.getSlotOpnd(src);

        if (typeof dst === 'number')
            dst = allocMap.getSlotOpnd(dst);

        assert (
            src instanceof IRValue ||
            src instanceof x86.Register ||
            src instanceof x86.MemLoc,
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
            if (immSize !== undefined && immSize <= backend.regSizeBits)
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

    // Allocated instruction input and destination operands
    var instrOpnds = [];
    var instrDst = undefined;

    /**
    Allocate an operand to a register
    */
    codeGen.alloc_opnd_r = function (use, fixedReg, excludeSet)
    {
        assert (
            use instanceof IRValue,
            'invalid use'
        );

        // Get the current value for the operand
        var curAlloc = getBestAlloc(allocMap, use);
        var curVal = (curAlloc !== undefined)? curAlloc:use;

        assert (
            !(use instanceof IRInstr && curAlloc === undefined),
            'no allocation for live temporary: ' + use
        );

        if (excludeSet !== undefined)
            excludeSet = excludeSet.concat(instrOpnds);
        else
            excludeSet = instrOpnds;

        // Allocate the value to any register
        var reg = allocReg(
            allocMap,
            instrLiveIn,
            use,
            fixedReg,
            excludeSet
        );

        // Add the operand to the list
        instrOpnds.push(reg);

        // Move the use value into the register
        moveValue(allocMap, reg, curVal);
    }

    // TODO
    // codeGen.alloc_opnd_rm = function (use, excludeSet)
    // If the operand is a link-time value, it must be in a register
    //if (x86.isLinkValue(use) === true && opndMustBeReg === false)

    /**
    Allocate the destination to a specific register/memory operand
    */
    codeGen.alloc_dst = function (opnd)
    {
        // Free the operand, if needed
        freeOpnd(
            allocMap, 
            instrLiveIn, 
            opnd, 
            instrOpnds
        );

        // Map the instruction's output to the operand
        allocMap.makeAlloc(instr, opnd);

        instrDst = opnd;
    }

    /**
    Allocate the destination to an input operand
    */
    codeGen.alloc_dst_opnd = function (opndIdx)
    {
        assert (
            opndIdx < instr.uses.length,
            'invalid operand index'
        );

        var opnd = instrOpnds[opndIdx]

        codeGen.alloc_dst(opnd);
    }

    /**
    Get the allocation for an input operand
    */
    codeGen.get_opnd = function (opndIdx)
    {
        assert (
            opndIdx < instr.uses.length,
            'invalid operand index'
        );

        assert (
            instrOpnds.length === instr.uses.length,
            'do not have operands for all uses'
        );

        var opnd = instrOpnds[opndIdx];

        if (typeof opnd === 'number')
            return allocMap.getSlotOpnd(opnd);

        // Get the sub-operand of the appropriate size
        var size = instr.uses[opndIdx].type.getSizeBits(params);
        opnd = opnd.getSubOpnd(size);

        return opnd;
    }

    /**
    Get the allocation for the destination operand
    */
    codeGen.get_dest = function ()
    {
        assert (
            instr.type !== IRType.none,
            'cannot get dest, instr has none type'
        );

        var dst = instrDst;

        if (typeof dst === 'number')
            return allocMap.getSlotOpnd(dst);

        // Get the sub-operand of the appropriate size
        var size = instr.type.getSizeBits(params);
        dst = opnd.getSubOpnd(size);

        return dst;
    }

    // For each block in the ordering
    for (var i = 0; i < blockOrder.length; ++i)
    {
        var block = blockOrder[i];

        log.debug('processing block: ' + block.getBlockName());

        // Get a copy of the allocation map at the start of this block
        var allocMap = allocMaps[block.blockId].copy();

        assert (
            allocMap instanceof x86.RegAllocMap,
            'invalid reg alloc map for: ' + block.getBlockName()
        );

        // Store the current allocation map on the code gen object
        codeGen.allocMap = allocMap;

        // For each predecessor of this block
        for (var j = 0; j < block.preds.length; ++j)
        {
            var pred = block.preds[j];

            // If this predecessor has only one successor, skip it
            if (pred.succs.length === 1)
                continue;

            /*
            // Insert the edge transition stub
            x86.insertEdgeTrans(
                asm,
                pred, 
                block,
                allocInfo.mergeMoves,
                blockLabels,
                edgeLabels,
                params
            );
            */
        }

        // Add the label for this block
        asm.addInstr(blockLabels[block.blockId]);

        // For each instruction in the block
        for (var j = 0; j < block.instrs.length; ++j)
        {
            var instr = block.instrs[j];

            log.debug('processing: ' + instr);

            // Get the live set at the output of this instruction
            var instrLiveIn = liveness.instrIn[instr.instrId];

            assert (
                instr instanceof PhiInstr || instrLiveIn instanceof HashMap,
                'invalid live map for: ' + instr.getValName()
            );

            assert (
                instr.x86_genCode !== undefined,
                'no genCode method for:\n' + instr
            );

            // Clear the instruction input and destination operands
            instrOpnds.length = 0;
            instrDst = undefined;

            // Generate code for the instruction
            instr.x86_genCode(
                instr,
                asm,
                codeGen
            );
        }

        // If this is a block with a single successor, insert the 
        // transition stub directly after it
        if (block.succs.length === 1)
        {
            /*
            x86.insertEdgeTrans(
                asm, 
                block, 
                block.succs[0],
                allocInfo.mergeMoves,
                blockLabels,
                edgeLabels,
                params
            );
            */
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
TODO: function to implement block edge transitions
Should these be methods of some class?
*/
x86.genEdgeTrans = function ()
{
    // TODO
    // TODO
    // TODO




}

/**
@class Register allocation map. Maps temporaries and constants to
registers and stack locations at a given point within a function.
*/
x86.RegAllocMap = function (slotSize, spReg)
{
    assert (
        slotSize === 4 || slotSize === 8,
        'invalid slot size'
    );

    assert (
        spReg instanceof x86.Register && spReg.type === 'gp',
        'invalid stack pointer'
    );

    /**
    @field Stack frame slot size, in bytes
    */
    this.slotSize = slotSize;

    /**
    @field Stack pointer register to be used
    */
    this.spReg = spReg;

    /**
    @field Map of arguments to slot indices
    */
    this.argMap = [];

    /**
    @field Number of argument slots
    */
    this.numArgSlots = 0;

    /**
    @field Slot index of the return address
    */
    this.retAddrSlot = undefined;

    /**
    @field Number of spill slots currently used. The current size
    of the stack frame depends on this value.
    */
    this.numSpillSlots = 0;

    /**
    @field Map of register numbers to IR values
    */
    this.regMap = [];

    /**
    @field Map of stack slots to values
    */
    this.stackMap = new HashMap();

    /**
    @field Map of values to list of current allocations
    */
    this.allocMap = new HashMap();
}

/**
Copy an allocation map
*/
x86.RegAllocMap.prototype.copy = function ()
{
    // Create a new reg alloc map
    var newMap = new x86.RegAllocMap(this.slotSize, this.spReg);

    // Copy the map of arguments to slot indices
    newMap.argMap = this.argMap.slice(0);

    // Copy the number of argument slots
    newMap.numArgSlots = this.numArgSlots;

    // Copy the slot index of the return address
    newMap.retAddrSlot = this.retAddrSlot;

    // Copy the number of spill slots
    newMap.numSpillSlots = this.numSpillSlots;

    // Copy the GP register map
    newMap.regMap.length = this.regMap.length;
    for (var i = 0; i < this.regMap.length; ++i)
        newMap.regMap[i] = this.regMap[i];

    // Copy the stack slot map
    newMap.stackMap = this.stackMap.copy();

    // Copy the number of spills
    newMap.numSpills = this.numSpills;

    // Copy the alloc map
    for (var itr = this.allocMap.getItr(); itr.valid(); itr.next())
    {
        var item = itr.get();
        var val = item.key;
        var list = item.value;

        var newList = new Array(list.length);
        for (var i = 0; i < list.length; ++i)
            newList[i] = list[i];

        newMap.allocMap.setItem(val, newList);
    }

    // Return the copy
    return newMap;
}

/**
Produce a string representation of an allocation map
*/
x86.RegAllocMap.prototype.toString = function ()
{
    var str = '';

    str += 'GP registers:';

    for (var regNo = 0; regNo < this.regMap.length; ++regNo)
    {        
        var value = this.regMap[regNo];

        if (value === undefined)
            continue;

        str += '\nreg' + regNo + ' => ' + value.getValName();
    }

    return str;
}

/**
Allocate a stack location for the return address
*/
x86.RegAllocMap.prototype.allocArg = function (argIdx)
{
    assert (
        isNonNegInt(argIdx) && this.argMap[argIdx] === undefined,
        'invalid argument index: ' + argIdx
    );

    var slotIdx = this.numArgSlots;

    this.argMap[argIdx] = slotIdx;

    this.numArgSlots += 1;
}

/**
Allocate a stack location for the return address
*/
x86.RegAllocMap.prototype.allocRetAddr = function ()
{
    assert (
        this.retAddrSlot === undefined,
        'return address mapped on stack'
    );

    var slotIdx = this.numArgSlots;

    this.retAddrSlot = slotIdx;
}

/**
Allocate a new spill slot on the stack
*/
x86.RegAllocMap.prototype.allocSpill = function (asm)
{
    // Compute the slot index for the new slot
    var slotIdx = (this.numArgSlots + 1) + this.numSpillSlots;

    // Increment the number of spill slots
    this.numSpillSlots++;

    // Decrement the stack pointer
    asm.sub(this.spReg, this.slotSize);
    
    return slotIdx;
}

/**
Get the stack slot for a given argument
*/
x86.RegAllocMap.prototype.getArgSlot = function (argIdx)
{
    assert (
        argIdx < this.argMap.length,
        'invalid argument index'
    );

    return this.argMap[argIdx];
}

/**
Get a memory operand from a stack slot index. The number of spill slots
is used to compute the offset from the current stack pointer.
*/
x86.RegAllocMap.prototype.getSlotOpnd = function (slotIdx)
{
    // Compute the total number of stack slots
    var numSlots = this.numArgSlots + 1 + this.numSpillSlots;

    assert (
        slotIdx < numSlots,
        'invalid stack slot index: ' + slotIdx + ', num slots:' + numSlots
    );
    
    // Compute the offset relative to the stack pointer
    var offset = (slotIdx + 1 - numSlots) * this.slotSize;

    // Return the memory location
    return new x86.MemLoc(
        this.slotSize * 8,
        this.spReg,
        offset
    );
}

/**
Allocate a value to a register or spill slot
*/
x86.RegAllocMap.prototype.makeAlloc = function (value, alloc)
{
    assert (
        value === undefined ||
        value instanceof IRValue ||
        value instanceof x86.Register,
        'invalid value in allocReg: ' + value
    );

    assert (
        isNonNegInt(alloc) ||
        (alloc instanceof x86.Register && alloc.type === 'gp'),
        'invalid allocation: ' + alloc
    );

    var name;
    if (value === undefined)
        valName = undefined;
    else if (value.getValName !== undefined)
        valName = value.getValName();
    else
        valName = value.toString();
    log.debug(alloc + ' -> ' + valName);

    // If the allocation is to a register
    if (alloc instanceof x86.Register)
    {
        // Get the value the register previously mapped to
        var prevRegVal = this.regMap[alloc.regNo];

        // Remove the allocation of the previous value to the register
        if (prevRegVal !== undefined)
            this.remAlloc(prevRegVal, alloc);

        // Update the register's value
        this.regMap[alloc.regNo] = value;
    }
    else
    {
        // Get the value the stack slot previously mapped to
        var prevSlotVal = this.stackMap[alloc];

        // Remove the allocation of the previous value to the spill slot
        if (prevSlotVal !== undefined)
            this.remAlloc(prevSlotVal, alloc);

        // Update the stack slot's value
        this.stackMap.setItem(alloc, value);
    }

    // Get the alloc set for this value
    var allocSet;
    if (this.allocMap.hasItem(value) === false)
    {
        var allocSet = [];
        this.allocMap.addItem(value, allocSet);
    }
    else
    {
        allocSet = this.allocMap.getItem(value);
    }

    // Add the new allocation to the set
    arraySetAdd(allocSet, alloc);
}

/**
Remove a value from the allocation map
*/
x86.RegAllocMap.prototype.remAlloc = function (value, alloc)
{
    assert (
        value instanceof IRValue ||
        value instanceof x86.Register,
        'invalid value in remAlloc: ' + value
    );

    if (this.allocMap.hasItem(value) === false)
        return;

    var allocSet = this.allocMap.getItem(value);

    assert (
        arraySetHas(allocSet, alloc),
        'allocation not in set: ' + alloc
    );

    arraySetRem(allocSet, alloc);

    if (alloc instanceof x86.Register)
        this.regMap[alloc.regNo] = undefined;
    else
        this.stackMap.setItem(alloc, undefined);
}

/**
Remove all allocations for a value from the allocation map
*/
x86.RegAllocMap.prototype.remAllocs = function (value)
{
    assert (
        value instanceof IRValue ||
        value instanceof x86.Register,
        'invalid value in remAllocs: ' + value
    );

    if (this.allocMap.hasItem(value) === false)
        return;

    var allocSet = this.allocMap.getItem(value);

    for (var i = 0; i < allocSet.length; ++i)
    {
        var alloc = allocSet[i];

        if (alloc instanceof x86.Register)
            this.regMap[alloc.regNo] = undefined;
        else
            this.stackMap.setItem(alloc, undefined);
    }

    allocSet.length = 0;
}

/**
Spill a value on the stack.
*/
x86.RegAllocMap.prototype.spillValue = function (value, liveSet, asm)
{
    var stackSlot = undefined;

    // For each stack slot
    for (var itr = this.stackMap.getItr(); itr.valid(); itr.next())
    {
        var itrVal = itr.get();
        var alloc = itr.key;
        var curVal = itr.value;

        // If this spill slot is free, or assigned to a dead value
        if (curVal === undefined || 
            (liveSet.hasItem(curVal) === false && 
             (curVal instanceof x86.Register) === false))
        {
            stackSlot = alloc;
            break;
        }
    }

    // If no free stack slot was found
    if (stackSlot === undefined)
    {
        log.debug('Creating new stack slot');

        // Get a new spill slot index
        stackSlot = this.allocSpill(asm);
    }

    // Allocate the value to the stack slot
    this.makeAlloc(value, stackSlot);

    return stackSlot;
}

/**
Get the value a register maps to
*/
x86.RegAllocMap.prototype.getRegVal = function (reg)
{
    assert (
        reg instanceof x86.Register && reg.type === 'gp',
        'invalid register'
    );

    return this.regMap[reg.regNo];
}

/**
Get the value an allocation maps to
*/
x86.RegAllocMap.prototype.getAllocVal = function (alloc)
{
    if (alloc instanceof x86.Register && alloc.type === 'gp')
    {
        return this.regMap[alloc.regNo];
    }
    else if (typeof alloc === 'number')
    {
        if (this.stackMap.hasItem(alloc) === true)
            return this.stackMap.getItem(alloc);

        return undefined;
    }

    error('invalid allocation: ' + alloc);
}

/**
Get the allocation set for a value.
*/
x86.RegAllocMap.prototype.getAllocs = function (value)
{
    assert (
        value instanceof IRValue,
        'invalid value in getAlloc: ' + value
    );

    if (this.allocMap.hasItem(value) === true)
        return this.allocMap.getItem(value);
    else
        return [];
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

