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
Register allocation for x86 code generation.

@author
Maxime Chevalier-Boisvert
*/

/* TODO:

PROBLEM:
Moving values from one stack loc to another, we crush the previous
allocation. Need a stack location -> value map, just like the gp reg map
to keep track of this.

1. Refactor stack frame map
- Still need arg, ret val pos
- Still need offsets for given stack locs
- No longer need list of values per stack loc, just number of stack locs
- No longer need value->stack loc association
- Need number of spill slots
- StackMap.numSpills

2. Refactor allocMap for "multi-home" allocation
- Map of vals to all current locations
- getAlloc returns alloc list
- getRegAlloc to get reg alloc only, if mustBeReg?
- Need map of stack locs to values
  - Scan this map to get spill loc for temp
  - Need liveness info to do this
- AllocMap.getAlloc
- AllocMap.getRegAlloc
- AllocMap.allocReg(value, reg)
- AllocMap.allocStack(value, stackLoc)
- AllocMap.spillValue(value)
  - Find first avail spill loc, alloc value to it

3. Implement freeReg spill function
- Use this for writeRegs spilling, no allocation of value to a register

4. Refactor allocReg w/ weights

5. Refactor opnd alloc loop into nested function?
- Better for clarity

*/

/**
Stack pointer register in 32-bit mode
*/
x86.spReg32 = x86.regs.esp;

/**
Stack pointer register in 64-bit mode
*/
x86.spReg64 = x86.regs.rsp;

/**
Context register in 32-bit mode
*/
x86.ctxReg32 = x86.regs.ecx;

/**
Context register in 64-bit mode
*/
x86.ctxReg64 = x86.regs.rcx;

/**
@class Per-instruction register allocation hints/constraints
*/
x86.InstrCfg = function ()
{
}

/**
Maximum number of memory operands to use
*/
x86.InstrCfg.prototype.maxMemOpnds = function (instr, params)
{
    return 1;
}

/**
Maximum number of immediate operands to use
*/
x86.InstrCfg.prototype.maxImmOpnds = function (instr, params)
{
    return 1;
}

/**
Number of scratch registers wanted
*/
x86.InstrCfg.prototype.numScratchRegs = function (instr, params)
{
    return 0;
}

/**
Registers this instruction will have to write to, excluding
scratch registers, operands and the destination.
*/
x86.InstrCfg.prototype.writeRegSet = function (instr, params)
{
    return undefined;
}

/**
Indicates that an operand can be an immediate
*/
x86.InstrCfg.prototype.opndCanBeImm = function (instr, opIdx, immSize, params)
{
    return false;
}

/**
Indicates that an operand must be placed in a register. A specific
register may be returned, otherwise, the value true specifies any
register.
*/
x86.InstrCfg.prototype.opndMustBeReg = function (instr, opIdx, params)
{
    return false;
}

/**
Dest is operand 0.
*/
x86.InstrCfg.prototype.destIsOpnd0 = function (instr, params)
{
    if (instr.uses.length === 0 || instr.type === IRType.none)
        return false;

    return true;
}

/**
Indicates that the destination must be placed in a register. A
specific register may be returned, otherwise, the value true specifies
any register.
*/
x86.InstrCfg.prototype.destMustBeReg = function (instr, params)
{
    return false;
}

/**
@class Stack frame map. Maps temporaries to stack frame locations.
*/
x86.StackFrameMap = function ()
{
    /**
    @field List of assigned temporaries for each stack locations
    */
    this.locList = [];

    /**
    @field Map of arguments to stack location indices
    */
    this.argMap = [];

    /**
    @field Location of the return address
    */
    this.retAddrLoc = undefined;

    /**
    @field Map of callee-save registers to stack indices
    */
    this.saveRegMap = {};

    /**
    @field Map of temporaries to stack location indices
    */
    this.valMap = [];

    /**
    @field Offsets for stack locations
    */
    this.locOffsets = [];

    /**
    @field Size of the portion of the stack frame used for spills
    */
    this.spillSize = undefined;
}

/**
Produce a string representation of the stack frame map
*/
x86.StackFrameMap.prototype.toString = function ()
{
    var str = '';

    // TODO

    return str;
}

/**
Compute the offsets for stack locations
*/
x86.StackFrameMap.prototype.compOffsets = function (x86_64)
{
    assert (
        typeof x86_64 === 'boolean',
        'invalid x86_64 flag'
    );

    this.locOffsets.length = this.locList.length;

    var curOffset = 0;

    var spillSize;

    // For each stack frame location, in reverse order
    for (var i = this.locList.length - 1; i >= 0; --i)
    {
        if (i === this.retAddrLoc)
            spillSize = curOffset;

        this.locOffsets[i] = curOffset;

        curOffset += x86_64? 8:4;
    }

    assert (
        spillSize !== undefined,
        'invalid spill size'
    );

    this.spillSize = spillSize;
}

/**
Allocate a stack location for an argument
*/
x86.StackFrameMap.prototype.allocArg = function (argIdx)
{
    assert (
        this.argMap[argIdx] === undefined,
        'argument already mapped on stack'
    );

    var locIdx = this.locList.length;

    this.locList.push(['arg' + argIdx]);

    this.argMap[argIdx] = locIdx;
}

/**
Allocate a stack location for the return address
*/
x86.StackFrameMap.prototype.allocRetAddr = function ()
{
    assert (
        this.retAddrLoc === undefined,
        'return address mapped on stack'
    );

    var locIdx = this.locList.length;

    this.locList.push(['ret_addr']);

    this.retAddrLoc = locIdx;
}

/**
Allocate a stack location for a callee-save register
*/
x86.StackFrameMap.prototype.allocSaveReg = function (reg)
{
    assert (
        reg instanceof x86.Register,
        'invalid register'
    );

    var locIdx = this.locList.length;

    this.locList.push([reg.name]);

    this.saveRegMap[reg.name] = locIdx;
}

/**
Get a stack location for a given value
*/
x86.StackFrameMap.prototype.getStackLoc = function (value)
{
    assert (
        value instanceof IRValue || value instanceof x86.Register,
        'invalid value in getStackLoc: ' + value
    );

    if (value instanceof x86.Register)
    {
        assert (
            this.saveRegMap[value.name] !== undefined,
            'no stack loc for callee-save register: ' + value
        );

        return this.saveRegMap[value.name];
    }

    if (value instanceof ArgValInstr)
    {
        var argIdx = value.argIndex;

        // If a stack location is pre-mapped for this argument
        if (this.argMap[argIdx] !== undefined)
            return this.argMap[argIdx];
    }

    if (this.valMap[value.instrId] !== undefined)
    {
        return this.valMap[value.instrId];
    }

    var locIdx = this.locList.length;

    this.locList.push([value]);

    this.valMap[value.instrId] = locIdx;

    return locIdx
}

/**
Get the offset for a stack location
*/
x86.StackFrameMap.prototype.getLocOffset = function (locIdx)
{
    assert (
        locIdx < this.locOffsets.length,
        'invalid stack loc idx: ' + locIdx
    );

    return this.locOffsets[locIdx]
}

/**
@class Register allocation map. Maps temporaries and constants to
registers and stack locations at a given point within a function.
*/
x86.RegAllocMap = function ()
{
    /**
    @field Map of GP register numbers to values
    */
    this.gpRegMap = [];

    /**
    @field Map of values to registers
    */
    this.valRegMap = new HashMap();

    /**
    @field Map of values to stack locations
    */
    this.valStackMap = new HashMap();
}

/**
Copy an allocation map
*/
x86.RegAllocMap.prototype.copy = function ()
{
    // Create a new object from the reg alloc map prototype
    var newMap = Object.create(x86.RegAllocMap.prototype);

    // Copy the GP register map
    newMap.gpRegMap = new Array(this.gpRegMap.length);
    for (var i = 0; i < this.gpRegMap.length; ++i)
        newMap.gpRegMap[i] = this.gpRegMap[i];

    // Copy the value->register map
    newMap.valRegMap = this.valRegMap.copy();

    // Copy the value->stack map
    newMap.valStackMap = this.valStackMap.copy();

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

    for (var regNo = 0; regNo < this.gpRegMap.length; ++regNo)
    {        
        var value = this.gpRegMap[regNo];

        if (value === undefined)
            continue;

        var reg = this.valRegMap.getItem(value);
        var regStr = reg.toString();

        str += '\n' + regStr + ' => ' + value.getValName();
    }

    return str;
}

/**
Allocate a value to a register
*/
x86.RegAllocMap.prototype.allocReg = function (value, reg)
{
    assert (
        value === undefined || value instanceof IRValue,
        'invalid value in allocReg: ' + value
    );

    assert (
        reg instanceof x86.Register && reg.type === 'gp',
        'invalid register'
    );

    log.debug(reg + ' -> ' + (value? value.getValName():undefined));

    var prevRegVal = this.gpRegMap[reg.regNo];

    if (prevRegVal !== undefined)
    {
        //print('unmapping prev reg val: ' + prevRegVal + ' (' + reg + ')');
        this.valRegMap.remItem(prevRegVal);
    }

    // Update the register's value
    this.gpRegMap[reg.regNo] = value;

    if (value !== undefined)
    {
        // If the value was in another register, unmap it from there
        if (this.valRegMap.hasItem(value) === true)
        {
            var prevReg = this.valRegMap.getItem(value);
            this.gpRegMap[prevReg.regNo] = undefined;
        }

        this.valRegMap.setItem(value, reg);
    }
}

/**
Allocate a value to a stack location
*/
x86.RegAllocMap.prototype.allocStack = function (value, stackLoc)
{
    assert (
        value === undefined || value instanceof IRValue,
        'invalid value'
    );

    assert (
        isNonNegInt(stackLoc),
        'invalid stack location: ' + stackLoc
    );

    // Remove the previous register allocation for this value, if any
    this.remAlloc(value);

    log.debug(stackLoc + ' -> ' + (value? value.getValName():undefined));

    if (value !== undefined)
    {
        this.valStackMap.setItem(value, stackLoc);
    }
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

    return this.gpRegMap[reg.regNo];
}

/**
Get the register a value is allocated to, if any
*/
x86.RegAllocMap.prototype.getValReg = function (value)
{
    assert (
        value instanceof IRValue,
        'invalid value'
    );

    if (this.valRegMap.hasItem(value) === true)
        return this.valRegMap.getItem(value);

    return undefined;
}

/**
Get the stack location a value is allocated to, if any
*/
x86.RegAllocMap.prototype.getValStack = function (value)
{
    assert (
        value instanceof IRValue,
        'invalid value'
    );

    if (this.valStackMap.hasItem(value) === true)
        return this.valStackMap.getItem(value);

    return undefined;
}

/**
Get the location allocated for a value. Registers are preffered.
*/
x86.RegAllocMap.prototype.getAlloc = function (value)
{
    if (this.valRegMap.hasItem(value) === true)
    {
        return this.valRegMap.getItem(value);
    }

    if (this.valStackMap.hasItem(value) === true)
    {
        return this.valStackMap.getItem(value);
    }

    return undefined;
}

/**
Remove a value from the allocation map
*/
x86.RegAllocMap.prototype.remAlloc = function (value)
{
    assert (
        value instanceof IRValue,
        'invalid value in remAlloc'
    );

    if (this.valRegMap.hasItem(value) === true)
    {
        var reg = this.valRegMap.getItem(value);

        this.gpRegMap[reg.regNo] = undefined;

        this.valRegMap.remItem(value);
    }

    if (this.valStackMap.hasItem(value) === true)
    {
        this.valStackMap.remItem(value);
    }
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
Perform register allocation on an IR function
*/
x86.allocRegs = function (irFunc, blockOrder, backend, params)
{
    //
    // Liveness analysis
    //

    // Work list for the analysis
    var workList = new LinkedList();

    // Add the blocks to the work list, in reverse order
    for (var i = blockOrder.length - 1; i >= 0; --i)
        workList.addLast(blockOrder[i]);

    // Array to store live sets at the input of each block, after the phi nodes
    var blockLiveIn = [];

    // Array of sets of live variables before instructions
    var instrLiveOut = [];

    // Until the work list is empty
    while (workList.isEmpty() === false)
    {
        var block = workList.remFirst();

        // Compute the union of the successor live in sets
        var liveCur = new HashMap();
        for (var i = 0; i < block.succs.length; ++i)
        {
            var succ = block.succs[i];

            var liveSucc = blockLiveIn[succ.blockId];
            if (liveSucc === undefined)
                continue;

            for (var itr = liveSucc.getItr(); itr.valid(); itr.next())
                liveCur.setItem(itr.get().key);

            // Add the phi inputs from this block to the live set
            for (var j = 0; j < succ.instrs.length; ++j)
            {
                var instr = succ.instrs[j];
                if (!(instr instanceof PhiInstr))
                    break;

                var inc = instr.getIncoming(block);
                liveCur.setItem(inc);
            }
        }

        // For each instruction, in reverse order
        for (var i = block.instrs.length - 1; i >= 0; --i)
        {
            var instr = block.instrs[i];

            // If this instruction is a phi node, stop
            if (instr instanceof PhiInstr)
                break;

            // Store the live set at the output of this instruction
            instrLiveOut[instr.instrId] = liveCur.copy();

            /*
            log.debug('instr live out: ' + instr);
            for (var itr = liveCur.getItr(); itr.valid(); itr.next())
                log.debug(itr.get().key.getValName());
            */

            // Remove the output of this instruction from the live set
            if (instr.dests.length > 0 && liveCur.hasItem(instr) === true)
                liveCur.remItem(instr);

            // For each use of the instruction
            for (var j = 0; j < instr.uses.length; ++j)
            {
                var use = instr.uses[j];

                // Map this use in the live set
                liveCur.setItem(use);
            }
        }

        // Remove the phi nodes from the live set
        for (var i = 0; i < i < block.instrs.length; ++i)
        {
            var instr = block.instrs[i];

            if (!(instr instanceof PhiInstr))
                break;

            if (liveCur.hasItem(instr) === true)
                liveCur.remItem(instr);
        }

        // Find the current live in set for this block
        var liveInCur = blockLiveIn[block.blockId];

        // If the new live set has more temps
        if (liveInCur === undefined || liveInCur.numItems !== liveCur.numItems)
        {
            // Replace the live in set for this block
            blockLiveIn[block.blockId] = liveCur;

            // Add all predecessors to the work list
            for (var i = 0; i < block.preds.length; ++i)
                workList.addLast(block.preds[i]);
        }
    }

    //
    // Register allocation
    //

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

    // Maximum number of register operands
    const MAX_REG_OPNDS = gpRegSet.length - 2;

    /**
    Function to allocate a register
    @param allocMap allocation map
    @param moveList list into which to insert spill moves
    @param liveSet set of live temporaries
    @param value value to be allocated, may be undefined
    @fixedReg fixed register we must allocate to, undefined
    @excludeMap map of registers to exclude from allocation
    */
    function allocReg(allocMap, moveList, liveOut, value, fixedReg, excludeMap)
    {
        // If no fixed register is specified, allocate from
        // the set of all available registers
        if (fixedReg === undefined)
            var regSet = gpRegSet;
        else
            var regSet = [fixedReg]





        /*
        TODO: freeReg spill function

        TODO: loop to find best register choice, unless fixed reg

        compute weight based on:
        - liveness
        - contains constant or temp
        - number of uses of temp

        var bestReg = null;
        var leastWeight = LOTS

        No more need to build lists of free regs, live regs, etc.!
        */





        // List of free registers
        var freeRegs = [];

        // List of registers holding live values
        var liveRegs = [];

        // List of registers holding constants
        var cstRegs = [];

        // For each register in the register set
        for (var i = 0; i < regSet.length; ++i)
        {
            var reg = regSet[i];

            // Get the sub-register corresponding to the value size
            var subReg;
            if (value !== undefined)
                subReg = reg.getSubReg(value.type.getSizeBits(params));
            else
                subReg = reg;
            
            // If a REX prefix is needed and we aren't in 64-bit, skip it
            if (subReg.rexNeeded === true && backend.x86_64 !== true)
                continue;

            // If this register is in the excluded set, skip it
            if (excludeMap[reg.regNo] === true)
                continue;

            // Get the value the register currently maps to
            var regVal = allocMap.getRegVal(reg);

            // If this register is already mapped to something
            if (regVal === undefined || liveOut.hasItem(regVal) === false)
            {
                freeRegs.push(reg);
            }
            else
            {
                if (regVal instanceof IRInstr)
                    liveRegs.push(reg);
                else
                    cstRegs.push(reg);
            }
        }

        // Chosen register
        var chosenReg = undefined;

        // If there are free registers, pick one
        if (freeRegs.length > 0)
        {
            chosenReg = freeRegs[0];
            //log.debug('using free reg (' + chosenReg + ')');
        }

        // If some registers are used to store constants, pick one
        else if (cstRegs.length > 0)
        {
            chosenReg = cstRegs[0];
            //log.debug('using cst reg (' + chosenReg + ')');
        }

        // If we could not find a free register
        if (chosenReg === undefined)
        {
            assert (
                liveRegs.length > 0,
                'no register found for allocation'
            );

            // Choose the first live register for spilling
            chosenReg = liveRegs[0];

            // Get the value the register currently maps to
            var regVal = allocMap.getRegVal(chosenReg);

            // If the register value is still live
            if (liveOut.hasItem(regVal) === true)
            {
                // Get the stack location for the value
                var stackLoc = stackMap.getStackLoc(regVal);

                // If the value is not already on the stack
                if (allocMap.getValStack(regVal) !== stackLoc)
                {
                    log.debug('spilling reg: ' + chosenReg + ' (' + regVal.getValName() + ')');

                    // Map the value to the stack location
                    allocMap.allocStack(regVal, stackLoc);

                    // Add a move for the spill
                    addMove(moveList, chosenReg, stackLoc);
                }
            }
        }

        assert (
            chosenReg !== undefined,
            'could not allocate register'
        );

        // If the register was allocated to a value
        if (value !== undefined)
        {
            // Get the sub-register corresponding to the value size
            chosenReg = chosenReg.getSubReg(value.type.getSizeBits(params));
        }

        // Update the allocation map
        allocMap.allocReg(value, chosenReg);

        // Return the allocated register
        return chosenReg;
    }

    /**
    Add a move to a list
    */
    function addMove(moveList, src, dst)
    {
        assert (
            moveList instanceof Array,
            'invalid move list'
        );

        assert (
            src instanceof x86.Register ||
            src instanceof ConstValue ||
            src instanceof IRFunction ||
            isNonNegInt(src),
            'invalid src for move: ' + src
        );

        assert (
            dst instanceof x86.Register ||
            isNonNegInt(dst),
            'invalid src for move: ' + dst
        );

        var srcStr = (src instanceof IRValue)? src.getValName():src.toString();
        log.debug(dst + ' <== ' + srcStr);

        moveList.push({ src:src, dst:dst });
    }

    // Create a stack frame map for the function
    // This maps stack locations, from top to bottom,
    // to temporaries assigned to the locations
    //
    // temporaries
    // -----------
    // return addr
    // -----------
    // arg vals

    // Create the stack frame map for this function. Maps
    // temporaries to stack frame locations, if the said
    // values are spilled.
    var stackMap = new x86.StackFrameMap();

    // Map of block ids to allocation map at block entries
    // This is used to store allocations at blocks with
    // multiple predecessors
    var allocMaps = [];

    // Get a reference to the entry block
    var entryBlock = irFunc.virginCFG.entry;

    // Create the register allocation map for the entry block
    // Maps temporaries and constants to register and stack locations
    // Note that constants will not be spilled on the stack
    var entryMap = new x86.RegAllocMap();

    // Map the entry block to its allocation map
    allocMaps[entryBlock.blockId] = entryMap;

    // Get the calling convention to be used with this function
    var callConv = backend.getCallConv(irFunc.cProxy? 'c':'tachyon');

    // Get the number of function arguments
    var numArgs = irFunc.argVars.length + (irFunc.cProxy? 0:2);

    // Map the arguments on the stack
    if (callConv.argsOrder === 'LTR')
    {
        // For each function argument, in left-to-right order
        for (var i = callConv.argRegs.length; i < numArgs; ++i)
            stackMap.allocArg(i);
    }
    else
    {
        // For each function argument, in left-to-right order
        for (var i = numArgs - 1; i >= callConv.argRegs.length; --i)
            stackMap.allocArg(i);
    }

    // Map the return address on the stack
    stackMap.allocRetAddr();

    // Map the callee-save registers on the stack
    for (var i = 0; i < callConv.calleeSave.length; ++i)
        stackMap.allocSaveReg(callConv.calleeSave[i]);

    // Map of instruction ids to instruction operands and pre-instruction moves
    var instrMap = [];

    // Map from CFG edges to lists of merge moves
    var mergeMoves = new CfgEdgeMap();

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

        //log.debug(allocMap + '\n');

        // For each instruction in the block
        for (var j = 0; j < block.instrs.length; ++j)
        {
            var instr = block.instrs[j];

            // Get the live set at the output of this instruction
            var liveOut = instrLiveOut[instr.instrId];

            assert (
                instr instanceof PhiInstr || liveOut instanceof HashMap,
                'invalid live map for: ' + instr.getValName()
            );

            /*
            log.debug('live set:');
            for (var itr = liveOut.getItr(); itr.valid(); itr.next())
                log.debug(itr.get().key.getValName());
            */            

            log.debug('processing: ' + instr);

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
                    allocReg(
                        allocMap,
                        undefined,
                        liveOut, 
                        instr, 
                        callConv.argRegs[argIndex],
                        []
                    );
                }
                else
                {
                    // Map the argument to its stack location
                    var stackLoc = stackMap.getStackLoc(instr);
                    allocMap.allocStack(instr, stackLoc);
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
                allocReg(
                    allocMap,
                    undefined,
                    liveOut, 
                    instr, 
                    callConv.argCountReg,
                    []
                );
            }

            // If this is the argument table instruction
            else if (instr instanceof GetArgTableInstr)
            {
                assert (
                    irFunc.usesArguments === true,
                    'arg table instr but function does not use arguments'
                );

                // Get a new stack location for the value
                var stackLoc = stackMap.getStackLoc(instr);

                // Map the value to the stack location
                allocMap.allocStack(instr, stackLoc);
            }

            // For all other kinds of instructions
            else
            {
                assert (
                    instr.x86 !== undefined &&
                    'missing reg alloc cfg for "' + instr.mnemonic + '"'
                );

                // Get the register allocation config for the instruction
                var instrCfg = instr.x86;

                // Set of registers not to be spilled
                var excludeMap = new Array(16);

                // For each use of the instruction
                for (var k = 0; k < instr.uses.length; ++k)
                {
                    var use = instr.uses[k];

                    // Get the set of registers this operand can be in
                    var opndReg = instrCfg.opndMustBeReg(instr, k, params);

                    // If this operand must be in a fixed register, reserve it
                    if (opndReg instanceof x86.Register)
                    {
                        excludeMap[opndReg.regNo] = true;
                        continue;
                    }

                    // Get the register mapping for the operand
                    var reg = allocMap.getValReg(use);

                    // If the operand is not in a register, ignore it
                    if (reg === undefined)
                        continue;

                    //log.debug('pre-excluding: ' + reg);

                    // Add the register to the exclude set
                    excludeMap[reg.regNo] = true;
                }

                // Get the allocation parameters for the destination
                var destIsOpnd0 = instrCfg.destIsOpnd0(instr, params);
                var destMustBeReg = instrCfg.destMustBeReg(instr, params);

                // If the destination is in a fixed register, exclude it
                // from allocation
                if (destMustBeReg instanceof x86.Register)
                    excludeMap[destMustBeReg.regNo] = true;

                // Get the set of registers this instruction will write to
                var writeRegs = instrCfg.writeRegSet(instr, params);

                // If this instruction writes to registers
                if (writeRegs !== undefined)
                {
                    assert (
                        writeRegs instanceof Array,
                        'write reg set is not an array'
                    );

                    // Add the write registers to the exclude set
                    for (var k = 0; k < writeRegs.length; ++k)
                        excludeMap[writeRegs[k].regNo] = true;
                }

                // List of moves to precede the instruction
                var preMoves = [];

                // Get the max number of memory operands for the instruction
                var maxMemOpnds = instrCfg.maxMemOpnds(instr, params);

                // Get the max number of immediate operands for the instruction
                var maxImmOpnds = instrCfg.maxImmOpnds(instr, params);

                // Number of register allocated operands
                var numRegOpnds = 0;

                // Number of memory allocated operands
                var numMemOpnds = 0;

                // Number of immediate operands used
                var numImmOpnds = 0;

                // List of operands
                var opnds = [];

                // For each use of the instruction
                for (var opndIdx = 0; opndIdx < instr.uses.length; ++opndIdx)
                {
                    var use = instr.uses[opndIdx];

                    // Get the register allocation parameters for this operand
                    var opndMustBeReg = instrCfg.opndMustBeReg(instr, opndIdx, params);

                    // Test if this operand can be in memory
                    if (opndMustBeReg === false && numMemOpnds >= maxMemOpnds)
                        opndMustBeReg = true;
 
                    // Test if this operand can be an immediate
                    var immSize = x86.getImmSize(use, params);
                    var opndCanBeImm = (immSize !== undefined);
                    if (opndCanBeImm === true)
                    {
                        opndCanBeImm = instrCfg.opndCanBeImm(
                            instr, 
                            opndIdx, 
                            immSize, 
                            params
                        );
                    }

                    // Operand assigned to this value
                    var opnd = undefined;

                    // Get the register or stack allocation
                    // allocated to this value, if any
                    var valAlloc = allocMap.getAlloc(use);

                    assert (
                        !(use instanceof IRInstr && valAlloc === undefined),
                        'no allocation for temporary: ' + use
                    );

                    //log.debug('use  : ' + use);
                    //log.debug('alloc: ' + valAlloc);

                    // If this value is already in a register
                    if (valAlloc instanceof x86.Register && numRegOpnds < MAX_REG_OPNDS)
                    {
                        // If this is a valid register for this operand
                        if (!(opndMustBeReg instanceof x86.Register && 
                              opndMustBeReg !== valAlloc))
                        {
                            // Use the register operand
                            opnd = valAlloc;
                        }
                    }

                    // FIXME: make this more optimal
                    // If this value can be in memory and is already stack allocated
                    else if (typeof valAlloc === 'number' && opndMustBeReg === false)
                    {
                        // Use the stack operand
                        opnd = valAlloc;
                    }

                    // If this operand can be an immediate and immediate 
                    // operands are still available
                    else if (opndCanBeImm === true &&
                             numImmOpnds < maxImmOpnds &&
                             (destIsOpnd0 === false || opndIdx !== 0))
                    {
                        // Use the immediate value for this operand
                        opnd = new x86.Immediate(use.getImmValue(params));
                    }

                    // If the dest is this operand and the use is 
                    // still live after this instruction
                    var destNeedsReg = destIsOpnd0 && opndIdx === 0 && liveOut.hasItem(use);

                    // Value for which this operand is being allocated
                    var mapVal = (destIsOpnd0 && opndIdx === 0)? instr:use;

                    //log.debug('dest needs reg: ' + destNeedsReg);

                    // If no operand was assigned or the dest needs
                    // its own register
                    if (opnd === undefined || destNeedsReg === true)
                    {
                        // If registers are still available
                        if (numRegOpnds < MAX_REG_OPNDS)
                        {
                            // Allocate a register for the operand
                            opnd = allocReg(
                                allocMap,
                                preMoves,
                                liveOut,
                                mapVal,
                                (opndMustBeReg instanceof x86.Register)?
                                opndMustBeReg:undefined,
                                (opndMustBeReg instanceof x86.Register)?
                                []:excludeMap
                            );
                        }
                        else
                        {
                            // Map the operand to its stack location
                            opnd = stackMap.getStackLoc(mapVal);
                        }

                        // Move the value into the operand
                        addMove(
                            preMoves,
                            (valAlloc !== undefined)? valAlloc:use,
                            opnd
                        );
                    }

                    // If the operand is a register
                    if (opnd instanceof x86.Register)
                    {
                        // Map the value to the register
                        allocMap.allocReg(mapVal, opnd);

                        // Add the register to the exclude set
                        excludeMap[opnd.regNo] = true;

                        ++numRegOpnds;
                    }

                    // If the operand is a stack location
                    else if (typeof opnd === 'number')
                    {
                        // Map the value to the stack location
                        allocMap.allocStack(mapVal, opnd);

                        ++numMemOpnds;
                    }

                    // If the operand is an immediate
                    else if (opnd instanceof x86.Immediate)
                    {
                        ++numImmOpnds;
                    }

                    // If the operand is not valid
                    else
                    {
                        error('invalid opnd: ' + opnd);
                    }

                    // Add the operand to the list
                    opnds.push(opnd);
                }

                // Get the number of scratch registers for this instruction
                var numScratchRegs = instrCfg.numScratchRegs(instr, params); 

                // List of scratch registers
                var scratchRegs = [];

                // For each scratch register to allocate
                for (var k = 0; k < numScratchRegs; ++k)
                {
                    log.debug('allocating scratch');

                    // Allocate the scratch register
                    var reg = allocReg(
                        allocMap,
                        preMoves,
                        liveOut,
                        undefined,
                        undefined,
                        excludeMap
                    );
                    scratchRegs.push(reg);

                    // Add the register to the exclude set
                    excludeMap[reg.regNo] = true;
                }

                // If this instruction writes to registers
                if (writeRegs !== undefined)
                {
                    assert (
                        writeRegs instanceof Array,
                        'write reg set is not an array'
                    );

                    // For each register this instruction writes to
                    for (var k = 0; k < writeRegs.length; ++k)
                    {
                        var reg = writeRegs[k];

                        log.debug('freeing write reg');

                        // Allocate the register
                        allocReg(
                            allocMap,
                            preMoves,
                            liveOut,
                            undefined,
                            reg,
                            []
                        );
                    }
                }

                assert (
                    !(destIsOpnd0 && opnds.length === 0),
                    'dest mapped to opnd0 but instr has no operands:\n' +
                    instr
                );

                // Destination operand
                var dest;

                // If this instruction has no output
                if (instr.type === IRType.none)
                {
                    dest = undefined;
                }

                // If the dest is not opnd 0
                else if (destIsOpnd0 === false)
                {
                    // Allocate a register for the destination
                    dest = allocReg(
                        allocMap,
                        preMoves,
                        liveOut,
                        instr,
                        (destMustBeReg instanceof x86.Register)?
                        destMustBeReg:undefined,
                        (destMustBeReg instanceof x86.Register)?
                        []:excludeMap
                    );
                }

                // If the dest is opnd 0
                else
                {
                    dest = opnds[0];
                }

                // Store the allocation info in the instruction map
                instrMap[instr.instrId] = {
                    preMoves: preMoves,
                    opnds: opnds,
                    scratchRegs: scratchRegs,
                    dest: dest
                };
            }
        }

        // For each successor of the block
        for (var j = 0; j < block.succs.length; ++j)
        {
            var succ = block.succs[j];

            // List of moves for this merge
            var moveList = [];

            // Map this edge to the move list
            mergeMoves.addItem({pred:block, succ:succ}, moveList);

            // Get the live set at the successor's entry, after the phi nodes
            var succLiveIn = blockLiveIn[succ.blockId];

            // Get the allocation map for the successor
            var succAllocMap = allocMaps[succ.blockId];

            // If there is no alloc map for the successor
            if (succAllocMap === undefined)
            {
                // Use the current register allocation for the successor
                var succAllocMap = allocMap.copy();
                allocMaps[succ.blockId] = succAllocMap;

                // For each instruction of the successor
                for (var k = 0; k < succ.instrs.length; ++k)
                {
                    var instr = succ.instrs[k];

                    // If this is not a phi node, stop
                    if (!(instr instanceof PhiInstr))
                        break;

                    // Get the incoming value for this block
                    var inc = instr.getIncoming(block);

                    // Get the allocation for the incoming value
                    var incAlloc = allocMap.getAlloc(inc);

                    assert (
                        !(incAlloc === undefined && inc instanceof IRInstr),
                        'no allocation for incoming phi temporary:\n' +
                        inc
                    );

                    // If this is an IR constant with no allocation
                    if (incAlloc === undefined)
                    {
                        // Allocate a register for the phi node
                        var reg = allocReg(
                            succAllocMap,
                            moveList,
                            succLiveIn,
                            instr,
                            undefined,
                            []
                        );

                        // Move the incoming value into the register
                        addMove(moveList, inc, reg);
                    }
                    else
                    {
                        // Use the register and stack mapping of the
                        // incoming value for the phi node
                        if (incAlloc instanceof x86.Register)
                            succAllocMap.allocReg(instr, incAlloc);
                        else
                            succAllocMap.allocStack(instr, incAlloc);
                    }
                }
            }

            // There is already a register allocation for the successor
            else
            {
                // Last phi node found
                var lastPhi = undefined;

                // For each instruction of the successor
                for (var insIdx = 0; insIdx < succ.instrs.length; ++insIdx)
                {
                    var instr = succ.instrs[insIdx];

                    // If this is not a phi node, stop
                    if (!(instr instanceof PhiInstr))
                        break;

                    lastPhi = instr;

                    // Get the location allocated to this phi node
                    var phiAlloc = succAllocMap.getAlloc(instr);

                    assert (
                        phiAlloc !== undefined,
                        'no allocation for phi node'
                    );

                    // Get the incoming value for this predecessor
                    var inc = instr.getIncoming(block);

                    // Get the location allocated to the incoming value
                    var incAlloc = allocMap.getAlloc(inc);

                    assert (
                        !(incAlloc === undefined && inc instanceof IRInstr),
                        'no allocation for incoming phi temporary:\n' +
                        inc
                    );

                    var srcValue = (incAlloc !== undefined)? incAlloc:inc;

                    // If the locations do not match, add a move
                    if (incAlloc !== phiAlloc)
                        addMove(moveList, srcValue, phiAlloc);
                }

                // For each value live after the phi nodes
                for (var itr = succLiveIn.getItr(); itr.valid(); itr.next())
                {
                    // Get the value
                    var value = itr.get().key;

                    // If the value is a phi node from this block, skip it
                    if (value instanceof PhiInstr && value.parentBlock === succ)
                        continue;

                    // Get the allocations for this value in both blocks
                    var predAlloc = allocMap.getAlloc(value);
                    var succAlloc = succAllocMap.getAlloc(value);

                    assert (
                        !(predAlloc === undefined && value instanceof IRInstr),
                        'no allocation for live temporary:\n' +
                        value + '\n' +
                        'in pred:\n' +
                        block.getBlockName()
                    );

                    assert (
                        !(succAlloc === undefined && value instanceof IRInstr),
                        'no allocation for live temporary:\n' +
                        value + '\n' +
                        'in succ:\n' +
                        succ.getBlockName()
                    );

                    var srcValue = (predAlloc !== undefined)? predAlloc:value;

                    // If the locations do not match, the value is stil
                    // live in the successor, and there is an allocation
                    // for the successor, insert a move
                    if (predAlloc !== succAlloc && 
                        succLiveIn.hasItem(value) === true &&
                        succAlloc !== undefined)
                        addMove(moveList, srcValue, succAlloc);
                }
            }
        }
    }

    // Compute the stack location offsets
    stackMap.compOffsets(backend.x86_64);

    /**
    Function to remap the memory operands of an abstract move
    */
    function remapMove(move)
    {
        // Compute the operand size
        var opndSize;
        if (move.src instanceof x86.Register)
            opndSize = move.src.size;
        else if (move.dst instanceof x86.Register)
            opndSize = move.dst.size;
        else
            opndSize = backend.regSizeBits;

        // Remap the src if needed
        if (typeof move.src === 'number')
        {
            move.src = new x86.MemLoc(
                opndSize,
                backend.spReg, 
                stackMap.getLocOffset(move.src)
            );
        }

        // Remap the dst if needed
        if (typeof move.dst === 'number')
        {
            move.dst = new x86.MemLoc(
                opndSize,
                backend.spReg,
                stackMap.getLocOffset(move.dst)
            );
        }
    }

    // For each block in the ordering
    for (var i = 0; i < blockOrder.length; ++i)
    {
        var block = blockOrder[i];

        // For each instruction in the block
        for (var j = 0; j < block.instrs.length; ++j)
        {
            var instr = block.instrs[j];

            var alloc = instrMap[instr.instrId];

            if (alloc === undefined)
                continue;

            //log.debug(instr);

            // For each operand
            for (var k = 0; k < alloc.opnds.length; ++k)
            {
                //log.debug('opnd: ' + alloc.opnds[k]);
                //log.debug(alloc.opnds[k] instanceof x86.Immediate);

                // Remap the operand if needed
                if (typeof alloc.opnds[k] === 'number')
                {   
                    //log.debug(instr.uses[k]);

                    alloc.opnds[k] = new x86.MemLoc(
                        instr.uses[k].type.getSizeBits(params),
                        backend.spReg,
                        stackMap.getLocOffset(alloc.opnds[k])
                    );
                }
            }

            // Remap the dest if needed
            if (typeof alloc.dest === 'number')
            {
                alloc.dest = new x86.MemLoc(
                    instr.type.getSizeBits(params),
                    backend.spReg, 
                    stackMap.getLocOffset(alloc.dest)
                );
            }

            // For each pre-instruction move
            for (var k = 0; k < alloc.preMoves.length; ++k)
                remapMove(alloc.preMoves[k]);
        }
    }

    // For each predecessor block in the merge move map
    for (var i = 0; i < blockOrder.length; ++i)
    {
        var pred = blockOrder[i];

        // For each successor
        for (var j = 0; j < pred.succs.length; ++j)
        {
            var succ = pred.succs[j];
            var edge = {pred:pred, succ:succ};

            // Get the moves for this predecessor->successor edge
            var moves = mergeMoves.getItem(edge);

            // For each merge move
            for (var k = 0; k < moves.length; ++k)
                remapMove(moves[k]);
        }
    }

    // Return the register allocation info
    return {
        stackMap: stackMap,
        instrMap: instrMap,
        mergeMoves: mergeMoves
    };
}

