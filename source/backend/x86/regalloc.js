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
Indicates that an operand must be placed in a register
*/
x86.InstrCfg.prototype.opndMustBeReg = function (instr, opIdx, params)
{
    return false;
}

/**
Set of registers an operand can be assigned to. This
may be a single register.
*/
x86.InstrCfg.prototype.opndRegSet = function (instr, opIdx, params)
{
    return undefined;
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
Set of registers the destination can be assigned to.
This may be a single register.
*/
x86.InstrCfg.prototype.destRegSet = function (instr, params)
{
    return undefined;
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
    this.argStackMap = [];

    /**
    @field Location of the return address
    */
    this.retAddrLoc = undefined;

    /**
    @field Map of temporaries to stack location indices
    */
    this.valStackMap = [];

    /**
    @field Offsets for stack locations
    */
    this.locOffsets = [];
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

    for (var i = this.locList.length; i >= 0; --i)
    {
        curOffset += x86_64? 8:4;

        this.locOffsets[i] = curOffset;
    }
}

/**
Allocate a stack location for an argument
*/
x86.StackFrameMap.prototype.allocArg = function (argIdx)
{
    assert (
        this.argStackMap[argIdx] === undefined,
        'argument already mapped on stack'
    );

    var locIdx = this.locList.length;

    this.locList.push(['arg' + argIdx]);

    this.argStackMap[argIdx] = locIdx;
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
Get a stack location for a given value
*/
x86.StackFrameMap.prototype.getStackLoc = function (value)
{
    assert (
        value instanceof IRValue,
        'invalid value'
    );

    if (value instanceof ArgValInstr)
    {
        var argIdx = value.argIndex;

        // If a stack location is pre-mapped for this argument
        if (this.argStackMap[argIdx] !== undefined)
            return this.argStackMap[argIdx];
    }

    if (this.valStackMap[value.instrId] !== undefined)
    {
        return this.valStackMap[value.instrId];
    }

    var locIdx = this.locList.length;

    this.locList.push([value]);

    this.valStackMap[value.instrId] = locIdx;

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
        'invalid value'
    );

    assert (
        reg instanceof x86.Register && reg.type === 'gp',
        'invalid register'
    );

    print(reg + ' -> ' + value.getValName());

    this.gpRegMap[reg.regNo] = value;

    this.valRegMap.setItem(value, reg);
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
        'invalid stack location'
    );

    // Remove the previous register allocation for this value, if any
    this.remAlloc();

    print(stackLoc + ' -> ' + value.getValName());

    this.valStackMap.setItem(value, stackLoc);
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
        this.valStackMap.remItem(value);
    }

    return undefined;
}

/**
Remove a value from the allocation map
*/
x86.RegAllocMap.prototype.remAlloc = function (value)
{
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

    // Array to store live sets at the input of each block
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
        }

        // For each instruction, in reverse order
        for (var i = block.instrs.length - 1; i >= 0; --i)
        {
            var instr = block.instrs[i];

            // Store the live set at the output of this instruction
            instrLiveOut[instr.instrId] = liveCur.copy();

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

    // Stack pointer register
    var spReg = backend.x86_64? x86.regs.rsp:x86.regs.esp;

    // Maximum number of register operands
    const MAX_REG_OPNDS = gpRegSet.length - 2;

    /**
    Function to allocate a register
    @param allocMap allocation map
    @param moveList list into which to insert spill moves
    @param liveSet set of live temporaries
    @param value value to be allocated, may be null
    @regSet set of registers to allocate from, null if any
    @excludeSet set of registers to exclude from allocation
    */
    function allocReg(allocMap, moveList, liveOut, value, regSet, excludeSet)
    {
        // If no register set is specified, allocate from
        // the set of all available registers
        if (regSet === undefined)
            regSet = gpRegSet;

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
            if (arraySetHas(excludeSet, subReg) === true)
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
        var chosenReg;

        // If there are free registers, pick one
        if (freeRegs.length > 0)
        {
            chosenReg = freeRegs[0];
            //print('using free reg (' + chosenReg + ')');
        }

        // If some registers are used to store constants, pick one
        else if (cstRegs.length > 0)
        {
            chosenReg = cstRegs[0];
            //print('using cst reg (' + chosenReg + ')');
        }

        // If we could not find a free register
        if (chosenReg === undefined)
        {
            /*
            print('free regs: ' + freeRegs.length);
            print('cst regs : ' + cstRegs.length);
            print('live regs: ' + liveRegs.length);

            for (var i = 0; i < excludeSet.length; ++i)
                print(excludeSet[i]);
            */

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
                print('spilling reg: ' + chosenReg);

                // Get the stack location for the value
                var stackLoc = stackMap.getStackLoc(value);

                // If the value is not already on the stack
                if (allocMap.getValStack(regVal) !== stackLoc)
                {
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

        // Update the allocation map
        allocMap.allocReg(value, chosenReg);
        
        // If the register was allocated to a value
        if (value !== undefined)
        {
            // Get the sub-register corresponding to the value size
            chosenReg = chosenReg.getSubReg(value.type.getSizeBits(params));
        }

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
        print(dst + ' <== ' + srcStr);

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
    // TODO: account for hidden arguments
    var numArgs = irFunc.argVars.length + 2;

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

    // Map of instruction ids to instruction operands and pre-instruction moves
    var instrMap = [];

    // Map of block ids to maps from block ids to lists of merge moves
    var mergeMoves = [];

    // For each block in the ordering
    for (var i = 0; i < blockOrder.length; ++i)
    {
        var block = blockOrder[i];

        print('processing block: ' + block.getBlockName());

        // Get a copy of the allocation map at the start of this block
        var allocMap = allocMaps[block.blockId].copy();

        assert (
            allocMap instanceof x86.RegAllocMap,
            'invalid reg alloc map for: ' + block.getBlockName()
        );

        //print(allocMap + '\n');

        // For each instruction in the block
        for (var j = 0; j < block.instrs.length; ++j)
        {
            var instr = block.instrs[j];

            // Get the live set at the output of this instruction
            var liveOut = instrLiveOut[instr.instrId];

            assert (
                liveOut instanceof HashMap,
                'invalid live map for: ' + instr.getValName()
            );

            /*
            print('live set:');
            for (var itr = liveOut.getItr(); itr.valid(); itr.next())
                print(itr.get().key.getValName());
            */            

            print('processing: ' + instr);

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
                        [callConv.argRegs[argIndex]], 
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
                    [callConv.argCountReg],
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
                var excludeSet = [];

                // For each use of the instruction
                for (var k = 0; k < instr.uses.length; ++k)
                {
                    var use = instr.uses[k];

                    // If we can allocate no other operands to registers, stop
                    if (excludeSet >= MAX_REG_OPNDS)
                        break;

                    // Get the set of registers this operand can be in
                    var opndRegSet = instrCfg.opndRegSet(instr, k, params);

                    // Get the register mapping for the operand
                    var reg = allocMap.getValReg(use);

                    // If the operand is not in a register, ignore it
                    if (reg === undefined)
                        continue;

                    // If this operand is in the wrong register, ignore it
                    if (opndRegSet && !arraySetHas(opndRegSet, reg))
                        continue;

                    //print('pre-excluding: ' + reg);

                    // Add the register to the exclude set
                    arraySetAdd(excludeSet, reg);
                }

                // List of moves preceding the instruction
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

                // Get the allocation parameters for the destination
                var destIsOpnd0 = instrCfg.destIsOpnd0(instr, params);
                var destRegSet = instrCfg.destRegSet(instr, params);

                // For each use of the instruction
                for (var opndIdx = 0; opndIdx < instr.uses.length; ++opndIdx)
                {
                    var use = instr.uses[opndIdx];

                    // Get the register allocation parameters for this operand
                    var opndMustBeReg = instrCfg.opndMustBeReg(instr, opndIdx, params);
                    var opndRegSet = instrCfg.opndRegSet(instr, opndIdx, params);

                    // Test if this operand can be in memory
                    var opndMustBeReg = numMemOpnds >= maxMemOpnds || opndMustBeReg;

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
                    var opnd;

                    // Get the register or stack allocation
                    // allocated to this value, if any
                    var valAlloc = allocMap.getAlloc(use);

                    // If this value is already in a register
                    if (valAlloc instanceof x86.Register && 
                        numRegOpnds < MAX_REG_OPNDS)
                    {
                        print('opnd is reg: ' + valAlloc);

                        // If this is a valid register for this operand
                        if (opndRegSet === undefined ||
                            arraySetHas(opndRegSet, valAlloc) === true)
                        {
                            // Use the register operand
                            opnd = valAlloc;
                        }
                    }

                    // FIXME: make this more optimal
                    // If this value can be in memory and is already stack allocated
                    else if (typeof valAlloc === 'number' && 
                             opndMustBeReg === false)
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

                    //print('dest needs reg: ' + destNeedsReg);

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
                                opndRegSet,
                                excludeSet
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
                            valAlloc? valAlloc:use,
                            opnd
                        );
                    }

                    // If the operand is a register
                    if (opnd instanceof x86.Register)
                    {
                        // Map the value to the register
                        allocMap.allocReg(mapVal, opnd);

                        // Add the register to the exclude set
                        arraySetAdd(excludeSet, opnd);

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
                    print('allocating scratch');

                    // Allocate the scratch register
                    var reg = allocReg(
                        allocMap,
                        preMoves,
                        liveOut,
                        undefined,
                        undefined,
                        excludeSet
                    );
                    scratchRegs.push(reg);

                    // Add the register to the exclude set
                    arraySetAdd(excludeSet, reg);
                }

                // Get the set of registers this instruction will write to
                var writeRegs = instrCfg.writeRegSet(instr, params);

                // If this instruction writes to registers
                if (writeRegs !== undefined)
                {
                    // For each register this instruction writes to
                    for (var k = 0; k < writeRegs.length; ++k)
                    {
                        var reg = writeRegs[k];

                        print('freeing write reg');

                        // Allocate the register
                        allocReg(
                            allocMap,
                            preMoves,
                            liveOut,
                            undefined,
                            [reg],
                            excludeSet
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
                        destRegSet,
                        excludeSet
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

            // Get the allocation map for the successor
            var succAllocMap = allocMaps[succ.blockId];

            // If there is no alloc map for the successor
            if (succAllocMap === undefined)
            {
                // Use the current register allocation for the successor
                allocMaps[succ.blockId] = allocMap;

                // For each instruction of the successor
                for (var k = 0; k < succ.instrs.length; ++k)
                {
                    var instr = succ.instrs[k];

                    // If this is not a phi node, stop
                    if (!(instr instanceof PhiInstr))
                        break;

                    // Get the incoming value for this block
                    var inc = instr.getIncoming(block);

                    // Get the register and stack mapping of the incoming value
                    var reg = allocMap.getValReg(inc);
                    var stackLoc = allocMap.getValStack(inc);

                    assert (
                        reg !== undefined || stackLoc !== undefined,
                        'no register/stack mapping for phi operand:\n' +
                        inc
                    );

                    // Use the register and stack mapping of the
                    // incoming value for the phi node
                    if (reg !== undefined)
                        allocMap.allocReg(instr, reg);
                    if (stackLoc !== undefined)
                        allocMap.allocStack(instr, stackLoc);
                }
            }

            // There is already a register allocation for the successor
            else
            {
                // Get the live set at the beginning of the successor
                var succLiveIn = liveIn[succ.blockId];

                /* 
                TODO: each phi node has a register/loc

                Need to move the opnd corresponding to the phi input into the
                phi node's assigned register/location.

                This should be done before other moves occur, because some
                operands may no longer be live after this and their
                registers may be cleared.
                */

                /*
                Want to avoid spills and redundant moves. Use xchg to swap
                between live registers.                

                TODO: Iterate through live values. 

                If value is in reg, goes to reg, either move it there
                (if dst free), or xchg it with target reg. Then
                need to update the current alloc map to reflect the change.

                If opnd is reg, goes to mem, just move it.

                If opnd is mem or imm, goes to reg, need reg to be free.
                Probably want to handle these operand types in a second pass.

                PROBLEM: what about imm opnds on stack? We should have liveness
                info for these values as well. Can place them on target stack
                if needed.

                TODO: operands that die at a phi node need no location of
                their own
                */











            }
        }
    }

    // Compute the stack location offsets
    stackMap.compOffsets(backend.x86_64);

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

            //print(instr);

            // For each operand
            for (var k = 0; k < alloc.opnds.length; ++k)
            {
                // Remap the operand if needed
                if (typeof alloc.opnds[k] === 'number')
                {   
                    //print(instr.uses[k]);

                    alloc.opnds[k] = new x86.MemLoc(
                        instr.uses[k].type.getSizeBits(params),
                        spReg,
                        stackMap.getLocOffset(alloc.opnds[k])
                    );
                }
            }

            // Remap the dest if needed
            if (typeof alloc.dest === 'number')
            {
                alloc.dest = new x86.MemLoc(
                    instr.type.getSizeBits(params),
                    spReg, 
                    stackMap.getLocOffset(alloc.dest)
                );
            }

            // For each pre-instruction move
            for (var k = 0; k < alloc.preMoves.length; ++k)
            {
                var move = alloc.preMoves[k];

                // Remap the src if needed
                if (typeof move.src === 'number')
                {
                    move.src = new x86.MemLoc(
                        backend.x86_64? 64:32,
                        spReg, 
                        stackMap.getLocOffset(move.src)
                    );
                }

                // Remap the dst if needed
                if (typeof move.dst === 'number')
                {
                    move.dst = new x86.MemLoc(
                        backend.x86_64? 64:32,
                        spReg,
                        stackMap.getLocOffset(move.dst)
                    );
                }
            }
        }
    }

    //
    // TODO: pass over block moves
    //

    // Return the register allocation info
    return {
        instrMap: instrMap,
        mergeMoves: mergeMoves
    };
}

