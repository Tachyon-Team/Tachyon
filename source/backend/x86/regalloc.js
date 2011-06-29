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
@class Per-instruction register allocation hints/constraints
*/
x86.RegAllocCfg = function ()
{
}

/**
Maximum number of memory operands to use
*/
x86.RegAllocCfg.prototype.maxMemOpnds = function (instr)
{
    return 1;
}

/**
Maximum number of immediate operands to use
*/
x86.RegAllocCfg.prototype.maxImmOpnds = function (instr)
{
    return 1;
}

/**
Number of scratch registers wanted
*/
x86.RegAllocCfg.prototype.numScratchRegs = function (instr)
{
    return 0;
}

/**
Registers this instruction will have to write to, excluding
scratch registers, operands and the destination.
*/
x86.RegAllocCfg.prototype.writeRegSet = function (instr)
{
    return undefined;
}

/**
Indicates that an operand can be an immediate
*/
x86.RegAllocCfg.prototype.opndcanBeImm = function (instr, opIdx)
{
    return false;
}

/**
Indicates that an operand must be placed in a register
*/
x86.RegAllocCfg.prototype.opndMustBeReg = function (instr, opIdx)
{
    return false;
}

/**
Set of registers an operand can be assigned to. This
may be a single register.
*/
x86.RegAllocCfg.prototype.opndRegSet = function (instr, opIdx)
{
    return undefined;
}

/**
Dest is operand 0.
*/
x86.RegAllocCfg.prototype.destIsOpnd0 = function (instr)
{
    return true;
}

/**
Set of registers the destination can be assigned to.
This may be a single register.
*/
x86.RegAllocCfg.prototype.destRegSet = function (instr)
{
    return undefined;
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

    // Add the last block of the ordering to the work list
    workList.addLast(blockOrder[blockOrder.length-1]);

    // Array to store live sets at the input of each block
    var liveIn = [];

    // Until the work list is empty
    while (workList.isEmpty() === false)
    {
        var block = workList.remFirst();

        // Compute the union of the successor live in sets
        var liveCur = new HashMap();
        for (var i = 0; i < block.succs.length; ++i)
        {
            var succ = block.succs[i];

            var liveSucc = liveIn[succ.blockId];
            if (liveSucc === undefined)
                continue;

            for (var itr = liveSucc.getItr(); itr.valid(); itr.next())
            {
                var temp = itr.get();
                liveCur.setItem(temp.key, true);
            }
        }

        // For each instruction, in reverse order
        for (var i = block.instrs.length - 1; i >= 0; --i)
        {
            var instr = block.instrs[i];

            // Remove the output of this instruction from the live set
            if (instr.dests.length > 0 && liveCur.hasItem(instr) === true)
                liveCur.remItem(instr);

            // For each use of the instruction
            for (var j = 0; j < instr.uses.length; ++j)
            {
                var use = instr.uses[j];

                // If this use just became live, map it to
                // the point it became live in this block
                if (liveCur.hasItem(use) === false)
                    liveCur.setItem(use, instr);
            }
        }

        // Find the current live in set for this block
        var liveInCur = liveIn[block.blockId];

        // If the new live set has more temps
        if (liveInCur === undefined || liveInCur.numItems !== liveCur.numItems)
        {
            // Replace the live in set for this block
            liveIn[block.blockId] = liveCur;

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

    /**
    Function to copy an allocation map
    */
    function copyAllocMap(map)
    {
        // TODO
    }

    /**
    Function to allocate a register
    @param allocMap allocation map
    @param value value to be allocated, may be null
    @regSet set of registers to allocate from, null if any
    @excludeSet set of registers to exclude from allocation
    */
    function allocReg(allocMap, value, regSet, excludeSet)
    {
        /*
        TODO
        // List of assigned temporaries for stack locations
        var stackLocList = []

        // Map of temporaries to stack location indices
        var stackMap = [];

        // Map of in-use registers to values
        var gpRegMap = [];
        */

        // If no register set is specified, allocate from
        // the set of all available registers
        if (regSet === undefined)
            regSet = gpRegSet;

        // Chosen register
        var chosenReg;
        var chosenSubReg;

        // For each register in the register set
        for (var i = 0; i < regSet.length; ++i)
        {
            var reg = regSet[i];

            // If this register is in the excluded set, skip it
            if (arraySetHas(excludeSet, reg) === true)
                continue;

            // If this register is already mapped to something, skip it
            if (gpRegMap[reg.regNo] !== undefined)
                continue;

            // Get the sub-register corresponding to the value size
            var subReg;
            if (value !== undefined)
                subReg = reg.getSubReg(value.type.getSizeBits(params));
            else
                subReg = reg;
            
            // If a REX prefix is needed and we aren't in 64-bit, skip it
            if (subReg.rexNeeded === true && backend.x86_64 !== true)
                continue;

            // Choose this register
            chosenReg = reg;
            chosenSubReg = subReg;
            break;
        }

        // If we could not find a free register
        if (chosenReg === undefined)
        {
            //
            // TODO: spill logic
            //






        }


        //
        // TODO: update alloc map, stack maps
        //


        if (chosenReg)
        {
            print(chosenReg + ' ==> ' + value.toString().substr(0, 20) + '...');

            // Update the register map
            gpRegMap[chosenReg.regNo] = value;
        }
        else
        {
            print('oh noes, out of regs!');
        }

        // Return the allocated register
        return chosenSubReg;
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

    // List of assigned temporaries for stack locations
    var stackLocList = []

    // Map of temporaries to stack location indices
    var stackMap = [];

    // Map of in-use general-purpose registers to values
    var gpRegMap = [];

    // Map of block ids to allocation map at block entries
    // This is used to store allocations at blocks with
    // multiple predecessors
    var allocMaps = [];

    // Get a reference to the entry block
    var entryBlock = irFunc.virginCFG.entry;

    // Create the register allocation map for the entry block
    // Maps temporaries and constants to register and stack locations
    // Note that constants will not be spilled on the stack
    var entryMap = new HashMap();

    // Map the entry block to its allocation map
    allocMaps[entryBlock.blockId] = entryMap;

    // Get the calling convention to be used with this function
    var callConv = backend.getCallConv(irFunc.cProxy? 'c':'tachyon');

    // TODO: map the args in the stack map. Do this lazily?

    // TODO: map the first args and arg count in the alloc map
    // do this lazily on demand?

    //ArgValInstr argIndex
    //GetNumArgsInstr
    //GetArgTableInstr





    // If the function uses the arguments object
    if (irFunc.usesArguments === true)
    {
        // TODO: map a stack slot for the arg object



    }








    // TODO: store operands, dest in a big array, simply map them by id.
    // Or perhaps just generate the code right here?
    //
    // Need to insert moves into the code before some instructions.
    // where should we put these moves? Are they obvious from the opnd
    // assignment? No. spills are not obvious.
    var instrMap = [];






    // For each block in the ordering
    for (var i = 0; i < blockOrder.length; ++i)
    {
        var block = blockOrder[i];

        // Get the allocation map for this block
        var allocMap = allocMaps[block.blockId];



        // TODO
        if (!allocMap)
        {
            print('skipping block: ' + block.getBlockName());
            continue;
        }



        // For each instruction in the block
        for (var j = 0; j < block.instrs.length; ++j)
        {
            var instr = block.instrs[j];

            // If this is a phi node
            if (instr instanceof PhiInstr)
            {
                // TODO



            }

            // If this is an argument value instruction
            else if (instr instanceof ArgValInstr)
            {
                // TODO



            }

            // For all other kinds of instructions
            else
            {
                assert (
                    instr.x86 !== undefined &&
                    instr.x86.regAllocCfg !== undefined,
                    'missing reg alloc cfg for "' + instr.mnemonic + '"'
                );

                // Get the register allocation config for the instruction
                var allocCfg = instr.x86.regAllocCfg;

                // Set of registers not to be spilled
                var excludeSet = [];

                // For each use of the instruction
                for (var k = 0; k < instr.uses.length; ++k)
                {
                    var use = instr.uses[k];

                    // Get the set of registers this operand can be in
                    var opndRegSet = allocCfg.opndRegSet(instr, k);

                    // If this operand is not mapped to anything, ignore it
                    if (allocMap.hasItem(use) === false)
                        continue;

                    // Get the register mapping for the operand
                    var reg = allocMap.getItem(use).reg;

                    // If the operand is not in a register, ignore it
                    if (!reg)
                        continue;

                    // If this operand is in the wrong register, ignore it
                    if (opndRegSet && !arraySetHas(opndRegSet, reg))
                        continue;

                    // Add the register to the exclude set
                    excludeSet.push(reg);
                }

                // Get the max number of memory operands for the instruction
                var maxMemOpnds = allocCfg.maxMemOpnds(instr);


                // TODO: maxImmOpnds***


                // Number of memory allocated operands
                var numMemOpnds = 0;

                // List of operands
                var opnds = [];

                // For each use of the instruction
                for (var k = 0; k < instr.uses.length; ++k)
                {
                    var use = instr.uses[k];

                    // Get the allocation parameters for this operand
                    var opndMustBeReg = allocCfg.opndMustBeReg(instr, k);
                    var opndRegSet = allocCfg.opndRegSet(instr, k);
                    

                    // TODO: opndCanBeImm***


                    // Test if this operand can be in memory
                    var opndMustBeReg = numMemOpnds >= maxMemOpnds || opndMustBeReg;

                    // If this operand has a mapping
                    if (allocMap.hasItem(use) === true)
                    {
                        // Get the mapping for the operand
                        var alloc = allocMap.getItem(use);

                        // If this operand is already in a valid register
                        if (alloc.reg && (!opndRegSet || arraySetHas(opndRegSet, alloc.reg)))
                        {
                            // Use the register operand
                            opnds.push(alloc.reg);
                            continue;
                        }

                        // FIXME
                        // If this operand can be in memory and is already stack allocated
                        if (!opndMustBeReg && alloc.stack)
                        {
                            // Use the stack operand
                            opnds.push(alloc.stack);
                            ++numMemOpnds;
                            continue;
                        }
                    }

                    // FIXME: for now, try to get a register for all operands
                    // If this operand must be in a register
                    //if (opndMustBeReg)
                    {
                        // Allocate a register for the operand
                        var reg = allocReg(allocMap, use, opndRegSet, excludeSet);
                        opnds.push(reg);

                        // Add the register to the exclude set
                        excludeSet.push(reg);
                    }
                }

                // Get the number of scratch registers for this instruction
                var numScratchRegs = allocCfg.numScratchRegs(instr); 

                // List of scratch registers
                var scratchRegs = [];

                // For each scratch register to allocate
                for (var k = 0; k < numScratchRegs; ++k)
                {
                    // Allocate the scratch register
                    var reg = allocReg(allocMap, undefined, undefined, excludeSet);
                    scratchRegs.push(reg);

                    // Add the register to the exclude set
                    excludeSet.push(reg);
                }

                // Get the set of registers this instruction will write to
                var writeRegs = allocCfg.writeRegSet(instr);

                // If this instruction writes to registers
                if (writeRegs !== undefined)
                {
                    // For each register this instruction writes to
                    for (var k = 0; k < writeRegs.length; ++k)
                    {
                        var reg = writeRegs[k];

                        // Allocate the register
                        allocReg(allocMap, undefined, [reg], excludeSet);
                    }
                }

                // Get the allocation parameters for the destination
                var destIsOpnd0 = allocCfg.destIsOpnd0(instr);
                var destRegSet = allocCfg.destRegSet(instr);

                // Destination operand
                var dest;

                // If the destination must be operand 0
                if (destIsOpnd0 === true)
                {
                    // Use operand 0 for the destination
                    dest = opnds[0];
                }
                else
                {
                    // Allocate a register for the destination
                    dest = allocReg(allocMap, instr, destRegSet, excludeSet);
                }

                // Store the allocation info in the instruction map
                instrMap[instr.instrId] = {
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

            // TODO: store a reg alloc map on the succ blocks if none already
            // present. Otherwise merge with succ.


            // TODO: insert moves as appropriate




        }
    }


    // TODO: pass over operands, translate stack references to mem operands








}

