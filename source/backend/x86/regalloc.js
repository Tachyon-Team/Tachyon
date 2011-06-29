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
    return null;
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
    return null;
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
    return null;
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

    // Function to copy an allocation map
    function copyAllocMap(map)
    {
        // TODO
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

        // For each instruction in the block
        for (var j = 0; j < block.instrs.length; ++j)
        {
            var instr = block.instrs[j];

            // If this is a phi node
            if (instr instanceof PhiInstr)
            {




            }

            // If this is an argument value instruction
            else if (instr instanceof ArgValInstr)
            {



            }

            // For all other kinds of instructions
            else
            {
                /*
                x86.RegAllocCfg.prototype.maxMemOpnds = function (instr)
                x86.RegAllocCfg.prototype.numScratchRegs = function (instr)
                x86.RegAllocCfg.prototype.writeRegSet = function (instr)
                x86.RegAllocCfg.prototype.opndMustBeReg = function (instr, opIdx)
                x86.RegAllocCfg.prototype.opndRegSet = function (instr, opIdx)
                x86.RegAllocCfg.prototype.destIsOpnd0 = function (instr)
                x86.RegAllocCfg.prototype.destRegSet = function (instr)
                */

                assert (
                    instr.x86 !== undefined &&
                    instr.x86.regAllocCfg !== undefined,
                    'missing reg alloc cfg for "' + instr.mnemonic + '"'
                );

                // Get the register allocation config for the instruction
                var allocCfg = instr.x86.regAllocCfg;

                // Get the number of scratch registers for this instruction
                var numScratchRegs = allocCfg.numScratchRegs(instr); 

                // TODO: allocate the scratch registers
                for (var k = 0; k < numScratchRegs; ++k)
                {
                    // TODO: implement allocReg function
                    // TODO: avoid spilling the instruction's operands!
                    //allocReg(allocMap, regSet, excludeSet)
                }

                // Get the max number of memory operands for the instruction
                var maxMemOpnds = allocCfg.maxMemOpnds(instr);

                // Get the set of registers this instruction will write to
                var writeRegs = allocCfg.writeRegSet(instr);

                // TODO: handle written registers
                if (writeRegs !== null)
                {
                    for (var k = 0; k < writeRegs.length; ++k)
                    {
                        // TODO: allocate rgsiters for the write regs
                        // TODO: avoid spilling the instruction's operands!
                        // TODO: avoid using the scratch regs
                        //allocReg(allocMap, regSet, excludeSet)
                    }
                }

                // For each use of the instruction
                for (var k = 0; k < instr.uses.length; ++k)
                {
                    var use = instr.uses[k];

                    // Get the allocation parameters for this operand
                    var opndMustBeReg = allocCfg.opndMustBeReg(instr, k);
                    var opndRegSet = allocCfg.opndRegSet(instr, k);



                    // TODO: allocate a register if needed, for now, KISS
                    // TODO: avoid spilling the instruction's other operands!
                    // TODO: avoid using the scratch regs or write regs
                    // allocOpnd(allocMap, use, opndMustBeReg, opndRegSet, excludeSet)






                }

                // Get the allocation parameters for the destination
                var destIsOpnd0 = allocCfg.destIsOpnd0(instr);
                var destRegSet = allocCfg.destRegSet(instr);

                //
                // TODO: handle instruction's dest
                //
            }
        }

        // For each successor of the block
        for (var j = 0; j < block.succs.length; ++j)
        {
            var succ = block.succs[j];

            // TODO: store a reg alloc map on the succ blocks if none already
            // present. Otherwise merge with succ.







        }
    }











}

