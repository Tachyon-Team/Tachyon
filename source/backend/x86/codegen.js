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


    Will need function to allocate multiple operands at once...
    such as alloc_r_rm, alloc_r_rmi

    This will require using an exclude set.

    Exclude set could be optional parameter to all primitive alloc functions.


    TODO: start simple, alloc all to registers...


    */




    /**
    Allocate an operand to any register
    */
    codeGen.alloc_r_opnd = function (use, excludeSet)
    {
        // TODO




    }

    // codeGen.alloc_rm_opnd = function (use, excludeSet)





    

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

            assert (
                instr.x86_genCode !== undefined,
                'no genCode method for:\n' + instr
            );

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
}













