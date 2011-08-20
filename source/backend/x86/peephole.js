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
x86 peephole optimizer implementation.

@author
Maxime Chevalier-Boisvert
*/

/**
x86 namespace
*/
var x86 = x86 || {};

/*
TODO: peephole optimizer

neg

lea opts for mul
  r0 := r1*{0,1} + r2*{0,1,2,4,8}

lea for 3-register addition
  lea eax, [ebx+ecx]

move sequence reduction
- see phi block merges
- much performance to gain here

Maybe:
inc/dec
- smaller, but may not be faster!

movcc, move on condition
- may not actually be faster! to be benchmarked

xadd, exchange and add

cmpxchg weirdness
*/

/**
Optimize a sequence of x86 instructions using peephole patterns
*/
x86.optimize = function (asm, maxPasses)
{
    // Get a reference to the instructions namespace
    const instrs = x86.instrs;

    // Get the x86_64 flag
    const x86_64 = asm.x86_64;

    /**
    Remove an instruction
    */
    function remInstr(instr)
    {
        asm.remInstr(instr);
        changed = true;
    }

    /**
    Add an instruction after another one
    */
    function addAfter(newInstr, prev)
    {
        asm.addInstrAfter(newInstr, prev);
        changed = true;
    }

    /**
    Replace an instruction
    */
    function replInstr(oldInstr, newInstr)
    {
        asm.replInstr(oldInstr, newInstr);
        changed = true;
    }

    // TODO: complete this table
    // Table of conditional jump instructions and their logical inverses
    const jumpInvs = {
        'je': 'jne',
        'jg': 'jle',
        'jge': 'jl',
        'jl': 'jge',
        'jle': 'jg',
        'jne': 'je'
    }

    /**
    Test if an instruction is an unconditional jump
    */
    function isDirectJump(instr)
    {
        return instr instanceof instrs.jmp;
    }

    /**
    Test if an instruction is a conditional jump
    */
    function isCondJump(instr)
    {
        return instr.mnem in jumpInvs;
    }

    /**
    Get the name for the logical inverse of a conditional jump instruction
    */
    function invCondJump(instr)
    {
        var invMnem = jumpInvs[instr.mnem];
        return instrs[invMnem];
    }

    /**
    Get the label for a jump
    */
    function getJumpLabel(instr)
    {
        return instr.opnds[0].label;
    }

    // Until no change occurred
    for (var pass = 1; (maxPasses === undefined || pass <= maxPasses); ++pass)
    {
        print(asm);

        // Flag to indicate a change occurred
        var changed = false;

        // For each instruction
        for (var instr = asm.getFirstInstr(); instr !== null; instr = instr.next)
        {
            // If this is a jump to a label
            if (instr.opnds[0] instanceof x86.LabelRef)
            {
                // Increment the reference count for the label
                var label = instr.opnds[0].label;
                label.refCount++;
            }
        }

        // For each instruction
        for (var instr = asm.getFirstInstr(); instr !== null; instr = instr.next)
        {
            //
            // TODO: ****
            // convert instr constructors to use arguments?
            // simplify assembler helper functions?
            //

            // If this is a jump
            if (isDirectJump(instr) === true ||
                isCondJump(instr) === true)
            {
                var label = getJumpLabel(instr);

                // If this is a sequence of instructions of the form:
                // jcc labelT
                // jmp labelF
                // labelT:
                // ...
                //
                // Replace it by:
                // jnc labelF:
                // labelT:
                // ..
                if (isCondJump(instr) === true &&
                    isDirectJump(instr.next) === true &&
                    label === instr.next.next)
                {
                    // Create the inverse logical jump
                    var invJmpCtor = invCondJump(instr);
                    var labelF = getJumpLabel(instr.next);
                    var newJmp = new invJmpCtor([new x86.LabelRef(labelF)], x86_64);               

                    // Remove the unconditional jump
                    remInstr(instr.next);

                    // Replace the conditional jump by its inverse
                    replInstr(instr, newJmp);
                }

                // If this is a jump to a jump
                else if (isDirectJump(label.next) === true)
                {
                    var j2Label = getJumpLabel(label.next);

                    // If the second jump label is not the same as ours
                    if (j2Label !== label)
                    {
                        // Jump directly to the second label
                        var ctor = instrs[instr.mnem];
                        var newJmp = new ctor([new x86.LabelRef(j2Label)], x86_64);
                        replInstr(instr, newJmp);
                    }
                }

                // If this is a jump to the next instruction
                else if (label === instr.next)
                {
                    // Remove the jump
                    remInstr(instr);
                }

                // If this is an unconditional jump and the next
                // instruction is not a label
                else if (isDirectJump(instr) === true &&
                         instr.next !== null &&
                         (instr.next instanceof x86.Label) === false)
                {
                    // Remove the instruction after the jump
                    remInstr(instr.next);
                }
            }

            // If this is a label
            else if (instr instanceof x86.Label)
            {
                // If the reference count is 0 and this label is
                // not exported, remove it
                if (instr.refCount === 0 && instr.export === false)
                    remInstr(instr);

                // Reset the reference count for the label
                instr.refCount = 0;
            }

            // If this is a move of 0 into a 32 or 64 bit register
            else if (instr instanceof instrs.mov &&
                     instr.opnds[0] instanceof x86.Register &&
                     instr.opnds[0].size >= 32 &&
                     instr.opnds[1] instanceof x86.Immediate &&
                     instr.opnds[1].value === 0)
            {
                // Replace the move by xor r, r
                var reg = instr.opnds[0];
                var newInstr = new instrs.xor([reg, reg], x86_64);
                replInstr(instr, newInstr);
            }

            // If this is an addition or subtraction
            else if (instr instanceof instrs.add ||
                     instr instanceof instrs.sub)
            {
                // If the second operand is immediate 0
                if (instr.opnds.length === 2 &&
                    instr.opnds[1] instanceof x86.Immediate &&
                    instr.opnds[1].value === 0)
                {
                    // Remove the instruction
                    remInstr(instr);
                }
            }

            // If this is a comparison between a register and 0
            else if (instr instanceof instrs.cmp &&
                     instr.opnds[0] instanceof x86.Register &&
                     instr.opnds[1] instanceof x86.Immediate &&
                     instr.opnds[1].value === 0)
            {
                var reg = instr.opnds[0];
                var testInstr = new instrs.test([reg, reg], x86_64);
                replInstr(instr, testInstr);
            }

            // If this is a return with zero bytes popped
            else if (instr instanceof instrs.ret &&
                     instr.opnds[0] instanceof x86.Immediate &&
                     instr.opnds[0].value === 0)
            {
                // Replace it by a ret with no immediate
                var newRet = new instrs.ret([], x86_64);
                replInstr(instr, newRet);
            }
        }

        // If no changes occurred, stop
        if (changed === false)
            break;
    }
};

