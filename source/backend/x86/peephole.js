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

Assembler.addPattern(patternFunc?) ?
Need this to be somewhat more optimized...? pattern should have a
start instruction? Map patterns based on start instructions?

Can always start simple, build on
Assembler.optimize() function... Hardcoded patterns to start with.
peephole.js? optimizer.js?

label, jump elimination

neg

lea opts for mul
  r0 := r1*{0,1} + r2*{0,1,2,4,8}

lea for 3-register addition
  lea eax, [ebx+ecx]

cmp 0 to test opt
- replace test with and?

mov r, 0 to xor r,r

move sequence reduction

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

    /**
    Remove an instruction
    */
    function remInstr(instr)
    {
        asm.remInstr(instr);

        changed = true;
    }

    /**
    Replace an instruction
    */
    function replInstr(oldInstr, newInstr)
    {
        // TODO


        changed = true;
    }

    // Until no change occurred
    for (var pass = 1; (maxPasses === undefined || pass <= maxPasses); ++pass)
    {
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
            // TODO


            /*
            TODO:
            Jump condition inversion:

            je entry_cmp_true (8);                  74 05
            jmp entry_if_false_1 (16);              E9 12 02 00 00
            entry_cmp_true:                         
            */



            // TODO: remove instrs after jump...



            /*
            TODO: fix code gen

            cmp ebx, 8;                             83 FB 08
            jl cmp_true_if_true (8);                7C 02
            jmp cmp_true_if_false (8);              EB 02
            cmp_true_if_true:                       
            jmp if_true (8);                        EB 02
            cmp_true_if_false:                      
            jmp if_false (8);                       EB 08
            if_true:                                
            */

            // If this is a jump
            if (instr instanceof instrs.jmp ||
                instr instanceof instrs.je ||
                instr instanceof instrs.jl)
            {
                var label = instr.opnds[0].label;

                // If this is a jump to a jump
                if (label.next instanceof instrs.jmp)
                {
                    var j2 = label.next;
                    var j2Label = j2.opnds[0].label;

                    // Jump directly to the second label
                    instr.opnds[0] = new x86.LabelRef(j2Label);

                    changed = true;
                }

                // If this is a jump to the next instruction
                else if (label === instr.next)
                {
                    // Remove the jump
                    remInstr(instr);
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
        }

        // If no changes occurred, stop
        if (changed === false)
            break;
    }
};

