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
x86 backend inteface.

@author
Maxime Chevalier-Boisvert
*/

/**
Generate the assembly code for one function
*/
x86.irToASM = function (irFunc, blockOrder, allocInfo, backend, params)
{
    // Assembler object to create instructions into
    var asm = new x86.Assembler(backend.x86_64);


    // TODO
    // Callee pops stack frame & args... Ideally want stack frame
    // normalization stub.


    // For each block in the ordering
    for (var i = 0; i < blockOrder.length; ++i)
    {
        var block = blockOrder[i];

        // For each instruction in the block
        for (var j = 0; j < block.instrs.length; ++j)
        {
            var instr = block.instrs[j];

            // If this is a pseudo-instruction, generate no code for it
            if (instr instanceof ArgValInstr ||
                instr instanceof GetNumArgsInstr ||
                instr instanceof GetArgTableInstr ||
                instr instanceof PhiInstr)
                continue;
            
            assert (
                instr.x86.genCode !== undefined,
                'no genCode method for:\n' + instr
            );

            // Get the allocation info for this instruction
            var instrAlloc = allocInfo.instrMap[instr.instrId];

            //
            // TODO: handle pre-moves
            //

            // Generate code for the instruction
            instr.x86.genCode(
                asm,
                instrAlloc.opnds,
                instrAlloc.dest,
                instrAlloc.scratchRegs,
                allocInfo.blockMoves,
                backend,
                params
            );
        }
    }

    print('');
    print('assembly:')
    print(asm);

    // Return the assembler object
    return asm;
}

// TODO: for now, default reg alloc config to test register allocation
IRInstr.prototype.x86 = {};
IRInstr.prototype.x86.regAllocCfg = new x86.RegAllocCfg();

GetCtxInstr.prototype.x86 = {};
GetCtxInstr.prototype.x86.regAllocCfg = new x86.RegAllocCfg();
GetCtxInstr.prototype.x86.genCode = function (asm, opnds, dest, scratch, blockMoves, backend, params)
{
    // TODO
    asm.nop();
};

ArithInstr.prototype.x86 = {};
ArithInstr.prototype.x86.regAllocCfg = new x86.RegAllocCfg();
ArithInstr.prototype.x86.regAllocCfg.opndCanBeImm = function (instr, idx, size) { return (size <= 32); }

AddInstr.prototype.x86 = {};
AddInstr.prototype.x86.regAllocCfg = ArithInstr.prototype.x86.regAllocCfg;
AddInstr.prototype.x86.genCode = function (asm, opnds, dest, scratch, blockMoves, backend, params)
{
    asm.add(opnds[0], opnds[1]);
};

MulInstr.prototype.x86 = {};
MulInstr.prototype.x86.regAllocCfg = ArithInstr.prototype.x86.regAllocCfg;
MulInstr.prototype.x86.genCode = function (asm, opnds, dest, scratch, blockMoves, backend, params)
{
    // TODO: look at this in more detail, need to force opnd into specific reg
    // for mul, not necessarily for imul
    //asm.mul(opnds[0], opnds[1]);
    asm.nop();
};

CallFuncInstr.prototype.x86 = {};
CallFuncInstr.prototype.x86.regAllocCfg = new x86.RegAllocCfg();
CallFuncInstr.prototype.x86.regAllocCfg.maxImmOpnds = function (instr, idx, size) { return instr.uses.length; }
CallFuncInstr.prototype.x86.regAllocCfg.opndCanBeImm = function (instr, idx, size) { return true; }
CallFuncInstr.prototype.x86.genCode = function (asm, opnds, dest, scratch, blockMoves, backend, params)
{
    // TODO
    asm.nop();
};

// TODO: IfInstr

RetInstr.prototype.x86 = {};
RetInstr.prototype.x86.regAllocCfg = new x86.RegAllocCfg();
RetInstr.prototype.x86.regAllocCfg.opndCanBeImm = function (instr, idx, size) { return size <= 32; }
RetInstr.prototype.x86.genCode = function (asm, opnds, dest, scratch, blockMoves, backend, params)
{
    // TODO
    asm.nop();
}

LoadInstr.prototype.x86 = {};
LoadInstr.prototype.x86.regAllocCfg = new x86.RegAllocCfg();
LoadInstr.prototype.x86.regAllocCfg.opndCanBeImm = function (instr, idx, size) { return (idx === 1 && size <= 32); }
LoadInstr.prototype.x86.genCode = function (asm, opnds, dest, scratch, blockMoves, backend, params)
{
    // TODO
    asm.nop();
}

