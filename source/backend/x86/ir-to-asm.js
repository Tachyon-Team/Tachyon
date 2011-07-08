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
                instr,
                instrAlloc.opnds,
                instrAlloc.dest,
                instrAlloc.scratchRegs,
                allocInfo.blockMoves,
                asm,
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
IRInstr.prototype.x86 = new x86.InstrCfg();

GetCtxInstr.prototype.x86 = new x86.InstrCfg();
GetCtxInstr.prototype.x86.genCode = function (instr, opnds, dest, scratch, blockMoves, asm, params)
{
    // TODO
    asm.nop();
};

ArithInstr.prototype.x86 = new x86.InstrCfg();
ArithInstr.prototype.x86.opndCanBeImm = function (instr, idx, size)
{ 
    return (size <= 32); 
}

AddInstr.prototype.x86 = Object.create(ArithInstr.prototype.x86);
AddInstr.prototype.x86.genCode = function (instr, opnds, dest, scratch, blockMoves, asm, params)
{
    asm.add(opnds[0], opnds[1]);
};

SubInstr.prototype.x86 = Object.create(ArithInstr.prototype.x86);
SubInstr.prototype.x86.genCode = function (instr, opnds, dest, scratch, blockMoves, asm, params)
{
    asm.sub(opnds[0], opnds[1]);
};

MulInstr.prototype.x86 = Object.create(ArithInstr.prototype.x86);
MulInstr.prototype.x86.opndRegSet = function (instr, idx)
{
    if (instr.type.isUnsigned() && idx === 0)
        return [x86.regs.rax, x86.regs.eax];

    return undefined;
}
MulInstr.prototype.x86.writeRegSet = function (instr)
{
    if (instr.type.isUnsigned())
        return [x86.regs.rdx, x86.regs.edx];

    return undefined;
}
MulInstr.prototype.x86.destIsOpnd0 = function (instr, params)
{
    if (instr.type.isUnsigned() === false)
    {
        for (var i = 0; i < instr.uses.length; ++i)
        {
            if (x86.getImmSize(instr.uses[i], params) <= 32)
                return false;
        }
    }

    return true;
}
MulInstr.prototype.x86.genCode = function (instr, opnds, dest, scratch, blockMoves, asm, params)
{
    // If an unsigned integer result is expected
    if (instr.type.isUnsigned())
    {
        assert (
            opnds[0] === x86.regs.rax || opnds[0] === x86.regs.eax,
            'invalid opnd 0'
        );

        asm.mul(opnds[1]);
    }

    // Otherwise, a signed result is expected
    else
    {
        if (opnds[0] instanceof x86.Immediate)
            asm.imul(dest, opnds[1], opnds[0]);
        else if (opnds[1] instanceof x86.Immediate)
            asm.imul(dest, opnds[0], opnds[1]);
        else
            asm.imul(opnds[0], opnds[0]);
    }
};

CallFuncInstr.prototype.x86 = new x86.InstrCfg();
CallFuncInstr.prototype.x86.maxImmOpnds = function (instr, idx, size)
{ 
    return instr.uses.length; 
}
CallFuncInstr.prototype.x86.opndCanBeImm = function (instr, idx, size) 
{ 
    return true; 
}
CallFuncInstr.prototype.x86.genCode = function (instr, opnds, dest, scratch, blockMoves, asm, params)
{
    // TODO
    asm.nop();
};

// TODO: IfInstr

RetInstr.prototype.x86 = new x86.InstrCfg();
RetInstr.prototype.x86.opndCanBeImm = function (instr, idx, size)
{ 
    return size <= 32; 
}
RetInstr.prototype.x86.genCode = function (instr, opnds, dest, scratch, blockMoves, asm, params)
{
    // TODO
    asm.nop();
}

LoadInstr.prototype.x86 = new x86.InstrCfg();
LoadInstr.prototype.x86.opndCanBeImm = function (instr, idx, size) 
{ 
    return (idx === 1 && size <= 32); 
}
LoadInstr.prototype.x86.genCode = function (instr, opnds, dest, scratch, blockMoves, asm, params)
{
    // TODO
    asm.nop();
}

