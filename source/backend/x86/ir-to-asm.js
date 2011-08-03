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
IR to assembly translation.

@author
Maxime Chevalier-Boisvert
*/

ArgValInstr.prototype.x86_genCode = function (instr, asm, codeGen)
{
    const callConv = codeGen.callConv;
    const allocMap = codeGen.allocMap;

    // Get the argument index
    var argIndex = instr.argIndex;

    // Operand for this argument
    var argOpnd;

    // If the argument is in a register
    if (argIndex < callConv.argRegs.length)
    {
        // Allocate the register to the argument
        argOpnd = callConv.argRegs[argIndex];
    }
    else
    {
        // Map the argument to its stack location
        argOpnd = allocMap.getArgSlot(argIndex);
    }

    // Allocate the argument to its operand
    codeGen.alloc_dst(argOpnd);
}

PhiInstr.prototype.x86_genCode = function ()
{
    // Do nothing
}

GetCtxInstr.prototype.x86_genCode = function (instr, asm, codeGen)
{
    // TODO: alloc dest to context reg

    // TODO:
    asm.nop();
}

SetCtxInstr.prototype.x86_genCode = function (instr, asm, codeGen)
{
    // TODO: move operand into upper portion of context reg

    // TODO:
    asm.nop();
}

AddInstr.prototype.x86_genCode = function (instr, asm, codeGen)
{
    codeGen.alloc_opnd_r(instr.uses[0]);
    codeGen.alloc_opnd_r(instr.uses[1]);
    codeGen.alloc_dst_opnd(0);

    var opnd0 = codeGen.get_opnd(0);
    var opnd1 = codeGen.get_opnd(1);

    asm.add(opnd0, opnd1);
};

CallFuncInstr.prototype.x86_genCode = function (instr, asm, codeGen)
{
    // TODO:
    asm.nop();
}

// TODO: CallFFIInstr

// TODO: CallApplyInstr

RetInstr.prototype.x86_genCode = function (instr, asm, codeGen)
{
    const callConv = codeGen.callConv;
    const allocMap = codeGen.allocMap;

    // Allocate the input value to the return value register
    codeGen.alloc_opnd_r(instr.uses[0], callConv.retReg);

    // Remove the spills from the stack
    asm.add(allocMap.spReg, allocMap.numSpillSlots * allocMap.slotSize);

    asm.ret();
};

LoadInstr.prototype.x86_genCode = function (instr, asm, codeGen)
{
    // TODO:
    asm.nop();
}

