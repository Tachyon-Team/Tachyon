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

    // Get a reference to the stack frame map
    var stackMap = allocInfo.stackMap;

    // Get the calling convention
    var callConv = params.backend.getCallConv(irFunc.cProxy? 'c':'tachyon');

    // Add space for the spills on the stack
    if (stackMap.spillSize !== 0)
        asm.sub(backend.spReg, stackMap.spillSize);

    // Save the callee-save registers, if any
    for (var i = 0; i < callConv.calleeSave.length; ++i)
    {
        var reg = callConv.calleeSave[i];
        var stackLoc = stackMap.getStackLoc(reg);
        var offset = stackMap.getLocOffset(stackLoc);
        asm.mov(asm.mem(backend.regSizeBits, backend.spReg, offset), reg);
    }

    //
    // TODO
    // Callee pops stack frame & args... Ideally want stack frame
    // normalization stub.
    //




    // TODO: 
    // allocInfo.mergeMoves
    // Want to build merge stubs, place them appropriately in the block
    // ordering.


    // TODO: set of labels for each CFG edge





    // Code generation info object
    var genInfo = {
        callConv: callConv,
        labels: [],
        stackMap: stackMap,
        backend: backend,
        params: params
    };

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

            // Insert the pre-instruction moves
            for (var k = 0; k < instrAlloc.preMoves.length; ++k)
                x86.insertMove(asm, instrAlloc.preMoves[k], params);

            // Generate code for the instruction
            instr.x86.genCode(
                instr,
                instrAlloc.opnds,
                instrAlloc.dest,
                instrAlloc.scratchRegs,
                asm,
                genInfo
            );
        }
    }

    print('');
    print('assembly:')
    print(asm.toString(true));

    // Return the assembler object
    return asm;
}

/**
Implement an abstract move operation
*/
x86.insertMove = function (asm, move, params)
{
    assert (
        move.dst instanceof x86.Operand,
        'invalid move dst: ' + move.dst
    );

    // If the source is an x86 operand
    if (move.src instanceof x86.Operand)
    {
        assert (
            !(move.src instanceof x86.MemLoc) ||
            !(move.dst instanceof x86.MemLoc),
            'memory to memory move'
        );

        asm.mov(move.dst, move.src);
        return;
    }

    // If the source is a constant
    if (move.src instanceof ConstValue)
    {
        var immSize = x86.getImmSize(move.src, params);

        // If the constant can be encoded as an immediate
        if (immSize !== undefined && immSize <= params.backend.regSizeBits)
        {
            asm.mov(move.dst, move.src.getImmValue(params));
            return;
        }
    }





    // TODO
    asm.nop();

    /*
    PROBLEM: for some moves, may need EAX/RAX to be free

    eg: move rdx, *<addr_64>
    This is used to load the value at some address...

    solution:
    xchg rdx, rax
    move rax, <addr_64>
    xchg rdx, rax

    This avoids overwriting the value in rax, if any.

    If rax gets written to after this, can always have opt pattern to
    remove redundant xchgs.

    NOTE: there is a mov, r64, imm64!
    */
}

// TODO: for now, default reg alloc config to test register allocation
IRInstr.prototype.x86 = new x86.InstrCfg();

GetCtxInstr.prototype.x86 = new x86.InstrCfg();
GetCtxInstr.prototype.x86.destMustBeReg = function (instr, params)
{
    return true;
}
GetCtxInstr.prototype.x86.destRegSet = function (instr, idx, params)
{
    return [x86.ctxReg32, x86.ctxReg64];
}
GetCtxInstr.prototype.x86.genCode = function (instr, opnds, dest, scratch, asm, genInfo)
{
    // Do nothing
};

SetCtxInstr.prototype.x86 = new x86.InstrCfg();
SetCtxInstr.prototype.x86.genCode = function (instr, opnds, dest, scratch, asm, genInfo)
{
    var x86_64 = params.backend.x86_64;

    asm.move(opnds[0], x86_64? x86.ctxReg64:x86.ctxReg32);
};

ArithInstr.prototype.x86 = new x86.InstrCfg();
ArithInstr.prototype.x86.opndCanBeImm = function (instr, idx, size)
{ 
    return (size <= 32); 
}

AddInstr.prototype.x86 = Object.create(ArithInstr.prototype.x86);
AddInstr.prototype.x86.genCode = function (instr, opnds, dest, scratch, asm, genInfo)
{
    asm.add(opnds[0], opnds[1]);
};

SubInstr.prototype.x86 = Object.create(ArithInstr.prototype.x86);
SubInstr.prototype.x86.genCode = function (instr, opnds, dest, scratch, asm, genInfo)
{
    asm.sub(opnds[0], opnds[1]);
};

MulInstr.prototype.x86 = Object.create(ArithInstr.prototype.x86);
MulInstr.prototype.x86.opndMustBeReg = function (instr, idx)
{
    return (idx === 0);
}
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
MulInstr.prototype.x86.genCode = function (instr, opnds, dest, scratch, asm, genInfo)
{
    // If an unsigned integer result is expected
    if (instr.type.isUnsigned() === true)
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
            asm.imul(opnds[0], opnds[1]);
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
CallFuncInstr.prototype.x86.genCode = function (instr, opnds, dest, scratch, asm, genInfo)
{
    // TODO
    asm.nop();
};

// Unconditional branching instruction
JumpInstr.prototype.x86 = new x86.InstrCfg();
JumpInstr.prototype.x86.genCode = function (instr, opnds, dest, scratch, asm, genInfo)
{
    asm.jump(labels[instr.parentBlock.blockId][instr.targets[0].blockId]);
}

// Conditional branching instruction
IfInstr.prototype.x86 = new x86.InstrCfg();
IfInstr.prototype.x86.genCode = function (instr, opnds, dest, scratch, asm, genInfo)
{
    // TODO: comparison & branching code
    asm.nop();

    /*
    TODO: block moves

    jnz foo1

    ... moves for foo2

    jmp foo2_prelude

    The moves for the "then" block must be in a separate prelude.
    Probably, this prelude should immediately precede the target block.

    TODO: consult block labels to know where to jump. Don't insert
    block moves manually!
    */
};

RetInstr.prototype.x86 = new x86.InstrCfg();
RetInstr.prototype.x86.opndMustBeReg = function (instr, idx, params)
{
    return true;
}
RetInstr.prototype.x86.opndRegSet = function (instr, idx, params)
{ 
    var cProxy = instr.parentBlock.parentCFG.ownerFunc.cproxy;
    var callConv = params.backend.getCallConv(cProxy? 'c':'tachyon');

    return [callConv.retReg];
}
RetInstr.prototype.x86.genCode = function (instr, opnds, dest, scratch, asm, genInfo)
{
    var backend = genInfo.backend;
    var callConv = genInfo.callConv;
    var stackMap = genInfo.stackMap;
    var spReg = genInfo.backend.spReg;

    // Restore the callee-save registers, if any
    for (var i = 0; i < callConv.calleeSave.length; ++i)
    {
        var reg = callConv.calleeSave[i];
        var stackLoc = stackMap.getStackLoc(reg);
        var offset = stackMap.getLocOffset(stackLoc);
        asm.mov(reg, asm.mem(backend.regSizeBits, spReg, offset));
    }

    // Remove space for the spills from the stack
    if (stackMap.spillSize !== 0)
        asm.add(spReg, stackMap.spillSize);

    // TODO: adapt this depending on calling convention

    asm.ret();
}

LoadInstr.prototype.x86 = new x86.InstrCfg();
LoadInstr.prototype.x86.opndCanBeImm = function (instr, idx, size)
{ 
    return (idx === 1 && size <= 32); 
}
LoadInstr.prototype.x86.destIsOpnd0 = function (instr)
{
    return false;
}
LoadInstr.prototype.x86.destMustBeReg = function (instr)
{
    return true;
}
LoadInstr.prototype.x86.genCode = function (instr, opnds, dest, scratch, asm, genInfo)
{
    // TODO
    asm.nop();
}

