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

// Get context instruction
GetCtxInstr.prototype.x86 = new x86.InstrCfg();
GetCtxInstr.prototype.x86.destMustBeReg = function (instr, params)
{
    return params.backend.ctxReg;
}
GetCtxInstr.prototype.x86.genCode = function (instr, opnds, dest, scratch, asm, genInfo)
{
    // Clear out the lower bits of the context register
    var ctxReg = genInfo.params.backend.ctxReg;
    asm.mov(ctxReg.getSubOpnd(8), 0);
};

// Set context instruction
SetCtxInstr.prototype.x86 = new x86.InstrCfg();
SetCtxInstr.prototype.x86.genCode = function (instr, opnds, dest, scratch, asm, genInfo)
{
    asm.mov(opnds[0], genInfo.params.backend.ctxReg);
};

// Base instruction configuration for arithmetic instructions
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
MulInstr.prototype.x86.opndMustBeReg = function (instr, idx, params)
{
    if (instr.type.isUnsigned() === true && idx === 0)
        return params.backend.x86_64? x86.regs.rax:x86.regs.eax;

    return false;
}
MulInstr.prototype.x86.writeRegSet = function (instr, params)
{
    if (instr.type.isUnsigned() === true)
        return [params.backend.x86_64? x86.regs.rdx:x86.regs.edx];

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

// Division instruction
DivInstr.prototype.x86 = new x86.InstrCfg();
DivInstr.prototype.x86.opndMustBeReg = function (instr, idx, params)
{
    if (idx === 0)
        return params.backend.x86_64? x86.regs.rax:x86.regs.eax;

    return false;
}
DivInstr.prototype.x86.destIsOpnd0 = function (instr)
{
    return false;
}
DivInstr.prototype.x86.destMustBeReg = function (instr, params)
{
    return params.backend.x86_64? x86.regs.rax:x86.regs.eax;
}
DivInstr.prototype.x86.writeRegSet = function (instr, params)
{
    return [params.backend.x86_64? x86.regs.rdx:x86.regs.edx];
}
DivInstr.prototype.x86.genCode = function (instr, opnds, dest, scratch, asm, genInfo)
{
    assert (
        opnds[0] === x86.regs.rax || opnds[0] === x86.regs.eax,
        'invalid first opnd: ' + opnds[0]
    );

    // If the output should be unsigned
    if (instr.type.isUnsigned())
    {
        // Zero-extend the value into xDX
        var xDX = x86.regs.rdx.getSubOpnd(instr.type.getSizeBits(genInfo.params));
        asm.mov(xDX, 0);

        // Use unsigned divide
        asm.div(opnds[1]);
    }
    else
    {
        // Sign-extend xAX into xDX:xAX
        switch (instr.type.getSizeBits(genInfo.params))
        {
            case 16: asm.cwd(); break;
            case 32: asm.cdq(); break;
            case 64: asm.cqo(); break;
        }

        // Use signed divide
        asm.idiv(opnds[1]);
    }
};

// Modulo instruction
ModInstr.prototype.x86 = Object.create(DivInstr.prototype.x86);
ModInstr.prototype.x86.destMustBeReg = function (instr, params)
{
    return params.backend.x86_64? x86.regs.rdx:x86.regs.edx;
}
ModInstr.prototype.x86.writeRegSet = function (instr, params)
{
    return [params.backend.x86_64? x86.regs.rax:x86.regs.eax];
}

/**
Generate a bitwise instruction's code generation
*/
x86.bitOpMaker = function (instrName)
{
    var instrConf = new x86.InstrCfg();

    instrConf.opndCanBeImm = function (instr, idx, size)
    { 
        return (size <= 32);
    };

    instrConf.genCode = function (instr, opnds, dest, scratch, asm, genInfo)
    {
        asm[instrName](opnds[0], opnds[1]);
    };

    return instrConf;
}

// Bitwise AND instruction
AndInstr.prototype.x86 = x86.bitOpMaker('and');

// Bitwise OR instruction
OrInstr.prototype.x86 = x86.bitOpMaker('or');

// Bitwise XOR instruction
XorInstr.prototype.x86 = x86.bitOpMaker('xor');

// Bitwise NOT instruction
NotInstr.prototype.x86 = new x86.InstrCfg();
NotInstr.prototype.x86.genCode = function (instr, opnds, dst, scratch, asm, genInfo)
{
    asm.not(dst);
};

/**
Generate a shift instruction's code generation
*/
x86.shiftMaker = function (instrName)
{
    var instrConf = new x86.InstrCfg();

    instrConf.opndCanBeImm = function (instr, idx, size)
    { 
        return (idx === 1 && size <= 8);
    };

    instrConf.genCode = function (instr, opnds, dest, scratch, asm, genInfo)
    {
        const backend = genInfo.params.backend;

        var opnd = opnds[1];

        if (opnds[1] instanceof x86.Immediate)
        {
            asm[instrName](dest, opnd);
        }
        else
        {
            var subOpnd = opnd.getSubOpnd(8);

            if (subOpnd.rexNeeded === true && backend.x86_64 === false)
            {
                var xcx = x86.regs.rdx.getSubReg(backend.regSizeBits);

                asm.xchg(x86.regs.xcx, opnd);
                asm[instrName](dest, x86.regs.cl);
                asm.xchg(x86.regs.xcx, opnd);
            }
            else
            {   
                asm.mov(x86.regs.cl, subOpnd);
                asm[instrName](dest, x86.regs.cl);
            }
        }
    };

    return instrConf;
}

// Left shift instruction
LsftInstr.prototype.x86 = x86.shiftMaker('sal');

// Right shift instruction
RsftInstr.prototype.x86 = x86.shiftMaker('sar');

// Unsigned right shift instruction
UrsftInstr.prototype.x86 = x86.shiftMaker('shr');

/**
Produce the code generation for arithmetic with overflow instructions
*/
x86.ArithOvfMaker = function (instrClass)
{
    var instrConf = Object.create(instrClass.prototype.x86);

    instrConf.genCode = function (instr, opnds, dest, scratch, asm, genInfo)
    {
        // Reuse the implementation of the instruction without overflow
        instrClass.prototype.x86.genCode.apply(
            this, 
            [instr, opnds, dest, scratch, asm, genInfo]
        );

        // Get the normal and overflow labels
        var thisBlock = instr.parentBlock;
        var normBlock = instr.targets[0];
        var overBlock = instr.targets[1];
        var normLabel = genInfo.edgeLabels.getItem({pred:thisBlock, succ:normBlock});
        var overLabel = genInfo.edgeLabels.getItem({pred:thisBlock, succ:overBlock});

        // Jump to the overflow block on overflow
        asm.jo(overLabel);
        asm.jmp(normLabel);
    };

    return instrConf;
}

// Addition with overflow handling
AddOvfInstr.prototype.x86 = x86.ArithOvfMaker(AddInstr);

// Subtraction with overflow handling
SubOvfInstr.prototype.x86 = x86.ArithOvfMaker(SubInstr);

// Multiplication with overflow handling
MulOvfInstr.prototype.x86 = x86.ArithOvfMaker(MulInstr);

// Left shift with overflow handling
LsftOvfInstr.prototype.x86 = x86.ArithOvfMaker(LsftInstr);

// Function call instruction
CallFuncInstr.prototype.x86 = new x86.InstrCfg();
// TODO: change to 'tachyon' once tachyon call conv support is completed
CallFuncInstr.prototype.x86.callConv = 'c';
CallFuncInstr.prototype.x86.maxImmOpnds = function (instr, idx, size)
{ 
    return instr.uses.length; 
}
CallFuncInstr.prototype.x86.maxMemOpnds = function (instr, idx, size)
{ 
    return instr.uses.length; 
}
CallFuncInstr.prototype.x86.opndCanBeImm = function (instr, idx, size) 
{ 
    return true; 
}
CallFuncInstr.prototype.x86.opndMustBeReg = function (instr, idx, params)
{
    // Get the calling convention for the callee
    var callConv = params.backend.getCallConv(this.callConv);


    // FIXME
    // FIXME: this deallocates live temps?
    // FIXME


    // If the argument must be in a register, return it
    //if (idx < callConv.argRegs.length)
    //    return callConv.argRegs[i];

    return false;
}
CallFuncInstr.prototype.x86.writeRegSet = function (instr, params)
{
    /*
    FIXME
    PROBLEM: right now, the write regs can't be used to allocate
    arguments into... That should probably be changed?

    May want to distinguish between spillregs/saveregs and excluderegs

    Look at other instructions that write to regs, can they receive
    their arguments in those registers?

    Mul instruction => yes
    Div/mod instruction => no

    */



    // Get the calling convention for the callee
    var callConv = params.backend.getCallConv(this.callConv);

    // FIXME
    //return this.callerSave;
    return undefined;
}
CallFuncInstr.prototype.x86.destMustBeReg = function (instr, params)
{
    // Get the calling convention for the callee
    var callConv = params.backend.getCallConv(this.callConv);

    // Return the return value register if there is one
    if (callConv.retReg instanceof x86.Register)
        return callConv.retReg;

    return true;
}

// TODO: scratch regs, need at least 1

CallFuncInstr.prototype.x86.genCode = function (instr, opnds, dest, scratch, asm, genInfo)
{
    // Get the calling convention for the callee
    var callConv = genInfo.params.backend.getCallConv(this.callConv);

    // Get the function pointer
    var funcPtr = opnds[0];


    // TODO: func object, global object? those are the operands, treated as
    // normal arguments








    asm.nop();
    asm.nop();
    asm.nop();


    // TODO
    //callConv.retReg;
};

// Unconditional branching instruction
JumpInstr.prototype.x86 = new x86.InstrCfg();
JumpInstr.prototype.x86.genCode = function (instr, opnds, dest, scratch, asm, genInfo)
{
    // Get the label for the jump
    var label = genInfo.edgeLabels.getItem(
        {pred: instr.parentBlock, succ:instr.targets[0]}
    );

    // Jump to the label
    asm.jmp(label);
}

// Conditional branching instruction
IfInstr.prototype.x86 = new x86.InstrCfg();
IfInstr.prototype.x86.opndCanBeImm = function (instr, idx, size)
{
    return (size <= 32); 
}
IfInstr.prototype.x86.genCode = function (instr, opnds, dest, scratch, asm, genInfo)
{
    // Get the true and false labels
    var thisBlock = instr.parentBlock;
    var trueBlock = instr.targets[0];
    var falseBlock = instr.targets[1];
    var trueLabel = genInfo.edgeLabels.getItem({pred:thisBlock, succ:trueBlock});
    var falseLabel = genInfo.edgeLabels.getItem({pred:thisBlock, succ:falseBlock});

    // Function to generate the comparison code
    function genCmp(jmpTrueSgn, jmpTrue, invSens)
    {
        if (opnds[0] instanceof x86.Immediate)
        {
            asm.cmp(opnds[1], opnds[0]);

            if (invSens === true)
            {
                var t = trueLabel;
                trueLabel = falseLabel;
                falseLabel = t;
            }
        }
        else
        {
            asm.cmp(opnds[0], opnds[1]);
        }

        if (instr.uses[0].type.isSigned() ||
            instr.uses[0].type === IRType.box)
            asm[jmpTrueSgn](trueLabel);
        else
            asm[jmpTrue](trueLabel);

        asm.jmp(falseLabel);
    }


    // Switch on the test operation
    switch (instr.testOp)
    {
        case 'LT':
        genCmp('jl', 'jb', true);
        break;

        case 'LE':
        genCmp('jle', 'jbe', true);
        break;

        case 'GT':
        genCmp('jg', 'ja', true);
        break;

        case 'GE':
        genCmp('jge', 'jae', true);
        break;

        case 'EQ':
        genCmp('je', 'je');
        break;

        case 'NE':
        genCmp('jne', 'jne');
        break;

        default:
        error(
            'unsupported comparison operation in if test: "' + 
            instr.testOp + '"'
        );
        break;
    }
};


// TODO: CallInstr

// TODO: ConstructInstr.prototype.x86 = CallInstr.prototype.x86;

// TODO: CallApplyInstr

// TODO: CallFFIInstr


// Return instruction
RetInstr.prototype.x86 = new x86.InstrCfg();
RetInstr.prototype.x86.opndMustBeReg = function (instr, idx, params)
{
    var cProxy = instr.parentBlock.parentCFG.ownerFunc.cproxy;
    var callConv = params.backend.getCallConv(cProxy? 'c':'tachyon');

    return callConv.retReg;
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
        var stackLoc = stackMap.getRegSlot(reg);
        var offset = stackMap.getSlotOffset(stackLoc);
        asm.mov(reg, asm.mem(backend.regSizeBits, spReg, offset));
    }

    // Remove space for the spills from the stack
    var spillSize = stackMap.getSpillSize();
    if (spillSize !== 0)
        asm.add(spReg, spillSize);

    // TODO: adapt this depending on calling convention

    asm.ret();
}


// TODO:
// Not yet implemented
ThrowInstr.prototype.x86 = RetInstr.prototype.x86;

// TODO: CatchInstr


// Integer cast instruction
ICastInstr.prototype.x86 = new x86.InstrCfg();
ICastInstr.prototype.x86.destMustBeReg = function (instr)
{
    return true;
}
ICastInstr.prototype.x86.destIsOpnd0 = function (instr, params)
{
    const srcWidth = instr.uses[0].type.getSizeBits(params);
    const dstWidth = instr.type.getSizeBits(params);

    return (srcWidth === dstWidth);
}
ICastInstr.prototype.x86.genCode = function (instr, opnds, dst, scratch, asm, genInfo)
{
    const srcWidth = instr.uses[0].type.getSizeBits(genInfo.params);
    const dstWidth = instr.type.getSizeBits(genInfo.params);

    var src = opnds[0];

    // If the source and destination have the same width
    if (srcWidth === dstWidth)
    {
        assert (
            (src === dst) || 
            (src instanceof x86.MemLoc && 
             dst instanceof x86.MemLoc &&
             src.base === dst.base && 
             src.disp === dst.disp &&
             src.index === dst.index &&
             src.scale === dst.scale),
            'source and destination allocations should match'
        );

        // Do nothing
    }

    // If the source width is larger than the destination width,
    // we retain only the least significative bits
    else if (srcWidth > dstWidth)
    {
        asm.mov(dst, src.getSubOpnd(dstWidth));
    }

    // If the source width is smaller than the destination width,
    // we need to extend the sign bit for signed values
    else
    {
        const isSigned = instr.uses[0].type.isSigned() && instr.type.isSigned();

        if (isSigned === true)
        {
            if (srcWidth === 32 && dstWidth === 64)
                asm.movsxd(dst, src);
            else
                asm.movsx(dst, src);
        }
        else
        {
            if (srcWidth === 32 && dstWidth === 64)
                asm.mov(dst.getSubOpnd(32), src);
            else
                asm.movzx(dst, src);
        }
    }
};

// Load from memory instruction
LoadInstr.prototype.x86 = new x86.InstrCfg();
LoadInstr.prototype.x86.opndCanBeImm = function (instr, idx, size)
{ 
    return (idx === 1 && size <= 32); 
}
LoadInstr.prototype.x86.opndMustBeReg = function (instr, idx)
{
    return true;
}
LoadInstr.prototype.x86.destIsOpnd0 = function (instr)
{
    return false;
}
LoadInstr.prototype.x86.destMustBeReg = function (instr, params)
{
    if (instr.type.getSizeBits(params) === 8 && 
        params.backend.x86_64 === false)
        return x86.regs.eax;

    return true;
}
LoadInstr.prototype.x86.genCode = function (instr, opnds, dst, scratch, asm, genInfo)
{
    var valSize = instr.type.getSizeBits(genInfo.params);

    var src = new x86.MemLoc(
        valSize,
        opnds[0],
        (opnds[1] instanceof x86.Immediate)? opnds[1].value:0, 
        (opnds[1] instanceof x86.Register)? opnds[1]:undefined,
        1
    );

    var dst = dst.getSubOpnd(valSize);

    asm.mov(dst, src);
}

// Store to memory
StoreInstr.prototype.x86 = new x86.InstrCfg();
StoreInstr.prototype.x86.opndCanBeImm = function (instr, idx, size)
{ 
    return (idx === 1 && size <= 32); 
}
StoreInstr.prototype.x86.opndMustBeReg = function (instr, idx, params)
{
    if (idx === 2 && 
        instr.uses[2].type.getSizeBits(params) === 8 && 
        params.backend.x86_64 === false)
        return x86.regs.eax;

    return true;
}
StoreInstr.prototype.x86.destIsOpnd0 = function (instr)
{
    return false;
}
StoreInstr.prototype.x86.genCode = function (instr, opnds, dst, scratch, asm, genInfo)
{
    var valSize = instr.uses[2].type.getSizeBits(genInfo.params);

    var dst = new x86.MemLoc(
        valSize,
        opnds[0],
        (opnds[1] instanceof x86.Immediate)? opnds[1].value:0, 
        (opnds[1] instanceof x86.Register)? opnds[1]:undefined,
        1
    );

    var src = opnds[2].getSubOpnd(valSize);

    asm.mov(dst, src);
}

