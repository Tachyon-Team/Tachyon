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
GetCtxInstr.prototype.x86.genCode = function (instr, opnds, dest, scratch, asm, genInfo)
{
    // Get the context register
    var ctxReg = genInfo.params.backend.ctxReg;

    // Clear out the lower bits of the context register
    asm.mov(ctxReg.getSubOpnd(8), 0);

    // Move the context pointer into the destination
    asm.mov(dest, ctxReg);
};

// Set context instruction
SetCtxInstr.prototype.x86 = new x86.InstrCfg();
SetCtxInstr.prototype.x86.excludeRegs = function (instr, params)
{
    // If anything is mapped to the context register, it should be moved out
    // Note: the AMD64 calling convention uses rcx for arguments
    return [params.backend.ctxReg];
}
SetCtxInstr.prototype.x86.genCode = function (instr, opnds, dest, scratch, asm, genInfo)
{
    // Get the context register
    var ctxReg = genInfo.params.backend.ctxReg;

    // Move the operand into the context register
    asm.mov(ctxReg, opnds[0]);
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
    // mul uses rax as dest
    if (instr.type.isUnsigned() === true && idx === 0)
        return params.backend.x86_64? x86.regs.rax:x86.regs.eax;

    // If the dest is the first operand, it should be a register
    if (this.destIsOpnd0(instr, params) === true)
        return true;

    return false;
}
MulInstr.prototype.x86.saveRegs = function (instr, params)
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
DivInstr.prototype.x86.excludeRegs = function (instr, params)
{
    return undefined;
}
DivInstr.prototype.x86.excludeRegs = function (instr, params)
{
    return [params.backend.x86_64? x86.regs.rdx:x86.regs.edx];
}
DivInstr.prototype.x86.genCode = function (instr, opnds, dest, scratch, asm, genInfo)
{
    assert (
        opnds[0] === x86.regs.rax || opnds[0] === x86.regs.eax,
        'invalid first opnd: ' + opnds[0]
    );

    if (genInfo.backend.debugTrace === true)
        x86.genTracePrint(asm, genInfo.params, 'division/modulo');

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
ModInstr.prototype.x86.saveRegs = function (instr, params)
{
    return [params.backend.x86_64? x86.regs.rax:x86.regs.eax];
}
ModInstr.prototype.x86.excludeRegs = function (instr, params)
{
    return undefined;
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
                var xcx = x86.regs.rcx.getSubOpnd(backend.regSizeBits);

                asm.xchg(xcx, opnd);
                asm[instrName](dest, x86.regs.cl);
                asm.xchg(xcx, opnd);
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
CallFuncInstr.prototype.x86.calleeConv = 'tachyon';
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
    // The function pointer shouldn't be received as an immediate
    if (idx === 0)
        return false;

    return true; 
}
CallFuncInstr.prototype.x86.opndMustBeReg = function (instr, idx, params)
{
    // Get the calling convention for the callee
    var callConv = params.backend.getCallConv(this.calleeConv);

    // The first argument is the function pointer
    if (idx === 0)
        return false;

    // If the argument must be in a register, return it
    if (idx - 1 < callConv.argRegs.length)
        return callConv.argRegs[idx - 1];

    return false;
}
CallFuncInstr.prototype.x86.saveRegs = function (instr, params)
{
    // Get the calling convention for the callee
    var callConv = params.backend.getCallConv(this.calleeConv);

    // The caller-save registers must be saved before the call
    return callConv.callerSave;
}
CallFuncInstr.prototype.x86.destIsOpnd0 = function ()
{
    return false;
}
CallFuncInstr.prototype.x86.destMustBeReg = function (instr, params)
{
    // Get the calling convention for the callee
    var callConv = params.backend.getCallConv(this.calleeConv);

    // Return the return value register if there is one
    if (callConv.retReg instanceof x86.Register)
        return callConv.retReg;

    return true;
}
CallFuncInstr.prototype.x86.numScratchRegs = function (instr, params)
{
    // Get the calling convention for the caller
    var irFunc = instr.parentBlock.parentCFG.ownerFunc;
    var callerConv = params.backend.getCallConv(irFunc.cProxy? 'c':'tachyon');

    // Get the calling convention for the callee
    var calleeConv = params.backend.getCallConv(this.calleeConv);

    // Determine if the stack pointer needs to be dynamically aligned
    var alignNeeded = (callerConv !== calleeConv && callerConv.spAlign < calleeConv.spAlign);

    // An extra scratch register is needed if we dynamically align the sp
    if (alignNeeded === true)
        return 2;

    // Need 1 scratch register
    return 1;
}
CallFuncInstr.prototype.x86.transStackOpnds = function (instr, params)
{
    // Delay translation of stack operands
    return false;
}
CallFuncInstr.prototype.x86.genCode = function (instr, opnds, dest, scratch, asm, genInfo)
{
    // Get a reference to the backend
    const backend = genInfo.backend;

    // Get the calling convention for the caller
    var callerConv = genInfo.callConv;

    // Get the calling convention for the callee
    var calleeConv = backend.getCallConv(this.calleeConv);

    // Get the allocation map
    var allocMap = genInfo.allocMap;

    // Get the stack pointer register
    var spReg = backend.spReg;

    // Get the context register
    var ctxReg = backend.ctxReg;

    // Get the temporary register
    var tmpReg = scratch[0];

    // Get the temporary stack pointer
    var tmpSp = scratch[1];

    // Get the function pointer
    var funcPtr = opnds[0];

    // Compute the actual number of arguments
    var numArgs = opnds.length - 1;

    // Compute the number of non-hidden arguments
    var numActualArgs = (this.calleeConv === 'tachyon')? (numArgs - 2):numArgs;

    assert (
        numActualArgs < 255,
        'too many arguments in function call: ' + numActualArgs
    );

    // Compute the number of register arguments
    var numRegArgs = Math.min(numArgs, calleeConv.argRegs.length);

    // Compute the number of stack arguments
    var numStackArgs = Math.max(numArgs - numRegArgs, 0);

    // Determine if the stack pointer needs to be dynamically aligned
    var alignNeeded = (callerConv !== calleeConv && callerConv.spAlign < calleeConv.spAlign);

    // Compute the stack space needed for the arguments
    var argSpace = allocMap.slotSize * numStackArgs;

    // Total stack space needing to be reserved
    var totalSpace = argSpace;

    // If we are calling a C function
    if (this.calleeConv === 'c')
    {
        // Create a memory location for the saved context
        var ctxLoc = new x86.MemLoc(
            backend.regSizeBits,
            spReg,
            totalSpace
        );

        // Add space to save the context register
        totalSpace += allocMap.slotSize;
    }

    // If the stack pointer needs to be dynamically aligned
    if (alignNeeded === true)
    {
        // Create a memory location for the saved stack pointer
        var spLoc = new x86.MemLoc(
            backend.regSizeBits,
            spReg,
            totalSpace
        );

        // Add space to save the old stack pointer
        totalSpace += allocMap.slotSize;

        // Move the stack pointer into an alternate register
        asm.mov(tmpSp, spReg);
    }
    else
    {
        var frameSize = allocMap.slotSize * allocMap.numSpillSlots;
        var alignRem = (frameSize + totalSpace) % calleeConv.spAlign;

        // If the current frame size breaks the alignment
        if (alignRem !== 0)
        {
            // Pad the total space by the necessary amount to align it
            var padSpace = calleeConv.spAlign - alignRem;
            totalSpace += padSpace;
        }
    }

    // Reserve the total stack space needed
    asm.sub(spReg, totalSpace);

    // If the stack pointer needs to be dynamically aligned
    if (alignNeeded === true)
    {
        // Compute the stack pointer alignment mask
        const alignMask = ~(calleeConv.spAlign - 1);

        // Align the new stack pointer
        asm.and(spReg, alignMask);

        // Save the old stack pointer below the arguments
        asm.mov(spLoc, tmpSp);
    }

    // If we are calling a C function
    if (this.calleeConv === 'c')
    {
        // Save the context register
        asm.mov(ctxLoc, ctxReg);
    }

    /**
    Copy an argument to its stack location
    */
    function copyStackArg(argIdx, stackArgIdx)
    {
        var opndIdx = opnds.length - numArgs + argIdx;
        var srcOpnd = opnds[opndIdx];

        // Get the stack argument operand
        var dstOpnd = new x86.MemLoc(
            allocMap.slotSize * 8,
            spReg,
            stackArgIdx * allocMap.slotSize
        );

        // If this is a memory->memory move
        if (typeof srcOpnd === 'number')
        {
            // Get the source operand
            var srcOpnd = allocMap.getSlotOpnd(
                srcOpnd,
                undefined,
                (alignNeeded === true)? tmpSp:spReg,
                (alignNeeded === true)? 0:totalSpace
            );

            asm.mov(tmpReg, srcOpnd);
            asm.mov(dstOpnd, tmpReg);
        }
        else
        {
            x86.moveValue(
                allocMap,
                dstOpnd,
                srcOpnd,
                asm,
                genInfo.params
            );
        }
    }

    // Copy the stack arguments
    if (calleeConv.argOrder === 'LTR')
    {
        // For each function argument, in left-to-right order
        for (var i = numStackArgs - 1; i >= 0; --i)
            copyStackArg(numRegArgs + i , numStackArgs - i - 1);
    }
    else if (calleeConv.argOrder === 'RTL')
    {
        // For each function argument, right-to-left order
        for (var i = 0; i < numStackArgs; ++i)
            copyStackArg(numRegArgs + i, i);
    }
    else
    {
        error('invalid argument order');
    }

    // If there is an argument count register, set its value
    if (calleeConv.argCountReg !== null)
        asm.mov(calleeConv.argCountReg, numActualArgs);

    // Try to get the callee function name
    var callee = instr.getCallee();
    var calleeName;
    if (callee !== undefined)
        calleeName = '"' + callee.funcName + '"' + ((callee instanceof CFunction)? ' (C)':'');
    else 
        calleeName = '<unknown>';

    if (backend.debugTrace === true)
        x86.genTracePrint(asm, genInfo.params, 'calling ' + calleeName);

    // Translate the function pointer operand if it is a stack reference
    if (typeof funcPtr === 'number')
    {
        funcPtr = allocMap.getSlotOpnd(
            funcPtr,
            undefined,
            (alignNeeded === true)? tmpSp:spReg,
            (alignNeeded === true)? 0:totalSpace
        );
    }

    // Call the function with the given address
    asm.call(funcPtr);

    if (backend.debugTrace === true)
        x86.genTracePrint(asm, genInfo.params, 'returned from ' + calleeName);

    // If we are calling a C function
    if (this.calleeConv === 'c')
    {
        // Restore the context register
        asm.mov(ctxReg, ctxLoc);
    }

    // If the stack pointer was dynamically aligned
    if (alignNeeded === true)
    {
        // Restore the stack pointer
        asm.mov(spReg, spLoc);
    }
    else
    {
        // If the arguments should be removed by the caller
        if (calleeConv.cleanup === 'CALLER')
        {
            // Remove the stack space for the arguments
            asm.add(spReg, totalSpace);
        }
        
        // If the stack was statically padded
        else if (padSpace !== undefined)
        {
            // Remove statically added padding space
            asm.add(spReg, padSpace);
        }
    }

    // If the call has a continuation label
    if (instr.targets[0] !== undefined)
    {
        // Jump to the continuation label
        var thisBlock = instr.parentBlock;
        var contBlock = instr.targets[0];
        var contLabel = genInfo.edgeLabels.getItem({pred:thisBlock, succ:contBlock});
        asm.jmp(contLabel);
    }
};

// Constructor call instruction, same implementation as regular calls
ConstructInstr.prototype.x86 = CallFuncInstr.prototype.x86;

// FFI call instruction, reuses the regular call logic
CallFFIInstr.prototype.x86 = Object.create(CallFuncInstr.prototype.x86);
CallFFIInstr.prototype.x86.calleeConv = 'c';

// Call using apply instruction
CallApplyInstr.prototype.x86 = new x86.InstrCfg();
CallApplyInstr.prototype.x86.calleeConv = 'tachyon';
CallApplyInstr.prototype.x86.maxMemOpnds = function (instr, idx, size)
{ 
    return instr.uses.length;
}
CallApplyInstr.prototype.x86.opndMustBeReg = function (instr, idx, params)
{
    // The argument table pointer must be in a register
    if (idx === 3)
        return true;

    return false;
}
CallApplyInstr.prototype.x86.saveRegs = function (instr, params)
{
    // Get the calling convention for the callee
    var callConv = params.backend.getCallConv(this.calleeConv);

    // The caller-save registers must be saved before the call
    return callConv.callerSave;
}
CallApplyInstr.prototype.x86.excludeRegs = function (instr, params)
{
    // Get the calling convention for the callee
    var callConv = params.backend.getCallConv(this.calleeConv);

    // Do not want instruction arguments inside argument registers
    var argRegSet = callConv.argRegs.slice(0);
    arraySetRem(argRegSet, callConv.retReg);
    return argRegSet;
}
CallApplyInstr.prototype.x86.destIsOpnd0 = function ()
{
    return false;
}
CallApplyInstr.prototype.x86.destMustBeReg = function (instr, params)
{
    // Get the calling convention for the callee
    var callConv = params.backend.getCallConv(this.calleeConv);

    // Return the return value register if there is one
    if (callConv.retReg instanceof x86.Register)
        return callConv.retReg;

    return true;
}
CallApplyInstr.prototype.x86.numScratchRegs = function (instr, params)
{
    // Reserve 0 scratch registers
    return 0;
}
CallApplyInstr.prototype.x86.transStackOpnds = function (instr, params)
{
    // Delay translation of stack operands
    return false;
}
CallApplyInstr.prototype.x86.genCode = function (instr, opnds, dest, scratch, asm, genInfo)
{
    // Get a reference to the backend
    const backend = genInfo.backend;

    // Get the calling convention for the caller
    var callerConv = genInfo.callConv;

    // Get the calling convention for the callee
    var calleeConv = backend.getCallConv(this.calleeConv);

    // Get the argument registers
    var argRegs = calleeConv.argRegs;

    assert (
        calleeConv.cleanup === 'CALLEE',
        'callee calling convention must be callee cleanup for call_apply'
    );

    assert (
        calleeConv.argOrder === 'LTR',
        'callee convention must use left-to-right arguments order'
    );

    assert (
        argRegs.length >= 4,
        'not enough argument registers to implement call_apply'
    );

    assert (
        backend.ctxReg.getSubOpnd(8) === calleeConv.argCountReg,
        'argument count register does not match context register'
    );

    // Get the allocation map
    var allocMap = genInfo.allocMap;

    // Get the stack pointer register
    var spReg = backend.spReg;

    // Get the instruction arguments
    var funcPtr = opnds[0];
    var funcObj = opnds[1];
    var thisObj = opnds[2];
    var argTbl  = opnds[3];
    var numArgs = opnds[4];

    assert (
        arraySetHas(argRegs, argTbl) === false,
        'argument table register in argument registers'
    );

    // Get the displacement for the arguments table
    const tblDisp = genInfo.params.memLayouts.arrtbl.getFieldOffset(["tbl", 0]);

    // Number of hidden arguments
    const NUM_HIDDEN_ARGS = 2;

    // Number of non-hidden arguments that can be passed in registers
    const NON_STACK_ARGS = argRegs.length - NUM_HIDDEN_ARGS;

    // Label for the case where we have stack arguments
    var HAVE_STACK_ARGS = new x86.Label('HAVE_STACK_ARGS');
    
    // Label for adding stack space
    var ADD_STACK_SPACE = new x86.Label('ADD_STACK_SPACE');

    // Stack argument copying loop
    var STACK_ARG_LOOP = new x86.Label('STACK_ARG_LOOP');

    // Stack argument loop exit
    var STACK_ARG_DONE = new x86.Label('STACK_ARG_DONE');

    // Label to push the argument count on the stack
    var PUSH_ARG_COUNT = new x86.Label('PUSH_ARG_COUNT');

    // Label for the register argument copying
    var COPY_REG_ARGS = new x86.Label('COPY_REG_ARGS');

    // Temporary stack pointer register
    var tmpSp = argRegs[0];

    // Register for the stack argument count
    var numStackArgs = argRegs[1];

    // Register for the argument stack space
    var argSpace = argRegs[2];

    if (backend.debugTrace === true)
        x86.genTracePrint(asm, genInfo.params, 'call w/ apply code');

    // Save the old sp into a temp register
    asm.mov(tmpSp, spReg);

    // Function to translate stack reference operands relative
    // to the old stack pointer
    function translOpnd(opnd)
    {
        if (typeof opnd === 'number')
            return allocMap.getSlotOpnd(opnd, backend.regSizeBits, tmpSp);

        return opnd;
    }

    // Translate the stack reference operands
    var funcPtr = translOpnd(funcPtr);
    var funcObj = translOpnd(funcObj);
    var thisObj = translOpnd(thisObj);
    var numArgs = translOpnd(numArgs);

    // If there are stack arguments
    asm.cmp(numArgs, NON_STACK_ARGS);
    asm.jg(HAVE_STACK_ARGS);

    // No space needed for stack arguments
    asm.mov(numStackArgs, 0);
    asm.jmp(ADD_STACK_SPACE);

    // There are stack arguments, compute how many of them there are
    asm.addInstr(HAVE_STACK_ARGS);
    asm.mov(numStackArgs, numArgs);
    asm.sub(numStackArgs, NON_STACK_ARGS);

    // Update the stack pointer
    asm.addInstr(ADD_STACK_SPACE);

    // Compute the space needed for stack arguments
    asm.imul(argSpace, numStackArgs, backend.regSizeBytes);

    // Add stack space for the arguments and old sp
    asm.sub(spReg, argSpace);
    asm.sub(spReg, backend.regSizeBytes);

    // Compute the stack pointer alignment mask and align the stack pointer
    const alignMask = ~(calleeConv.spAlign - 1);
    asm.and(spReg, alignMask);

    // Save the old sp below the arguments
    asm.mov(
        asm.mem(
            spReg.size,
            spReg,
            0,
            argSpace
        ),
        tmpSp
    );

    // Register for the stack argument index
    var stackIdx = argRegs[1];

    // Register for the table index
    var tableIdx = argRegs[2];

    // Memory-memory move temporary register
    var mtmTmp = argRegs[3];

    // Initialize the loop indices
    asm.mov(stackIdx, numStackArgs);
    asm.sub(stackIdx, 1);
    asm.mov(tableIdx, NON_STACK_ARGS);

    if (backend.debugTrace === true)
        x86.genTracePrint(asm, genInfo.params, 'copying stack args');

    // Stop looping when the stack index is 0
    asm.addInstr(STACK_ARG_LOOP);
    asm.cmp(stackIdx, 0);
    asm.jl(STACK_ARG_DONE);

    // Move the value from the argument table to the tmp reg
    asm.mov(
        mtmTmp,
        asm.mem(
            backend.regSizeBits,
            argTbl,
            tblDisp,
            tableIdx,
            backend.regSizeBytes
        )
    );

    // Move the value from the tmp reg to the argument slot
    asm.mov(
        asm.mem(
            backend.regSizeBits,
            spReg,
            0,
            stackIdx,
            backend.regSizeBytes
        ),
        mtmTmp
    );

    // Increment the loop indices and repeat the loop
    asm.sub(stackIdx, 1);
    asm.add(tableIdx, 1);
    asm.jmp(STACK_ARG_LOOP);

    // Done copying the stack arguments
    asm.addInstr(STACK_ARG_DONE);

    if (backend.debugTrace === true)
        x86.genTracePrint(asm, genInfo.params, 'done copying stack args');

    // If there are too many arguments
    asm.cmp(numArgs, 255);
    asm.jge(PUSH_ARG_COUNT);

    // Set the argument count into the argument count register
    asm.mov(argRegs[1], numArgs);
    asm.mov(backend.ctxReg.getSubOpnd(8), 0);
    asm.or(backend.ctxReg, argRegs[1]);
    asm.jmp(COPY_REG_ARGS);

    // Push the argument count on the stack
    asm.addInstr(PUSH_ARG_COUNT);
    if (backend.debugTrace === true)
        x86.genTracePrint(asm, genInfo.params, 'pushing arg count');
    asm.mov(calleeConv.argCountReg, 255)
    asm.push(numArgs);

    // Copy the register arguments
    asm.addInstr(COPY_REG_ARGS);

    if (backend.debugTrace === true)
        x86.genTracePrint(asm, genInfo.params, 'copying register args');

    // For each argument register
    for (var i = argRegs.length - 1; i >= 0; --i)
    {
        var argReg = argRegs[i];

        var src;

        if (i === 0)
        {
            if (backend.debugTrace === true)
                x86.genTracePrint(asm, genInfo.params, 'copying func arg');
            asm.mov(argReg, funcObj);
        }

        else if (i === 1)
        {
            if (backend.debugTrace === true)
                x86.genTracePrint(asm, genInfo.params, 'copying this arg');
            asm.mov(argReg, thisObj);
        }

        // This is a table argument
        else
        {
            var tableIdx = i - NUM_HIDDEN_ARGS;

            assert (
                tableIdx >= 0,
                'negative arg table index'
            );

            if (backend.debugTrace === true)
                x86.genTracePrint(asm, genInfo.params, 'arg count cmp');

            // Compare the table argument count to the table
            // index of the argument
            asm.cmp(numArgs, tableIdx);

            // if (numArgs > tableIdx)
            asm.cmovg(
                argReg,
                asm.mem(
                    backend.regSizeBits,
                    argTbl,
                    tblDisp + (tableIdx * backend.regSizeBytes)
                )
            );
        }
    }

    if (backend.debugTrace === true)
        x86.genTracePrint(asm, genInfo.params, 'calling w/ apply');

    // Call the function with the given address
    asm.call(funcPtr);

    if (backend.debugTrace === true)
        x86.genTracePrint(asm, genInfo.params, 'returned from call w/ apply');

    // Restore the old stack pointer
    // Note: at this point, the arguments should be popped off by the
    // callee, old sp should be at the top
    asm.mov(spReg, asm.mem(spReg.size, spReg, 0));

    // If the call has a continuation label
    if (instr.targets[0] !== undefined)
    {
        // Jump to the continuation label
        var thisBlock = instr.parentBlock;
        var contBlock = instr.targets[0];
        var contLabel = genInfo.edgeLabels.getItem({pred:thisBlock, succ:contBlock});
        asm.jmp(contLabel);
    }
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
    const calleeConv = genInfo.callConv;
    const allocMap = genInfo.allocMap;

    // Restore the callee-save registers, if any
    for (var i = 0; i < calleeConv.calleeSave.length; ++i)
    {
        var reg = calleeConv.calleeSave[i];
        var stackLoc = allocMap.getAllocs(reg)[0];
        var stackOpnd = allocMap.getSlotOpnd(stackLoc);
        asm.mov(reg, stackOpnd);
    }

    // Remove the spills from the stack
    asm.add(allocMap.spReg, allocMap.numSpillSlots * allocMap.slotSize);

    if (genInfo.backend.debugTrace === true)
        x86.genTracePrint(asm, genInfo.params, 'leaving "' + genInfo.irFunc.funcName + '"');

    // If the arguments should be removed by the callee
    if (calleeConv.cleanup === 'CALLEE')
    {
        // Return, popping the arguments from the stack
        var argSpace = allocMap.slotSize * allocMap.numArgSlots;
        asm.ret(argSpace);
    }
    else
    {
        // Add a return instruction
        asm.ret();
    }
}

// TODO:
// Not yet implemented
ThrowInstr.prototype.x86 = RetInstr.prototype.x86;

// TODO: CatchInstr

// Integer cast instruction
ICastInstr.prototype.x86 = new x86.InstrCfg();
ICastInstr.prototype.x86.opndMustBeReg = function (instr, idx, params)
{
    const srcWidth = instr.uses[0].type.getSizeBits(params);
    const dstWidth = instr.type.getSizeBits(params);

    // If the output is 8 bits, the input is not the same width as the input
    // and we are compiling for 32 bits, force the operand to be in al
    if (dstWidth === 8 && srcWidth !== dstWidth &&
        params.backend.x86_64 === false)
        return x86.regs.eax;

    return false;
}
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
    // we retain only the least significant bits
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

