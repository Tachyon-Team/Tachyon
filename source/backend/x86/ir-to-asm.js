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

Translate the low-level IR to machine dependent assembly code.

@copyright
Copyright (c) 2010 Tachyon Javascript Engine, All Rights Reserved
*/

/** @namespace */
var irToAsm = {};

/** 
    @private
    Returns an entry point for the function.
*/
irToAsm.getEntryPoint = function (irfunc, name, params, type)
{
    if (name === undefined)
    {
        name = "default";
    }

    if (type === undefined)
    {
        type = "addr";
    }
    
    const width = params.target.ptrSizeBits;
    var ep;

    function setEntryPoint(ep, name)
    {
        if (name === undefined)
            name = "default";

        this.entryPoints[name] = ep;
    };

    function getEntryPoint(name)
    {
        if (name === undefined)
            name = "default";

        return this.entryPoints[name];
    };

    const offset = params.target.ptrSizeBytes;
    function linkValueOffset(dstAddr) 
    { 
        var bytes = dstAddr
                    .addOffset(offset)
                    .getAddrOffsetBytes(this.srcAddr);
        return bytes;
    };


    // Assign an entry point to this irfunc
    // if it doesn't exist
    if (irfunc.linking.entryPoints === undefined)
    {
        irfunc.linking.entryPoints = {};
        irfunc.linking.setEntryPoint = setEntryPoint;
        irfunc.linking.getEntryPoint = getEntryPoint;
    }

    ep = irfunc.linking.entryPoints[name];

    if (ep === undefined)
    {
        ep = x86.Assembler.prototype.linked(
            irfunc.funcName + "_" + name,
            function (dstAddr) { return this.getAddr().getBytes(); },
            width
        );
        irfunc.linking.setEntryPoint(ep, name);
    }

    // Customize the return entry point object behavior
    assert(type === "addr" || type === "offset", "Invalid entry point type");
    
    if (type === "offset")
    {
        ep = Object.create(ep);
        ep.linkValue = linkValueOffset;       
    }
    
    return ep;
};


/**
@class
Returns an object allocating stack slots for spilling during
register allocation.
*/
irToAsm.spillAllocator = function (params)
{
    var that = Object.create(irToAsm.spillAllocator.prototype);
    that.slots = [];
    that.target = params.target;
    return that;
};
/** Returns a new assembly memory object */
irToAsm.spillAllocator.prototype.newSlot = function ()
{
    // Assembler imports
    const mem = x86.Assembler.prototype.memory; 

    // Memory is byte addressed
    const offset = this.slots.length * this.target.ptrSizeBytes;
    const s = mem(offset, this.target.backendCfg.stack);
    this.slots.push(s);
    return s;
};

/**
@private
Maker function for creating the genCode method of shift instructions
*/
irToAsm.shiftMaker = function (irinstr, name)
{
    // Register allocation hints and constraints
    irinstr.prototype.regAlloc = Object.create(irinstr.prototype.regAlloc);
   
    irinstr.prototype.regAlloc.usedRegisters = function (instr, params)
    {
        const width = params.target.ptrSizeBits;
        if (width === 32 && instr.uses[0] instanceof IRInstr)
        {
            // Block xDX
            return [2];
        } else
        {
            return null;
        }
    };

    // Code generation
    irinstr.prototype.genCode = function (tltor, opnds) 
    {
        // Assembler imports
        const reg = x86.Assembler.prototype.register;
        const $ = x86.Assembler.prototype.immediateValue;

        const width = this.type.getSizeBits(tltor.params);

        var dest = this.regAlloc.dest;

        if (dest === null)
        {
            dest = tltor.params.target.backendCfg.scratchReg;
        }

        var shiftAmt;
        if (opnds[1].type === x86.type.IMM_VAL)
        {
            assert(
                opnds[1].value > 0, 
                "Shift amount should be a positive integer"
            );

            shiftAmt = $(opnds[1].value % 256);
        } else
        {
            if (tltor.asm.target === x86.target.x86 &&
                opnds[1].type === x86.type.REG &&
                opnds[1] !== reg.eax &&
                opnds[1] !== reg.ebx &&
                opnds[1] !== reg.ecx &&
                opnds[1] !== reg.edx)
            {
                // x86 32 bits only permit access to lower 8 bits
                // of eax, ebx, ecx and edx 
                tltor.asm.
                // 32 bit move
                mov(opnds[1], reg.edx).
                // 8 bit move
                mov(reg.dl, reg.cl);
            } else if (opnds[1].type === x86.type.MEM)
            {
                tltor.asm.mov(opnds[1], reg.cl);
            } else
            {
                tltor.asm.mov(opnds[1].subReg(8), reg.cl);
            }
            shiftAmt = reg.cl;
        }

        if (opnds[0] === dest)
        {
            tltor.asm[name](shiftAmt, dest.subReg(width));
        }
        else
        {
            tltor.asm.
            mov(opnds[0], dest);

            tltor.asm[name](shiftAmt, dest.subReg(width));
        }

        if (opnds[1].type !== x86.type.IMM_VAL)
        {
            tltor.asm.mov($(0), reg.cl);
        }
    };
};

/**
@private
Maker function for creating the genCode method of bitwise instructions
*/
irToAsm.bitOpMaker = function (irinstr, name)
{
    /*
        if (tltor.asm.target === x86.target.x86_64 && opnds[1].type === x86.type.LINK)
        {
            tltor.asm.mov(opnds[1], scratchReg);
            var opnd = scratchReg;
        } else
        {
            var opnd = opnds[1];
        }
    */

    /*
    if ((src.type === x86.type.MEM || 
         (!tltor.asm.is32bitImm(src))) &&
        dest.type === x86.type.MEM)
    {
        tltor.asm.
        mov(src, scratchReg).
        mov(scratchReg, dest);
    } else
    {
        tltor.asm.
        mov(src, dest, width);
    }
    */

    // Code generation
    irinstr.prototype.genCode = function (tltor, opnds) 
    {
        const dest = this.regAlloc.dest;
        const scratchReg = tltor.params.target.backendCfg.scratchReg;

        const width = this.type.getSizeBits(tltor.params);

        var opndL = opnds[0];
        var opndR = opnds[1];

        assert (
            !(tltor.asm.isImmediate(opndL) && tltor.asm.isImmediate(opndR)),
            'no support for two immediate operands'
        );

        if (tltor.asm.isImmediate(opndL) && !tltor.asm.is32bitImm(opndL))
        {
            tltor.asm.mov(opndL, scratchReg);
            opndL = scratchReg;
        }

        else if (tltor.asm.isImmediate(opndR) && !tltor.asm.is32bitImm(opndR))
        {
            tltor.asm.mov(opndR, scratchReg);
            opndR = scratchReg;
        }

        if (opndL.type === x86.type.REG && opndL === dest)
        {
            tltor.asm[name](opndR, dest);
        }
        else if (opndR.type === x86.type.REG && opndR === dest)
        {
            tltor.asm[name](opndL, dest);
        } 
        else
        {
            tltor.asm.
            mov(opndL, dest)
            [name](opndR, dest, width);
        }
    };
};

//=============================================================================
//
// IR translator class definition
//
//=============================================================================

/**
@class
Returns a new translator object to translate IR to Assembly.
*/
irToAsm.translator = function (params)
{
    assert(
        params !== undefined,
        'invalid translator params'
    );

    var that = Object.create(irToAsm.translator.prototype);
    const $ = x86.Assembler.prototype.immediateValue;
    const mem = x86.Assembler.prototype.memory;
    const backendCfg = params.target.backendCfg;

    // Store the compilation parameters on the translator
    that.params = params;

    that.asm = new x86.Assembler(params.target.ptrSizeBits === 64 ? 
                                 x86.target.x86_64 : x86.target.x86);

    that.asm.useListing = params.printASM;
    that.asm.codeBlock.bigEndian = false;
    that.fct = null;

    // Use the context register value as a true (nonzero) boolean
    that.trueVal = backendCfg.stack;

    // The false boolean must be 0
    that.falseVal = $(0);

    // Get the memory location for the temporary register in the context
    var tempOffset = backendCfg.ctxLayout.getFieldOffset([backendCfg.tempName]);
    that.temp = mem(tempOffset, backendCfg.context);

    return that;
};

/** @private assembler object */
irToAsm.translator.prototype.asm = null;

/** 
Generate the corresponding assembly code for the function in the order
specified in blockList.
*/
irToAsm.translator.prototype.genFunc = function (fct, blockList)
{
    const that = this;
    const offset = this.params.target.ptrSizeBytes;
    const $ = x86.Assembler.prototype.immediateValue;

    // Maintain the function object throughout the translation
    // to have to information from register allocation 
    this.fct = fct;

    function replace(opnd, index)
    {
        // If this is a static function reference
        if (opnd instanceof IRFunction)
        {
            const isStaticCall = (this instanceof CallInstr && index === 0);
            const expNumArgs = opnd.argVars.length;
            const numArgs = this.uses.length - 3;
            const callType = (isStaticCall && expNumArgs === numArgs) ? 
                             "fast" : "default";
            // Get the function entry point
            return irToAsm.getEntryPoint(
                opnd,
                callType, 
                that.params,
                "addr"
            );
        }

        // If this is a string
        else if (opnd instanceof ConstValue && typeof opnd.value === "string")
        {
            return that.stringValue(opnd.value);
        }
 
        else if (opnd instanceof ConstValue)
        {
            return $(opnd.getImmValue(that.params));
        }

        else 
        {
            return opnd;
        }
    };

    this.asm.genListing('<fn:' + fct.funcName + '>');

    // Start the code generation
    this.prelude();

    blockList.forEach(function (block)
    {
        // Generate the asm label for the current block 
        that.asm.label(that.label(block, block.label));

        // Generate all the asm instructions for each IR
        // instructions
        block.getInstrItr().forEach(function (instr)
        {
            var opnds;

            if (instr.regAlloc.opnds === null)
            {
                opnds = instr.uses.map(replace, instr);

                instr.genCode(that, opnds);
            } 
            else
            {
                // Replace constants by immediate values
                opnds = instr.regAlloc.opnds.map(replace, instr);

                assert(
                    instr.genCode !== undefined,
                    'genCode method not present'
                );

                instr.genCode(that, opnds);
            }
        });

        that.asm.genListing("");
    });
};

/** 
    @private
    Returns a label for the object. If a label was 
    previously defined, the same label will be returned.
*/
irToAsm.translator.prototype.label = function (obj, name)
{
    var label;
    // Assign a unique label to this obj 
    // if it doesn't exist
    if (obj.irToAsm === undefined)
    {
        obj.irToAsm = {};
    }

    label = obj.irToAsm.label;

    if (label === undefined)
    {
        label = ((name === undefined) ? this.asm.labelObj() : 
                                        this.asm.labelObj(name));
        obj.irToAsm.label = label;
    }

    return label;
};

/**
    @private
    Returns a representation of the string that fits into a register
*/
irToAsm.translator.prototype.stringValue = function (s)
{
    var that = this;
    return this.asm.linked(
        '"' + s + '"',
        function ()
        {
            if (that.params.getStrObj instanceof Function)
            {
                return that.params.getStrObj(s);
            }
            else
            {
                return asm.address.
                       nullAddr(that.params.target.ptrSizeBits).getBytes();
            }
        },
        that.params.target.ptrSizeBits
    );
};

/**
    @private
    Returns an array of unused registers
*/
irToAsm.translator.prototype.freeRegisters = function (used)
{
    // Configuration imports
    const backendCfg = this.params.target.backendCfg;
    const freeRegs = backendCfg.physReg.slice(0);
    arraySetRemAll(freeRegs, used);
    return freeRegs;
};

/**
    @private
    
    Move all operands in 'wanted' on context, replacing their occurence
    in 'used' by their memory location on context.
    
    Returns all available free registers after operation.
*/
irToAsm.translator.prototype.spillOnCtx = function (wanted, used)
{
    const that = this;
    // Assembler imports
    const mem = x86.Assembler.prototype.memory;
    
    // Configuration imports
    const backendCfg = this.params.target.backendCfg;
    const ctxLayout = backendCfg.ctxLayout;
    const context = backendCfg.context;

    assert(
        wanted.length <= backendCfg.physReg.length, 
        "Insufficient number of slots available on context"
    );

    // Allow destructive modification
    wanted = wanted.slice(0);

    const free = this.freeRegisters(used);
    var temp;
    var tempIdx = -1;
    var i = 0;
    function spill(index)
    {
        if (index === -1)
        {
            // Nothing to spill
            return;
        }

        const slot = used[index];

        const ctxSlotOffset = ctxLayout.getFieldOffset(["reg" + i]);
        const ctxSlot = mem(ctxSlotOffset, context);
        if (slot.type === x86.type.REG)
        {
            that.asm.
            mov(slot, ctxSlot); 

            free.push(slot);
        } else
        {
            that.asm.
            mov(slot, temp).
            mov(temp, ctxSlot);
        }

        used[index] = ctxSlot;

        i++;
        return ctxSlot;
    }

    if (free.length === 0)
    {
        // Move a register value on context first
        tempIdx = arrayFind(used, function (u)
        {
            return u.type === x86.type.REG;
        });
        temp = spill(tempIdx);
    } else 
    {
        temp = free[0]; 
    }

    wanted.forEach(function (slot) {
        const index = used.indexOf(slot);

        if (index !== tempIdx)
        {
            spill(index); 
        }
    });

    return free;
};

/**
    Generate code to move arguments passed in registers on stack,
    generate some code and move arguments back in registers.
*/
irToAsm.translator.prototype.withNormalizedFrame = function (code)
{
    const $   = x86.Assembler.prototype.immediateValue;
    const mem = x86.Assembler.prototype.memory;

    const that = this;

    const target     = this.params.target;
    const backendCfg = target.backendCfg;
    const argsRegNb  = backendCfg.argsReg.length;
    const refByteNb  = target.ptrSizeBytes; 

    const stack   = backendCfg.stack;
    const scratch = backendCfg.scratchReg;
    const argsReg = backendCfg.argsReg;

    /*
    At first we have the following stack frame:

    -----------------
    Return Address     <-- stack pointer
    -----------------
    Arguments (only those not passed in registers)
    -----------------


    In the end, we want the following stack frame:

    -----------------
    Return Address     <-- stack pointer
    -----------------
    Arguments passed in registers
    Arguments passed on stack
    -----------------
    */

    const retAddrOrigOffset = (argsRegNb*refByteNb);
    const retAddrNewOffset  = 0;

    // Change return address location on stack
    if (retAddrOrigOffset !== retAddrNewOffset)
    {
        // Create space on stack for the arguments in registers
        this.asm.
        sub($(argsRegNb*refByteNb), stack);

        // Move return address
        this.asm.
        mov(mem(retAddrOrigOffset, stack), scratch).
        mov(scratch, mem(retAddrNewOffset, stack));

        // Push each argument passed in registers on stack
        // (overwrites the previous return address location)
        argsReg.forEach(function (reg, index) {
            that.asm.
            mov(reg, mem((index + 1)*refByteNb, stack));
        });
    }

    code.call(this);

    // Restore stack frame
    if (retAddrOrigOffset !== retAddrNewOffset)
    {
        // Move arguments back in registers
        argsReg.forEach(function (reg, index) {
            that.asm.
            mov(mem((index + 1)*refByteNb, stack), reg);
        });

        // Restore return address
        this.asm.
        mov(mem(retAddrNewOffset, stack), scratch).
        mov(scratch, mem(retAddrOrigOffset, stack)).
        add($(argsRegNb*refByteNb), stack);
    }
};

/**
    
*/
irToAsm.translator.prototype.populateArgTable = function ()
{
    const $   = x86.Assembler.prototype.immediateValue;
    const mem = x86.Assembler.prototype.memory;
    const reg = x86.Assembler.prototype.register;
    const target     = this.params.target;
    const backendCfg = target.backendCfg;
    const context = backendCfg.context;
    const width = target.ptrSizeBits;
    const implArgsRegNb = backendCfg.implArgsRegNb;
    const ctxNumArgs = backendCfg.getCtxField("numargs");
    const ctxArgTbl  = backendCfg.getCtxField("argtbl");
    const refByteNb = target.ptrSizeBytes; 
    const tblOffset = this.params.memLayouts.arrtbl.getFieldOffset(
                         ["tbl", 0]
                      );

    const allocArgTbl   = irToAsm.getEntryPoint(
                            this.params.staticEnv.getBinding("allocArgTable"),
                            "fast",
                            this.params, 
                            "addr"
                        );

    // The return address and implementation related values are also on 
    // stack but should not be copied in argument table
    const spOffset = (implArgsRegNb + 1)*refByteNb;

    const index      = backendCfg.argsReg[0];
    const argTblPtr  = backendCfg.retValReg;
    const scratch    = backendCfg.scratchReg;
    const stack      = backendCfg.stack;
    const fstOpnd    = backendCfg.argsReg[implArgsRegNb];

    assert(
        index !== argTblPtr,
        "Incompatible index and argTblPtr"
    );

    this.asm.
    // Move numArgs into corresponding register
    mov(ctxNumArgs, fstOpnd).

    // Preserve the number of argument accross calls
    push(fstOpnd).
    
    // Only retain user-visible arguments nb
    sub($(implArgsRegNb), fstOpnd).

    // Call handler, result is in argTblPtr
    mov(allocArgTbl, scratch).
    call(scratch).

    // Restore the number of arguments
    pop(index).          
    mov(index, ctxNumArgs).

    // Copy arguments into argument table
    sub($(1 + implArgsRegNb), index).    // i = ctxNumArgs - 1 - implArgsRegNb

    forLoop(index, $(0), function ()    // for (;i >= 0; --i)
    {
        this.
        mov(mem(spOffset, stack, index, refByteNb), scratch). // temp = sp[i] + 3
        mov(scratch, mem(tblOffset, argTblPtr, index, refByteNb));   // argTbl[i] = temp
    }).

    // Store argument table on the context
    mov(argTblPtr, ctxArgTbl);

};

irToAsm.translator.prototype.prelude = function ()
{
    const reg = x86.Assembler.prototype.register;
    const mem = x86.Assembler.prototype.memory;
    const $ = x86.Assembler.prototype.immediateValue;

    const that = this; 
    const target = this.params.target;
    const width = target.ptrSizeBits;
    const refByteNb = target.ptrSizeBytes;
    const backendCfg = target.backendCfg;
    const stack = backendCfg.stack;
    const context = backendCfg.context;

    const cstack =  reg.rsp.subReg(width);
    const scratch  = backendCfg.nonArgsReg[0];
    const scratch2 = backendCfg.scratchReg;

    const argsReg   = backendCfg.argsReg;
    const argsRegNb = backendCfg.argsReg.length;
    const spillNb = this.fct.regAlloc.spillNb;

    // Add an entry point for default static calls
    var lobj = irToAsm.getEntryPoint(this.fct, undefined, this.params);
    this.asm.provide(lobj);

    if (this.fct.cProxy)
    {
        if (this.asm.target === x86.target.x86)
        {
            // We follow 32 bits Windows, Linux, BSD and Mac OS X
            // callee-save convention
            this.asm.
            push(reg.ebx).
            push(reg.esi).
            push(reg.edi).
            push(reg.ebp);
        }
        else
        {
            // Push the arguments that were passed in registers on stack
            for (var i= Math.min(this.fct.argVars.length-1, 5); i >= 0; --i)
            {
                this.asm.
                push(backendCfg.x64ArgsReg[i]);
            }

            // We follow 64 bits Linux, BSD, Mac OS X
            // callee-save convention
            this.asm.
            push(reg.rbx).
            push(reg.rbp).
            push(reg.r12).
            push(reg.r13).
            push(reg.r14).
            push(reg.r15);
        }

        // Put the stack pointer where tachyon expects it to be
        if (cstack !== stack)
        {
            this.asm.
            mov(cstack, stack);
        }
    } else
    {
        const a0  = backendCfg.argsReg[0];
        const a1  = backendCfg.argsReg[1];

        const numArgsOffset = backendCfg.ctxLayout.getFieldOffset(["numargs"]);
        const numArgs = mem(numArgsOffset, context);

        const tempOffset = backendCfg.ctxLayout.getFieldOffset(["temp"]);
        const ctxTemp = mem(tempOffset, context);

        if (this.fct.usesArguments)
        {
            this.withNormalizedFrame(this.populateArgTable);
        }
        
        // Handle a variable number of arguments
        const frameOk = this.asm.labelObj();
        const frameTooBig   = this.asm.labelObj();
        const tbDone        = this.asm.labelObj();
        const tbLoop        = this.asm.labelObj();
        const tsDone        = this.asm.labelObj();
        const tsLoop        = this.asm.labelObj();

        
        const expNumArgs  = this.fct.argVars.length;
        const expCallArgs = expNumArgs + 2; 
        const frameNumArgs = expCallArgs - argsRegNb;

        const argOffset = scratch;
        const argPtr    = scratch2;
        const temp      = a0;
        const undefImm = $(this.params.staticEnv
                          .getBinding("BIT_PATTERN_UNDEF").value);
        const shiftAmt = (refByteNb === 4) ? 2 : 3;

        this.asm.
        mov(numArgs, argOffset).
        sub($(expCallArgs), argOffset).
        test(argOffset, argOffset).
        je(frameOk);

        this.asm.
        // Frame is either too big or too small
        cmp($(0), argOffset).
        jg(frameTooBig);


        const undef = scratch2;

        this.asm.
        mov(undefImm, undef).
        mov(numArgs, scratch);

        // Handle the case where some registers should be
        // initialized to undefined
        argsReg.forEach(function (reg, index)
        {
            if (expCallArgs >= index + 1)
            {
                that.asm.
                cmp($(index), scratch).
                cmovle(undef, reg); 
            }
        });

        if (expCallArgs > argsRegNb)
        {
            const index = a1;

            this.asm.

            mov(temp, ctxTemp).
            mov($(0), temp).

            // Calculate the remaining amount of undefined values to
            // add on stack
            mov($(argsRegNb), argPtr).
            sub(numArgs, argPtr).
            cmovl(temp, argPtr).

            mov(numArgs, argOffset).
            sub($(expCallArgs), argOffset).
            add(argPtr, argOffset).
            mov(ctxTemp, temp).

            // Adjust stack
            lea(mem(0,stack,argOffset,refByteNb), stack).
            // Initialize argument iterator
            mov(stack, argPtr).
            // Preserve registers
            push(temp).
            push(index).
            // Take the positive value
            not(argOffset).
            inc(argOffset).
            // Initialize index to the return address position
            mov($(frameNumArgs), index).

            // For each stack frame slot on stack
            label(tsLoop).
            cmp($(0), index).
            jl(tsDone).

            // If it is already defined, copy it, 
            // otherwise set it to undefined
            cmp(argOffset, index).
            mov(undefImm, temp).
            cmovnl(mem(0,argPtr,argOffset,refByteNb), temp).
            mov(temp, mem(0,argPtr)).

            // Next stack frame slot
            add($(refByteNb), argPtr).
            dec(index).
            jmp(tsLoop).

            label(tsDone).
            // Restore registers
            pop(index).
            pop(temp);
        }

        this.asm.
        jmp(frameOk).

        label(frameTooBig);


        // Frame is too big


        // If some arguments were passed on the stack
        this.asm.
        mov(temp, ctxTemp).
        mov(stack, argPtr);

        if (expCallArgs < argsRegNb)
        {
            this.asm.
            // Remove from the offset the arguments
            // that were passed in registers
            sub($(argsRegNb - expCallArgs), argOffset).

            // If the argOffset is less or equal to zero,
            // there is nothing to be done
            cmp($(0), argOffset).
            jle(frameOk);
        }

        if (frameNumArgs > 0)
        {
            this.asm.
            add($(frameNumArgs*refByteNb), argPtr);
        }

        this.asm.
        // Copy slot values to their new location 
        label(tbLoop).
        cmp(stack, argPtr).
        jl(tbDone).

        // Move the value on the stack
        mov(mem(0,argPtr), temp).
        mov(temp, mem(0,argPtr, argOffset, refByteNb)).
        sub($(refByteNb), argPtr).
        
        jmp(tbLoop).
        label(tbDone). 

        // Restore the value in register
        mov(ctxTemp, temp).
        
        // Adjust stack pointer
        sal($(shiftAmt), argOffset).
        add(argOffset, stack);

        this.asm.
        label(frameOk);
    }

    // Sets a handler entry point to avoid the previous overhead for static
    // calls to a handler
    const fastEp = irToAsm.getEntryPoint(
                        this.fct,
                        "fast",
                        this.params
                    );
    this.asm.
    provide(fastEp);

    if (spillNb > 0)
    {
        this.asm.sub($(spillNb*refByteNb), stack);
    }
};

//=============================================================================
//
// Translation of IR instructions to x86 machine code
//
//=============================================================================
IRInstr.prototype.regAlloc = regAlloc.instr(); 

/* code generation for each ir instruction */
PhiInstr.prototype.genCode = function (tltor, opnds)
{
    // Do nothing
};

/**
*   Allocation information for argument value instructions 
*/
ArgValInstr.prototype.regAlloc = Object.create(IRInstr.prototype.regAlloc);

ArgValInstr.prototype.regAlloc.outRegHint = function (instr, params)
{
    const backendCfg = params.target.backendCfg;

    if (instr.argIndex < backendCfg.argsIndex.length)
    {
        return backendCfg.argsIndex[instr.argIndex];
    } 
    else 
    {
        return null;
    }
};

ArgValInstr.prototype.genCode = function (tltor, opnds)
{
    // Configuration imports
    const target = tltor.params.target;
    const backendCfg = target.backendCfg;

    // Assembler imports
    const mem = x86.Assembler.prototype.memory;

    // Register used for the return value
    const dest = this.regAlloc.dest;

    // Index of the current argument
    const argIndex = this.argIndex;

    // Array of registers reserved for passing arguments
    const argsReg = backendCfg.argsReg;

    // Number of registers used for passing arguments
    const argRegNb = (tltor.fct.cProxy === true) ? 0 
                     : backendCfg.argsIndex.length;

    // Number of bytes in a reference
    const refByteNb = target.ptrSizeBytes;

    // Number of variables spilled during register allocation
    const regAllocSpillNb = tltor.fct.regAlloc.spillNb; 

    // Index on the call site argument space
    const callSiteArgIndex = (argIndex - argRegNb);
    
    // Offset due to spilling of callee-save registers
    // from C calling convention 
    const ccallOffset = (tltor.fct.cProxy === true) ?
                        ((tltor.asm.target === x86.target.x86) ? 4 : 6) : 0;

    // On 64 bits, we spill arguments passed in registers on stack
    // at the entry of the function
    const retAddrOffset = (tltor.fct.cProxy === true &&
                           tltor.asm.target === x86.target.x86_64 &&
                           argIndex < 6) ? 0 : 1;

    // Offset to the argument on the stack
    const spoffset = (regAllocSpillNb + callSiteArgIndex + 
                      ccallOffset + retAddrOffset) * refByteNb;

    // Stack pointer
    const stack = backendCfg.stack;

    // Ignore if the argument is not required
    if (dest === null)
    {
        return;
    }

    if ((argIndex < argRegNb))
    {
        // The argument is in a register
        assert(
            argsReg[argIndex] === dest,
            "arg: dest register '" + dest + 
            "' unexpected for argument index '" + argIndex + "'"
        );
    } 
    else
    {
        // The argument is on the stack
        tltor.asm.
        mov(mem(spoffset, stack), dest); 
    }
};

AddInstr.prototype.genCode = function (tltor, opnds)
{
    // Register used for the output value
    var dest = this.regAlloc.dest;

    if (dest === null)
    {
        dest = tltor.params.target.backendCfg.scratchReg;
    }

    if (opnds[1].type === x86.type.IMM_VAL)
    {
        // Case where one of the operands is an immediate
        // value

        if (dest !== opnds[0])
        {
            tltor.asm.mov(opnds[0], dest);
        }

        if (opnds[1].value === 1)
        {
            tltor.asm.inc(dest);
        } 
        else
        {
            tltor.asm.add(opnds[1], dest);
        }
    } 
    else if (opnds[1].type === x86.type.REG && opnds[1] === dest)
    {
        tltor.asm.add(opnds[0], dest);
    } 
    else
    {
        if (opnds[0] !== dest)
        {
            tltor.asm.mov(opnds[0], dest);
        }
   
        tltor.asm.add(opnds[1], dest);
    }
};

SubInstr.prototype.genCode = function (tltor, opnds)
{
    // Register used for the output value
    var dest = this.regAlloc.dest;
 
    if (dest === null)
    {
        dest = tltor.params.target.backendCfg.scratchReg;
    }

    if (opnds[1].type === x86.type.IMM_VAL)
    {
        // Case where one of the operands is an immediate
        // value

        if (dest !== opnds[0])
        {
            tltor.asm.mov(opnds[0], dest);
        }

        if (opnds[1].value === 1)
        {
            tltor.asm.dec(dest);
        } 
        else
        {
            tltor.asm.sub(opnds[1], dest);
        }
    } 
    else if (opnds[0] === opnds[1])
    {
        // Operands are the same, put a zero in the destination register
        tltor.asm.xor(dest, dest); 
    } 
    else if (opnds[1].type === x86.type.REG && opnds[1] === dest)
    {
        // Operands are inverted with regard to x86 notation 

        // TODO: Change when register allocation spilling is done differently
        tltor.asm.
        mov(opnds[1], tltor.temp).
        mov(opnds[0], dest).
        sub(tltor.temp, dest);
    }
    else
    {
        if (opnds[0] !== dest)
        {
            tltor.asm.mov(opnds[0], dest);
        }
   
        tltor.asm.sub(opnds[1], dest);
    }
};

/**
Allocation information for multiplication instruction
*/
MulInstr.prototype.regAlloc = Object.create(IRInstr.prototype.regAlloc);

MulInstr.prototype.regAlloc.opndsRegHint = function (instr, params, position)
{
    if (instr.type.isSigned())
        return null;

    // Operand 0 should be placed in xAX if possible (not guaranteed)
    if (position === 0) 
        return 0;
    else
        return null;
};

MulInstr.prototype.regAlloc.outRegHint = function (instr, params)
{
    if (instr.type.isSigned())
        return null;

    // The output will be in xAX
    return 0; 
};

MulInstr.prototype.regAlloc.usedRegisters = function (instr, params) 
{
    if (instr.type.isSigned())
        return null;
 
    // xDX:xAX are reserved for the multiplier,
    return [0,2];
};

MulInstr.prototype.genCode = function (tltor, opnds)
{
    // Get the operand width
    var width;
    if (opnds[0].width !== undefined)
        width = opnds[0].width();
    else if (opnds[1].width !== undefined)
        width = opnds[1].width();
    else
        width = this.uses[0].type.getSizeBits(tltor.params);

    const reg = x86.Assembler.prototype.register;
    const xAX = reg.rax.subReg(width);
    const xDX = reg.rdx.subReg(width);

    // Register used for the output value
    var dest = this.regAlloc.dest;

    if (dest === null)
    {
        dest = tltor.params.target.backendCfg.scratchReg;
    }

    // If an unsigned integer result is expected
    if (this.type.isUnsigned())
    {
        // Make sure that one of the operands is in EAX
        if (opnds[0] === xAX)
        {
            var op1 = opnds[1];
        }
        else if (opnds[1] === xAX)
        {
            var op1 = opnds[0];
        }
        else
        {
            // Put operand 0 in eax
            tltor.asm.mov(opnds[0], xAX);
            var op1 = opnds[1];
        }
        
        // If operand 1 is an immediate value, put it into EDX
        if (op1.type === x86.type.IMM_VAL)
        {
            tltor.asm.mov(op1, xDX);
            var op1 = xDX;
        }

        tltor.asm.mul(op1, width);
    }

    // Otherwise, a signed result is expected
    else
    {
        if (opnds[0].type === x86.type.IMM_VAL)
        {
            tltor.asm.imul(
                opnds[1], 
                dest, 
                opnds[0], 
                width
            );
        }

        else if (opnds[1].type === x86.type.IMM_VAL)
        {
            tltor.asm.imul(
                opnds[0], 
                dest, 
                opnds[1], 
                width
            );
        }

        else if (opnds[0] === dest)
        {
            tltor.asm.imul(
                opnds[1], 
                dest, 
                undefined, 
                width
            );
        }

        else if (opnds[1] === dest)
        {
            tltor.asm.imul(
                opnds[0], 
                dest, 
                undefined, 
                width
            );
        }

        else
        {
            tltor.asm.mov(opnds[0], dest);
            tltor.asm.imul(
                opnds[1], 
                dest, 
                undefined, 
                width
            );
        }
    }
};

/**
Allocation information for division instruction
*/
DivInstr.prototype.regAlloc = Object.create(IRInstr.prototype.regAlloc);

DivInstr.prototype.regAlloc.opndsRegHint = function (instr, params, position)
{
    // Operand 0 should be placed in xAX if possible (not guaranteed)
    if (position === 0) 
        return 0;
    else if (position === 1)
        return 1;
    else
        return null;
};

DivInstr.prototype.regAlloc.outRegHint =  function (instr, params)
{ 
    // The output will be in xAX
    return 0; 
};

DivInstr.prototype.regAlloc.usedRegisters = function (instr, params) 
{ 
    // xDX:xAX are blocked for the dividend
    return [0,2];
};

DivInstr.prototype.genCode = function (tltor, opnds)
{
    // Configuration imports
    //const width = tltor.params.target.ptrSizeBits;
    // Get the operand width
    var width;
    if (opnds[0].width !== undefined)
        width = opnds[0].width();
    else if (opnds[1].width !== undefined)
        width = opnds[1].width();
    else
        width = this.uses[0].type.getSizeBits(tltor.params);

    // Assembler imports
    const reg = x86.Assembler.prototype.register;
    const $ = x86.Assembler.prototype.immediateValue;

    // In the end, we want to have:
    // - Dividend in xAX
    // - Divisor NOT in xAX or xDX

    // We have no support for sign extension on 
    // less than 32 bits for now
    assert(
        width >= 32,
        "Unsupported width " + width
    );
    
    var dvnd = opnds[0];
    var dsor = opnds[1]; 
    const scratchReg = tltor.params.target.backendCfg.scratchReg;

    var xAX = reg.rax.subReg(width);
    var xDX = reg.rdx.subReg(width);

    if (dsor === xAX && dvnd.type === x86.type.REG && dvnd !== xDX)
    {
        tltor.asm.
        xchg(xAX, dvnd);
        dsor = dvnd;
    } else
    {
        if (dsor === xAX || dsor === xDX || tltor.asm.isImmediate(dsor))
        {
            tltor.asm.
            mov(dsor, scratchReg);
            dsor = scratchReg;
        }

        if (dvnd !== xAX)
        {
            tltor.asm.
            mov(dvnd, xAX);
        }
    }
    
    // If the output should be unsigned, use unsigned divide, otherwise
    // use signed divide 
    if (this.type.isUnsigned())
    {
        // Extend the value into xDX
        tltor.asm.mov($(0), xDX);

        tltor.asm.div(dsor, width);
    }
    else
    {
        // Sign-extend xAX into xDX:xAX using CDQ or CQO
        if (width === 32)
        {
            tltor.asm.cdq();
        } else
        {
            tltor.asm.cqo();
        }

        tltor.asm.idiv(dsor, width);
    }
};

/**
Allocation information for modulo instruction
*/
ModInstr.prototype.regAlloc = Object.create(DivInstr.prototype.regAlloc);

ModInstr.prototype.regAlloc.outRegHint =  function (instr, params)
{ 
    // The output will be in xDX
    return 2; 
};

// Same code as division instruction, different register hint for output
ModInstr.prototype.genCode = DivInstr.prototype.genCode;

AddOvfInstr.prototype.genCode = function (tltor, opnds)
{
    // Reuse the implementation of the addition without overflow
    AddInstr.prototype.genCode.apply(this, [tltor, opnds]);

    const normalTarget = this.targets[0];
    const overflowTarget = this.targets[1];

    // Handle jump to exception
    tltor.asm.
    jno(tltor.label(normalTarget, normalTarget.label)).
    jmp(tltor.label(overflowTarget, overflowTarget.label));
};

SubOvfInstr.prototype.genCode = function (tltor, opnds)
{
    // Reuse the implementation of the subtraction without overflow
    SubInstr.prototype.genCode.apply(this, [tltor, opnds]);

    const normalTarget = this.targets[0];
    const overflowTarget = this.targets[1];

    // Handle jump to exception
    tltor.asm.
    jno(tltor.label(normalTarget, normalTarget.label)).
    jmp(tltor.label(overflowTarget, overflowTarget.label));
};

/**
Allocation information for multiplication with overflow instruction
*/
MulOvfInstr.prototype.regAlloc = Object.create(MulInstr.prototype.regAlloc);

MulOvfInstr.prototype.genCode = function (tltor, opnds)
{
    // Reuse the implementation of the multiplication without overflow
    MulInstr.prototype.genCode.apply(this, [tltor, opnds]);

    const normalTarget = this.targets[0];
    const overflowTarget = this.targets[1];

    // Handle jump to exception
    tltor.asm.
    jno(tltor.label(normalTarget, normalTarget.label)).
    jmp(tltor.label(overflowTarget, overflowTarget.label));
};

LsftOvfInstr.prototype.genCode = function (tltor, opnds)
{
    // Reuse the implementation of the left shift without overflow
    LsftInstr.prototype.genCode.apply(this, [tltor, opnds]);

    const normalTarget = this.targets[0];
    const overflowTarget = this.targets[1];

    // Handle jump to exception
    tltor.asm.
    jno(tltor.label(normalTarget, normalTarget.label)).
    jmp(tltor.label(overflowTarget, overflowTarget.label));
};

NotInstr.prototype.genCode = function (tltor, opnds)
{
    const dest = this.regAlloc.dest;

    if (dest !== opnds[0])
    {
        tltor.asm.mov(opnds[0], dest);
    }

    tltor.asm.not(dest);
};

irToAsm.bitOpMaker(AndInstr, "and"); 
irToAsm.bitOpMaker(OrInstr, "or"); 
irToAsm.bitOpMaker(XorInstr, "xor");

irToAsm.shiftMaker(LsftInstr, "sal"); 
irToAsm.shiftMaker(RsftInstr, "sar"); 
irToAsm.shiftMaker(UrsftInstr, "shr"); 

LtInstr.prototype.genCodeCmp = function (tltor, opnds)
{
    const dest = this.regAlloc.dest;

    assert(
        opnds[0].type !== x86.type.LINK &&
        opnds[1].type !== x86.type.LINK,
        "Invalid link object as operand"
    );
       
    // Get the operand width
    var width;
    if (opnds[0].width !== undefined)
        width = opnds[0].width();
    else if (opnds[1].width !== undefined)
        width = opnds[1].width();
    else
        width = this.uses[0].type.getSizeBits(tltor.params);

    if ((opnds[0].type === x86.type.MEM &&
        opnds[1].type === x86.type.MEM) ||
        (tltor.asm.isImmediate(opnds[0]) &&
         tltor.asm.isImmediate(opnds[1])))
    {
        tltor.asm.
        mov(opnds[0], dest).
        cmp(opnds[1], dest);
    } 
    else if (opnds[0].type === x86.type.IMM_VAL)
    {
        tltor.asm.
        cmp(opnds[0], dest);
    } 
    else
    {
        tltor.asm.cmp(opnds[1], opnds[0], width);
    }
};

LtInstr.prototype.genCode = function (tltor, opnds)
{
    this.genCodeCmp(tltor, opnds);

    const dest = this.regAlloc.dest;
    tltor.asm.
    mov(tltor.falseVal, dest);

    if (this.uses[0].type.isSigned() || this.uses[0].type === IRType.box)
        tltor.asm.cmovl(tltor.trueVal, dest);
    else
        tltor.asm.cmovb(tltor.trueVal, dest);
};

LeInstr.prototype.genCodeCmp = LtInstr.prototype.genCodeCmp;
LeInstr.prototype.genCode = function (tltor, opnds)
{
    this.genCodeCmp(tltor, opnds);

    const dest = this.regAlloc.dest;
    tltor.asm.
    mov(tltor.falseVal, dest);

    if (this.uses[0].type.isSigned() || this.uses[0].type === IRType.box)
        tltor.asm.cmovle(tltor.trueVal, dest);
    else
        tltor.asm.cmovbe(tltor.trueVal, dest);
};

GtInstr.prototype.genCodeCmp = LtInstr.prototype.genCodeCmp;
GtInstr.prototype.genCode = function (tltor, opnds)
{
    this.genCodeCmp(tltor, opnds);

    const dest = this.regAlloc.dest;
    tltor.asm.
    mov(tltor.falseVal, dest);

    if (this.uses[0].type.isSigned() || this.uses[0].type === IRType.box)
        tltor.asm.cmovnle(tltor.trueVal, dest);
    else
        tltor.asm.cmovnbe(tltor.trueVal, dest);
};

GeInstr.prototype.genCodeCmp = LtInstr.prototype.genCodeCmp;
GeInstr.prototype.genCode = function (tltor, opnds)
{
    this.genCodeCmp(tltor, opnds);

    const dest = this.regAlloc.dest;
    tltor.asm.
    mov(tltor.falseVal, dest);

    if (this.uses[0].type.isSigned() || this.uses[0].type === IRType.box)
        tltor.asm.cmovnl(tltor.trueVal, dest);
    else
        tltor.asm.cmovnb(tltor.trueVal, dest);
};

EqInstr.prototype.genCodeCmp = function (tltor, opnds)
{
    const scratchReg = tltor.params.target.backendCfg.scratchReg;
    const dest = this.regAlloc.dest;

    // Get the operand width
    var width;
    if (opnds[0].width !== undefined)
        width = opnds[0].width();
    else if (opnds[1].width !== undefined)
        width = opnds[1].width();
    else
        width = this.uses[0].type.getSizeBits(tltor.params);

    if (opnds[0].type === x86.type.REG && 
        opnds[1].type === x86.type.IMM_VAL &&
        opnds[1].value === 0) 
    {
        tltor.asm.test(opnds[0], opnds[0]);
    } 
    else if (opnds[1].type === x86.type.REG && 
             opnds[0].type === x86.type.IMM_VAL &&
             opnds[0].value === 0)
    {
        tltor.asm.test(opnds[1], opnds[1]);
    } 
    else if ((opnds[0].type === x86.type.MEM || tltor.asm.isImmediate(opnds[0])) &&
             (opnds[1].type === x86.type.MEM || tltor.asm.isImmediate(opnds[1])))
    {
        if (tltor.asm.target === x86.target.x86_64 && opnds[1].type === x86.type.LINK)
        {
            tltor.asm.mov(opnds[1], scratchReg);
            var opnd = scratchReg;
        } else
        {
            var opnd = opnds[1];
        }
        tltor.asm.
        mov(opnds[0], dest).
        cmp(opnd, dest);
    } 
    else if (tltor.asm.isImmediate(opnds[1]))
    {
        if (opnds[1].type === x86.type.LINK && opnds[1].width() === 64)
        {
            // Cmp cannot have a 64 bit immediate value as operand
            tltor.asm.
            mov(opnds[1], scratchReg).
            cmp(scratchReg, opnds[0]);
        } else
        {
            tltor.asm.cmp(opnds[1], opnds[0], width);
        }
    }
    else
    {
        if (opnds[0].type === x86.type.LINK && opnds[0].width() === 64)
        {
            // Cmp cannot have a 64 bit immediate value as operand
            var opnd = scratchReg;
            tltor.asm.mov(opnds[0], scratchReg);

        } else
        {
            var opnd = opnds[0];
        }
        tltor.asm.cmp(opnd, opnds[1], width);
    }
};

EqInstr.prototype.genCode = function (tltor, opnds)
{
    this.genCodeCmp(tltor, opnds);

    const dest = this.regAlloc.dest;
    tltor.asm.
    mov(tltor.falseVal, dest).
    cmovz(tltor.trueVal, dest);
};

NeInstr.prototype.genCodeCmp = EqInstr.prototype.genCodeCmp;
NeInstr.prototype.genCode = function (tltor, opnds)
{
    this.genCodeCmp(tltor, opnds);

    const dest = this.regAlloc.dest;
    tltor.asm.
    mov(tltor.falseVal, dest).
    cmovnz(tltor.trueVal, dest);
};

JumpInstr.prototype.genCode = function (tltor, opnds)
{
    const cont = this.targets[0];
    tltor.asm.jmp(tltor.label(cont, cont.label));
};

/**
*   Allocation information for return instructions   
*/
RetInstr.prototype.regAlloc = Object.create(IRInstr.prototype.regAlloc);

RetInstr.prototype.regAlloc.opndsRegHint = function (instr, params, position)
{
    return params.target.backendCfg.retValIndex;
};

RetInstr.prototype.regAlloc.opndsRegRequired = true;

RetInstr.prototype.genCode = function (tltor, opnds)
{
    // Assembler imports
    const reg = x86.Assembler.prototype.register;
    const $ = x86.Assembler.prototype.immediateValue;

    // Configuration imports
    const target = tltor.params.target;
    const backendCfg = tltor.params.target.backendCfg;
    const width = target.ptrSizeBits;
    const refByteNb = target.ptrSizeBytes;
    const spillNb = tltor.fct.regAlloc.spillNb;

    // Register used for the return value
    const retValReg = (tltor.fct.cProxy === false) ? 
                      backendCfg.retValReg : reg.rax.subReg(width);
    const cstack = reg.rsp.subReg(width);
    const stack = backendCfg.stack;

    // Remove all spilled values from stack
    if (spillNb > 0)
    {
        tltor.asm.add($(spillNb*refByteNb), stack);
    }

    // If there is a value to return and it isn't in the return value register
    if (opnds.length > 0 && opnds[0] !== retValReg)
    {
        tltor.asm.mov(opnds[0], retValReg);
    }

    // If this is a proxy callable from C
    if (tltor.fct.cProxy)
    {
        // Put the stack pointer where C expects it to be
        if (stack !== cstack)
        {
            tltor.asm.
            mov(stack, cstack);
        }

        if (tltor.asm.target === x86.target.x86)
        {
            // We follow 32 bits Windows, Linux, BSD and Mac OS X
            // callee-save convention
            tltor.asm.
            pop(reg.ebp).
            pop(reg.edi).
            pop(reg.esi).
            pop(reg.ebx);
        } 
        else 
        {
            // We follow 64 bits Linux, BSD, Mac OS X
            // callee-save convention
            tltor.asm.
            pop(reg.r15).
            pop(reg.r14).
            pop(reg.r13).
            pop(reg.r12).
            pop(reg.rbp).
            pop(reg.rbx);

            // Pop the arguments that were passed in registers from stack
            for (var i=0; i < Math.min(tltor.fct.argVars.length, 6); ++i)
            {
                tltor.asm.
                pop(backendCfg.x64ArgsReg[i]);
            }
        }

        tltor.asm.ret();
    } else
    {
        // Stack frame has been modified, pop the 
        // arguments passed in registers that were 
        // moved to the stack
        const argsRegNb = backendCfg.argsReg.length;
        const expNumArgs = tltor.fct.argVars.length;
        const retOffset = expNumArgs + 2 - argsRegNb;

        tltor.asm.ret($((retOffset > 0 ? retOffset : 0)*refByteNb));
    }
};

IfInstr.prototype.genCode = function (tltor, opnds)
{
    // Assembler imports
    const $ = x86.Assembler.prototype.immediateValue;

    const trueLabel = tltor.label(this.targets[0], this.targets[0].label);
    const falseLabel = tltor.label(this.targets[1], this.targets[1].label);

    // If the operand is in a register
    if (opnds[0].type === x86.type.REG)
    {
        // Use the test instruction
        tltor.asm.
        test(opnds[0], opnds[0]).
        je(falseLabel).
        jmp(trueLabel);
    }
    else
    {
        // Use the compare instruction
        tltor.asm.
        cmp($(0), opnds[0], this.uses[0].type.getSizeBits(tltor.params)).
        je(falseLabel).
        jmp(trueLabel);
    }
};

// For now, acts as a return 
ThrowInstr.prototype.genCode = RetInstr.prototype.genCode;

//CatchInstr

/**
*   Allocation information for Call Instructions 
*/
CallInstr.prototype.regAlloc = Object.create(IRInstr.prototype.regAlloc);

CallInstr.prototype.regAlloc.opndsRegHint = function (instr, params, position)
{
    const backendCfg = params.target.backendCfg;

    if (position > 0 && position - 1 < backendCfg.argsIndex.length)
    {
        return backendCfg.argsIndex[position - 1];
    } 
    else
    {
        return null;
    }
};

CallInstr.prototype.regAlloc.outRegHint = function (instr, params)
{
    return params.target.backendCfg.retValIndex;
};

CallInstr.prototype.regAlloc.usedRegisters = function (instr, params)
{
    return arrayRange(params.target.backendCfg.physReg.length);
};

CallInstr.prototype.genCode = function (tltor, opnds)
{
    // Assembler imports
    const $ = x86.Assembler.prototype.immediateValue;
    const mem = x86.Assembler.prototype.memory;

    // Configuration imports
    const target = tltor.params.target;
    const backendCfg = target.backendCfg;
    const refByteNb = target.ptrSizeBytes;
    const context = backendCfg.context;
    const width = target.ptrSizeBits;

    // Register used for the return value
    const dest = this.regAlloc.dest;

    // Scratch register
    const scratch = backendCfg.scratchReg;

    // Stack register
    const stack = backendCfg.stack;

    // Array of registers reserved for passing arguments
    const argsReg = backendCfg.argsReg;

    // Number of registers used for passing arguments
    const argRegNb = backendCfg.argsIndex.length;

    // Register to be used for the function pointer
    const funcPtrReg = backendCfg.funcPtrReg;
    
    // Num args location
    const numArgsOffset = backendCfg.ctxLayout.getFieldOffset(["numargs"]);
    const numArgs = mem(numArgsOffset, context);

    // Used for loop iterations
    var i;

    // Register object
    var reg;

    // Used for moving operands in the right registers
    var map;

    assert(
        dest === backendCfg.retValReg || dest === null,
        'invalid destination register for function call'
    );

    const funcPtrWrongReg = (
        (opnds[0].type === x86.type.REG && opnds[0] !== funcPtrReg) ||
        opnds[0].type === x86.type.LINK
    );

    const toSpill = [scratch]; 
    if (funcPtrWrongReg)
    {
        toSpill.push(funcPtrReg);
    }

    tltor.spillOnCtx(toSpill, opnds);

    if (funcPtrWrongReg)
    {
        tltor.asm.mov(opnds[0], funcPtrReg);
        var funcPtr = funcPtrReg;
    } else
    {
        var funcPtr = opnds[0];
    }

    // Get the function arguments
    var funcArgs = opnds.slice(1);

    // Index for the last argument passed in a register 
    const lastArgIndex = argRegNb - 1;

    // Number of operands that must be passed on stack
    var argStackNb = funcArgs.length - argRegNb;
    argStackNb = (argStackNb < 0) ? 0 : argStackNb;
    
    // Stack pointer offset for all operands passed on stack
    const stackOffset = argStackNb * refByteNb;

    // Stack pointer offset
    var spoffset;

    // Temporary opnd
    var opnd;

    // Allocate space on stack for extra args
    if (stackOffset > 0)
    {
        tltor.asm.sub($(stackOffset), stack);

        for (i = argRegNb, spoffset = 0; 
             i < funcArgs.length; 
             ++i, spoffset += refByteNb)
        {
            var arg = funcArgs[i];

            if (arg.type === x86.type.MEM ||
                arg.type === x86.type.LINK && arg.width() === 64)
            {
                if (arg.type === x86.type.MEM && arg.base === stack)
                {
                    // Adjust the offset to take the displacement of the 
                    // stack pointer into account
                    arg = mem(arg.disp + stackOffset, stack);
                } 

                // Source memory location
                tltor.asm.
                mov(arg, scratch).
                mov(scratch, mem(spoffset, stack));
            } 
            else
            {
                tltor.asm.
                mov(arg, mem(spoffset, stack), stack.width());
            }
        }
    }

    // Move arguments in the right registers
    map = allocator.mapping();

    for (i = 0; i < argRegNb && i < funcArgs.length; ++i)
    {
        var arg = funcArgs[i];
        
        reg = argsReg[i];
        
        if (arg !== reg)
        {
            // Fix the offset since the stack pointer has been moved
            if (arg.type === x86.type.MEM && 
                arg.base === stack)
            {
                arg = mem(arg.disp + stackOffset, stack);
            }

            map.add(arg, reg);
        }
    }

    map.orderAndInsertMoves( 
        function (move)
        {
            move.genCode(tltor, move.uses);
        },
        scratch
    );

    // The first operand should be the function address
    assert(
        opnds[0].type === x86.type.REG || 
        opnds[0].type === x86.type.MEM ||
        opnds[0].type === x86.type.LINK,
        "Invalid CallInstr function operand '" + opnds[0] + "'"
    );

    const ctx = backendCfg.context;

    if (!tltor.fct.cProxy)
    {
        // Store the number of arguments in the context register
        tltor.asm.mov($(funcArgs.length), numArgs, width);
    }

    // Call the function by its address
    tltor.asm.call(funcPtr);

    // If this function has a continuation label
    if (this.targets[0] !== undefined)
    {
        // Label for the continuation
        const continue_label = tltor.label(this.targets[0], this.targets[0].label);

        // Jump to continue_label
        tltor.asm.jmp(continue_label);
    }
};

CallApplyInstr.prototype.regAlloc = Object.create(CallInstr.prototype.regAlloc);

CallApplyInstr.prototype.genCode = function (tltor, opnds)
{
    // Assembler imports
    const mem = x86.Assembler.prototype.memory;
    const $ = x86.Assembler.prototype.immediateValue;

    // Configuration imports
    const target = tltor.params.target;
    const backendCfg = tltor.params.target.backendCfg;
    const stack = backendCfg.stack;
    const argsReg = backendCfg.argsReg;
    const argsRegNb = argsReg.length;
    const funcPtrReg = backendCfg.funcPtrReg;
    const context = backendCfg.context;

    const refByteNb = target.ptrSizeBytes;

    const numArgsCtxOffset = backendCfg.ctxLayout.getFieldOffset(["numargs"]);
    const numArgsCtx = mem(numArgsCtxOffset, context);
    
    // Move all operands on context
    const freeRegs = tltor.spillOnCtx(opnds, opnds);

    // Implicit number of arguments to call
    const impArgNb = 2;

    // Call apply operands are now on context
    const funcPtr  = opnds[0];
    const funcObj  = opnds[1];
    const thisArg  = opnds[2];
    const argTable = opnds[3];
    const numArgs  = opnds[4];

    const scratchRegs = freeRegs.slice(0);
    arraySetRemAll(scratchRegs, argsReg);
    assert(
        scratchRegs.length >= 1, 
        "Insufficient number of scratch registers available"
    );
    const index   = scratchRegs[0];
    const temp    = backendCfg.scratchReg;

    const otherRegs = freeRegs.slice(0);
    arraySetRemAll(otherRegs, scratchRegs);
    assert(
        otherRegs.length >= 1,
        "Insufficient number of non-scratch registers available"
    );
    const srcPtr  = otherRegs[0];

    const cpLoop  = tltor.asm.labelObj();
    const cpDone  = tltor.asm.labelObj();

    const shiftAmt = (refByteNb === 4) ? 2 : 3;

    // Get the table offset in the arguments table
    const tblOffset = tltor.params.memLayouts.arrtbl.getFieldOffset(["tbl", 0]);

    tltor.asm.
    
    // Initialize loop
    mov(numArgs, index).
    mov(argTable, srcPtr).

    // Add space for argTable values and 
    // implicit arguments
    mov(index, temp).
    sal($(shiftAmt), temp).
    sub(temp, stack).
    sub($(impArgNb*refByteNb), stack).

    // Copy implicit arguments on stack
    mov(funcObj, temp).
    mov(temp, mem(0, stack)).
    mov(thisArg, temp).
    mov(temp, mem(refByteNb, stack)).

    // Loop over entries and copy on stack
    label(cpLoop).
    cmp($(0), index).
    jle(cpDone).

    mov(mem(tblOffset - refByteNb, srcPtr, index, refByteNb), temp).
    mov(temp, mem((impArgNb-1)*refByteNb, stack, index, refByteNb)).

    dec(index).
    jmp(cpLoop).
    label(cpDone).

    mov(numArgs, index).
    add($(impArgNb), index).

    // Store the number of arguments in context
    mov(index, numArgsCtx);

    // Move values that should be in registers
    argsReg.forEach(function (reg, i) {
        tltor.asm.
        cmp($(i), index).
        cmovnle(mem(i*refByteNb, stack), reg);
    });

    // Adjust stack pointer
    tltor.asm.
    mov($(argsRegNb), temp).
    cmp(temp, index).
    cmovnle(temp, index).
    lea(mem(0, stack, index, refByteNb), stack).

    // Call function 
    call(funcPtr);

    // If this function call has a continuation label
    if (this.targets[0] !== undefined)
    {
        // Label for the continuation
        const continue_label = tltor.label(this.targets[0], this.targets[0].label);

        // Jump to continue_label
        tltor.asm.jmp(continue_label);
    }
};

CallFFIInstr.prototype.regAlloc = Object.create(CallInstr.prototype.regAlloc);

CallFFIInstr.prototype.genCode = function (tltor, opnds)
{
    // Assembler imports
    const reg = x86.Assembler.prototype.register; 
    const mem = x86.Assembler.prototype.memory;
    const $ = x86.Assembler.prototype.immediateValue;

    // Configuration imports
    const target = tltor.params.target; 
    const backendCfg = target.backendCfg;
    const refByteNb = target.ptrSizeBytes; 
    const width = target.ptrSizeBits;

    const argsReg = backendCfg.argsReg;

    const stack = backendCfg.stack;

    const context = backendCfg.context;


    const xSP = reg.rsp.subReg(width);

    const xAX = reg.rax.subReg(width);

    const stackAlignByteNb = backendCfg.stackAlignByteNb;

    const cfct = this.uses[0];

    const fctAddr = cfct.funcPtr;

    const callDest  = tltor.asm.linked(
                    cfct.funcName, 
                    function (dstAddr) { return fctAddr.getBytes(); },
                    fctAddr.width());
                    
    const numArgs = opnds.length - 1;        

    const is64 = tltor.asm.target === x86.target.x86_64;

    const firstIndex = is64 ? 6 : 0;



    if (is64)
    {
        // Use registers not required by the target calling convention 
        var altStack   = reg.r10;
        var scratchReg = backendCfg.scratchReg;
    } else
    {
        // No registers are required by the target calling convention,
        // but prefer registers that would less frequently contain 
        // operands
        var altStack   = backendCfg.nonArgsReg[0];
        var scratchReg = backendCfg.scratchReg;
    }

    // Spill an operand on the context if it is in one of the reserved registers
    tltor.spillOnCtx([altStack, scratchReg], opnds);

    // Invariant: opnds are constants, registers or memory location
    //            containing C-valid object or primitive values

    // Stack space taken by arguments
    var argStackSpace = 0;

    // Offsets to write each argument at
    var argOffsets = [];

    // For each argument
    for (var i = firstIndex; i < numArgs; ++i)
    {
        argOffsets.push(argStackSpace);

        var opndSizeBytes = this.uses[i+1].type.getSizeBytes(tltor.params); 

        argStackSpace += opndSizeBytes;

        // Align the offset for the next argument
        var rem = argStackSpace % refByteNb;
        if (rem !== 0)
            argStackSpace += refByteNb - rem;
    }

    /*
    Stack frames pushed on top, addresses decreasing upward. 

    C stack frame
    -----------------
    C arguments       <-- 16 byte aligned for OS X ABI Function call guide
    -----------------
    Old stack pointer
    -----------------
    ...
    Padding 
    (variable size)
    ...
    -----------------
    Context pointer
    -----------------   
    Tachyon frame     <-- Old stack pointer (altStack)
    */

    // Compute the total stack space needed for the context register, 
    // the old stack pointer and arguments
    var totalStackSpace = (2 * refByteNb) + argStackSpace;

    // Mask works both for 32 and 64 bits because the value will 
    // be signed extended to 64 bits
    const alignMask = ~(stackAlignByteNb - 1);

    // Move the current stack pointer into alternate register
    tltor.asm.
    mov(stack, altStack).

    // Reserve the total stack space needed
    sub($(totalStackSpace), stack).

    // Save the context pointer
    mov(context, mem(-refByteNb, altStack)).

    // Align the new stack pointer
    and($(alignMask), stack).
    
    // Save the old stack pointer below the arguments
    mov(altStack, mem(argStackSpace, stack));

    // Write arguments on stack in reverse order
    for (i = firstIndex; i < numArgs; ++i)
    {
        var opnd = opnds[i+1];

        var opndSizeBits = this.uses[i+1].type.getSizeBits(tltor.params);

        var offset = argOffsets[i];

        if (opnd.type === x86.type.REG)
        {
            tltor.asm.
            mov(opnd, mem(offset, stack));
        } 
        else if (opnd.type === x86.type.MEM)
        {
            tltor.asm.
            mov(mem(opnd.disp, altStack), scratchReg).
            mov(scratchReg, mem(offset, stack));
        } 
        else if (opnd.type === x86.type.IMM_VAL)
        {
            tltor.asm.
            mov(opnd, mem(offset, stack), opndSizeBits);
        }
        else
        {
            error("invalid opnd type for ffi function call: " + opnd.type);
        }
    }

    // If we are on 64 bits, the first 6 arguments should be in registers
    if (is64)
    {
        // Move arguments in the right registers
        map = allocator.mapping();
        for (var i = 0; i < 6 && i < numArgs; ++i)
        {
            var arg = opnds[i+1];
            
            var x64Arg = backendCfg.x64ArgsReg[i];
            
            if (arg !== x64Arg)
            {
                // Fix the offset since the stack pointer has been moved
                if (arg.type === x86.type.MEM && 
                    arg.base === stack)
                {
                    arg = mem(arg.disp, altStack);
                }
                
                map.add(arg, x64Arg);
            }
        }

        map.orderAndInsertMoves( 
            function (move)
            {
                tltor.asm.mov(move.uses[0], move.uses[1]);
            },
            scratchReg
        );
    }

    // Prepare stack pointer for C calling convention
    if (stack !== xSP)
    {
        tltor.asm.
        mov(stack, xSP);
    }

    // Call the C function
    tltor.asm.
    mov(callDest, xAX).
    call(xAX);

    // Move return value into Tachyon calling convention register
    if (backendCfg.retValReg !== xAX)
    {
        tltor.asm.
        mov(xAX, backendCfg.retValReg);
    }

    // Restore runtime specific registers
    tltor.asm.
    mov(mem(argStackSpace, xSP), altStack).
    mov(mem(-refByteNb, altStack), context).
    mov(altStack, stack);
};

ConstructInstr.prototype.genCode = CallFuncInstr.prototype.genCode;

ICastInstr.prototype.genCode = function (tltor, opnds)
{
    const reg = x86.Assembler.prototype.register; 

    const dest = this.regAlloc.dest;

    const configWidth = tltor.params.target.ptrSizeBits;
    const dstWidth = this.type.getSizeBits(tltor.params);
    const srcWidth = this.uses[0].type.getSizeBits(tltor.params);

    assert(
        dest.type === x86.type.REG, 
        "Destination should be a register"
    );   

    if (dstWidth === 8 && configWidth === 32)
    {
        assert(
            dest === reg.rax.subReg(configWidth) ||
            dest === reg.rbx.subReg(configWidth) ||
            dest === reg.rcx.subReg(configWidth) ||
            dest === reg.rdx.subReg(configWidth),
            "Invalid destination register '" + dest + "' on 32 bits "
        );
    }

    if (srcWidth === 8 && configWidth === 32)
    {
        assert(
            opnds[0] === reg.rax.subReg(configWidth) ||
            opnds[0] === reg.rbx.subReg(configWidth) ||
            opnds[0] === reg.rcx.subReg(configWidth) ||
            opnds[0] === reg.rdx.subReg(configWidth),
            "Invalid operand register '" + opnds[0] + "' on 32 bits "
        );
    }

    if (opnds[0] === dest && srcWidth === dstWidth)
    {
        // Do nothing
    }
    else if (srcWidth === dstWidth)
    {
        tltor.asm.
        mov(opnds[0], dest);
    }
    else if (srcWidth > dstWidth)
    {
        if (opnds[0].type === x86.type.REG)
        {
            tltor.asm.
            mov(opnds[0].subReg(dstWidth), dest.subReg(dstWidth));
        } else 
        {
            tltor.asm.
            mov(opnds[0], dest.subReg(dstWidth), dstWidth);
        }

        if (dstWidth !== 32)
        {
            tltor.asm.
            movxx(dest.subReg(dstWidth), dest, false);
        }
    } 
    else
    {
        const isSigned = this.uses[0].type.isSigned() && this.type.isSigned();

        if (opnds[0].type === x86.type.REG)
        {
            tltor.asm.
            movxx(opnds[0].subReg(srcWidth), dest.subReg(dstWidth), isSigned);
        } else
        {
            tltor.asm.
            movxx(opnds[0], dest.subReg(dstWidth), isSigned, srcWidth);
        }
    }
};

//IToFPInstr

//FPToIInstr

/**
Allocation information for store instruction
*/
LoadInstr.prototype.regAlloc = Object.create(IRInstr.prototype.regAlloc);

// All operands must be in registers
LoadInstr.prototype.regAlloc.opndsRegRequired = true;

LoadInstr.prototype.genCode = function (tltor, opnds)
{
    /*
    SIB = Scale, Index, Base

    mem(disp, base, index, scale)

    base    : register
    disp    : immediate offset
    index   : register              (optional)
    scale   : applied to index reg  (optional) 
              1, 2, 4, 8
    */

    assert (
        opnds[0].type === x86.type.REG,
        'cannot use immediate values as pointers'
    );

    assert (
        opnds[1].type === x86.type.REG || opnds[1].type === x86.type.IMM_VAL,
        'cannot use memory locations as offsets'
    );

    // Assembler imports
    const mem = x86.Assembler.prototype.memory; 

    const dst = this.regAlloc.dest;

    assert (
        this.uses[0].type === IRType.rptr ||
        this.uses[0].type === IRType.ref,
        'load can only operate on raw pointers or references'
    );

    // If the offset is a register
    if (opnds[1].type === x86.type.REG)
    {
        // Use the index field
        var memLoc = mem(0, opnds[0], opnds[1], 1);
    }
    else
    {
        // Use the displacement field
        var memLoc = mem(opnds[1].value, opnds[0], undefined, 1);
    }

    // If the value we are loading needs to be extended
    if (this.type.getSizeBits(tltor.params) < 
        IRType.pint.getSizeBits(tltor.params))
    {
        // If we are loading a signed value
        if (this.type.isSigned())
        {
            // Sign-extend the value
            tltor.asm.movsx(memLoc, dst, this.type.getSizeBits(tltor.params));
        }
        else
        {
            // Zero-extend the value
            tltor.asm.movzx(memLoc, dst, this.type.getSizeBits(tltor.params));
        }
    }
    else
    {
        // Load the value directly
        tltor.asm.mov(memLoc, dst, this.type.getSizeBits(tltor.params));
    }
};

/**
Allocation information for store instruction
*/
StoreInstr.prototype.regAlloc = Object.create(IRInstr.prototype.regAlloc);

// All operands must be in registers
StoreInstr.prototype.regAlloc.opndsRegRequired = true;

StoreInstr.prototype.regAlloc.usedRegisters = function (instr, params) 
{
    const srcOpnd = instr.uses[2];

    if ((!srcOpnd instanceof IRInstr) || 
        srcOpnd.type.getSizeBits(params) !== 8)
        return null;
 
    // On x86 32 bits, reserve one of EAX, EBX or EDX 
    // in case the src operand is not allocated to one of those 
    //return [0];
    
    // FIXME: until stronger register hints are implemented
    return arrayRange(params.target.backendCfg.physReg.length);
};

StoreInstr.prototype.genCode = function (tltor, opnds)
{
    /*
    SIB = Scale, Index, Base

    mem(disp, base, index, scale)

    base    : register
    disp    : immediate offset
    index   : register              (optional)
    scale   : applied to index reg  (optional) 
              1, 2, 4, 8
    */

    assert (
        opnds[0].type === x86.type.REG,
        'cannot use immediate values as pointers'
    );

    assert (
        opnds[1].type === x86.type.REG || opnds[1].type === x86.type.IMM_VAL,
        'cannot use memory locations as offsets'
    );

    assert (
        opnds[2].type === x86.type.REG || tltor.asm.isImmediate(opnds[2]),
        'cannot perform store from memory to memory'
    );

    assert (
        this.uses[0].type === IRType.rptr ||
        this.uses[0].type === IRType.ref,
        'load can only operate on raw pointers or references'
    );

    // Assembler imports
    const mem = x86.Assembler.prototype.memory; 
    const reg = x86.Assembler.prototype.register;  

    // Configuration imports
    const target = tltor.params.target; 
    const width = target.ptrSizeBits;
    const backendCfg = target.backendCfg;
    const context = backendCfg.context;

    // If the offset is a register
    if (opnds[1].type === x86.type.REG)
    {
        // Use the index field
        var memLoc = mem(0, opnds[0], opnds[1], 1);
    }
    else
    {
        // Use the displacement field
        var memLoc = mem(opnds[1].value, opnds[0], undefined, 1);
    }

    // Get the size of the value to be stored
    var typeSize = this.uses[2].type.getSizeBits(tltor.params);

    // If the value to store is an immediate
    if (tltor.asm.isImmediate(opnds[2]))
    {
        // Store it directly
        MoveInstr.prototype.genMov(tltor, opnds[2], memLoc, typeSize);
    }

    // Otherwise, the value to store is in a register
    else
    {
        // Get the register corresponding to the type size
        var srcReg = opnds[2].subReg(typeSize);

        // On x86 32 bits, only AL, BL, CL, DL can be accessed directly
        if (typeSize === 8 && 
            tltor.asm.target === x86.target.x86 && 
            (srcReg !== reg.al || srcReg !== reg.bl || 
             srcReg !== reg.cl || srcReg !== reg.dl))
        {
            // FIXME: ugly code below to temporarily deal with x86 gayness 
            // without help from the register allocator
            //          ______   ________                      
            // ___  ___/  __  \ /  _____/   ________ _____  ___
            // \  \/  />      </   __  \   /  ___/  |  \  \/  /
            //  >    </   --   \  |__\  \  \___ \|  |  />    < 
            // /__/\_ \______  /\_____  / /____  >____//__/\_ \
            //       \/      \/       \/       \/            \/

            /*
            const xAX = reg.rax.subReg(width);     

            // xAX has been reserved for this case
            tltor.asm.mov(opnds[2], xAX);    
            srcReg = reg.al;
            */

            var lowRegs = [reg.eax, reg.ebx, reg.ecx, reg.edx];

            arraySetRem(lowRegs, opnds[0]);
            arraySetRem(lowRegs, opnds[1]);

            var newSrc = lowRegs[0];
            var newSrcLow = newSrc.subReg(8);

            tltor.asm.mov(opnds[2], newSrc);    
            srcReg = newSrcLow;
        }

        // Store the value to memory
        tltor.asm.mov(srcReg, memLoc, typeSize);
    }
};

GetCtxInstr.prototype.genCode = function (tltor, opnds)
{
    const ctx = tltor.params.target.backendCfg.context; 
    const dest = this.regAlloc.dest;

    if (ctx !== dest)
    {
        tltor.asm.mov(ctx, dest);
    }
};

SetCtxInstr.prototype.genCode = function (tltor, opnds)
{
    tltor.asm.
    mov(opnds[0], tltor.params.target.backendCfg.context);
};

MoveInstr.prototype.genCode = function (tltor, opnds)
{
    // Configuration imports
    const target = tltor.params.target; 
    const width = target.ptrSizeBits;

    this.genMov(tltor, opnds[0], opnds[1], width);
};

MoveInstr.prototype.genMov = function (tltor, src, dest, width)
{
    // Configuration imports
    const target = tltor.params.target; 
    const scratchReg = target.backendCfg.scratchReg;

    if ((src.type === x86.type.MEM || 
         (!tltor.asm.is32bitImm(src))) &&
        dest.type === x86.type.MEM)
    {
        tltor.asm.
        mov(src, scratchReg).
        mov(scratchReg, dest);
    } else
    {
        tltor.asm.
        mov(src, dest, width);
    }
};

GetNumArgsInstr.prototype.genCode = function (tltor, opnds)
{
    // Assembler imports
    const mem = x86.Assembler.prototype.memory; 
    const $ = x86.Assembler.prototype.immediateValue;

    // Configuration imports
    const backendCfg = tltor.params.target.backendCfg;
    const context = backendCfg.context;

    const numArgsOffset = backendCfg.ctxLayout.getFieldOffset(["numargs"]);
    const numArgs = mem(numArgsOffset, context);
    const dest = this.regAlloc.dest;

    tltor.asm.
    mov(numArgs, dest).
    sub($(2), dest);
};

GetArgTableInstr.prototype.genCode = function (tltor, opnds)
{
    // Assembler imports
    const mem = x86.Assembler.prototype.memory; 

    // Configuration imports
    const backendCfg = tltor.params.target.backendCfg;
    const context = backendCfg.context;

    const argTblOffset = backendCfg.ctxLayout.getFieldOffset(["argtbl"]);
    const argTbl = mem(argTblOffset, context);
    const dest = this.regAlloc.dest;

    tltor.asm.
    mov(argTbl, dest);

};
