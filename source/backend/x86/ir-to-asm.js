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
            irfunc.funcName,
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

        const dest = this.regAlloc.dest;

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
            tltor.asm[name](shiftAmt, dest);
        }
        else
        {
            tltor.asm.
            mov(opnds[0], dest);

            tltor.asm[name](shiftAmt, dest);
        }

        if (opnds[1].type !== x86.type.IMM_VAL)
        {
            tltor.asm.mov($(0), reg.cl);
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
                (this instanceof CallInstr && index === 0) ? "offset" : "addr"
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
                return [0,0,0,0];
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
    const scratch = backendCfg.physReg[backendCfg.physReg.length - 1];
    const scratch2 = backendCfg.physReg[backendCfg.physReg.length - 2];

    const argsReg   = backendCfg.argsReg;
    const argsRegNb = backendCfg.argsReg.length;
    const spillNb = this.fct.regAlloc.spillNb;

    // Add an entry point for default static calls
    var lobj = irToAsm.getEntryPoint(this.fct, undefined, this.params);
    this.asm.provide(lobj);

    if (this.fct.cProxy)
    {
        if (target.ptrSizeBits === 32)
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
        const numArgsOffset = backendCfg.ctxLayout.getFieldOffset(["numargs"]);
        const numArgs = mem(numArgsOffset, context);

        const tempOffset = backendCfg.ctxLayout.getFieldOffset(["temp"]);
        const ctxTemp = mem(tempOffset, context);

        const xAX = reg.rax.subReg(width);
        const xBX = reg.rbx.subReg(width);
        const xDX = reg.rdx.subReg(width);
        const xSI = reg.rsi.subReg(width);

        const printInt = irToAsm.getEntryPoint(
                                this.params.staticEnv.getBinding("printInt"),
                                "fast",
                                this.params, 
                                "offset"
                            );

        if (this.fct.usesArguments)
        {
            // TODO: Should be moved into a separate handler

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

            // Retrieve pointer to argument table
            assert(backendCfg.argsReg.length >= 3, "Unsupported calling convention");

            const fstOpnd = backendCfg.argsReg[2];

            const argTblOffset = backendCfg.ctxLayout.getFieldOffset(["argtbl"]);
            const argTbl  = mem(argTblOffset, context);
            const tblOffset = this.params.memLayouts.arrtbl.getFieldOffset(
                                 ["tbl", 0]
                              );
    
            const allocArgTbl   = irToAsm.getEntryPoint(
                                    this.params.staticEnv.getBinding("allocArgTable"),
                                    "fast",
                                    this.params, 
                                    "offset"
                                );



            const loop = this.asm.labelObj(); 
            const end  = this.asm.labelObj(); 

            this.asm.
            // Move numArgs into corresponding register
            sub($(2), numArgs, width).
            mov(numArgs, fstOpnd).

            // Preserve the number of argument accross calls
            push(fstOpnd).

            // Call handler, result is in xAX
            call(allocArgTbl).

            // Restore the number of arguments
            // And copy arguments into argument table
            pop(xBX).          // n = numArgs
            mov(xBX, numArgs).

            mov($(0), xSI).    // i = 0
            label(loop).       // for(i=0, i<n, ++i) {
            cmp(xBX, xSI).    
            jge(end).

            mov(mem(3*refByteNb, stack, xSI, refByteNb), xDX). // temp = sp[i] + 3
            mov(xDX, mem(tblOffset, xAX, xSI, refByteNb)).   // argTbl[i] = temp

            inc(xSI).                                      
            jmp(loop).         // }
            label(end).

            // Store argument table on the context
            mov(xAX, argTbl).
            add($(2), numArgs, width);

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
        }
        
        // Handle a variable number of arguments
        assert(argsRegNb + 2 <= backendCfg.physReg.length,
               "Current handling of variable number of arguments need " +
               "2 registers to operate");
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
        const temp      = xAX;
        const undefImm = $(this.params.staticEnv
                          .getBinding("BIT_PATTERN_UNDEF").value);

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
            const index = xBX;

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
        sal($(2), argOffset).
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
    
    // Offset due to C calling convention 
    const ccallOffset = (tltor.fct.cProxy === true) ?
                        ((tltor.asm.target === x86.target.x86) ? 4 : 6) : 0;

    // Offset to the argument on the stack
    const spoffset = (regAllocSpillNb + callSiteArgIndex + 
                      ccallOffset + 1) * refByteNb;

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
    const dest = this.regAlloc.dest;

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
    const dest = this.regAlloc.dest;
 
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
    const reg = x86.Assembler.prototype.register;
    const width = tltor.params.target.ptrSizeBits;
    const xAX = reg.rax.subReg(width);
    const xDX = reg.rdx.subReg(width);

    // Register used for the output value
    const dst = this.regAlloc.dest;

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

        tltor.asm.mul(op1, this.type.getSizeBits(tltor.params));
    }

    // Otherwise, a signed result is expected
    else
    {
        if (opnds[0].type === x86.type.IMM_VAL)
        {
            tltor.asm.imul(
                opnds[1], 
                dst, 
                opnds[0], 
                this.type.getSizeBits(tltor.params)
            );
        }

        else if (opnds[1].type === x86.type.IMM_VAL)
        {
            tltor.asm.imul(
                opnds[0], 
                dst, 
                opnds[1], 
                this.type.getSizeBits(tltor.params)
            );
        }

        else if (opnds[0] === dst)
        {
            tltor.asm.imul(
                opnds[1], 
                dst, 
                undefined, 
                this.type.getSizeBits(tltor.params)
            );
        }

        else if (opnds[1] === dst)
        {
            tltor.asm.imul(
                opnds[0], 
                dst, 
                undefined, 
                this.type.getSizeBits(tltor.params)
            );
        }

        else
        {
            tltor.asm.mov(opnds[0], dst);
            tltor.asm.imul(
                opnds[1], 
                dst, 
                undefined, 
                this.type.getSizeBits(tltor.params)
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
    // xDX:xAX are reserved for the dividend,
    // xBX is reverved as a scratch register
    return [0,1,2];
};

DivInstr.prototype.genCode = function (tltor, opnds)
{
    // Configuration imports
    const width = tltor.params.target.ptrSizeBits;

    // Assembler imports
    const reg = x86.Assembler.prototype.register;
    const $ = x86.Assembler.prototype.immediateValue;

    // In the end, we want to have:
    // - Dividend in xAX
    // - Divisor NOT in xAX or xDX

    const dvnd    = {reg:null, value:opnds[0]};
    const dsor    = {reg:null, value:opnds[1]};
    const scratch = {reg:null, value:null};

    const xAX     = {physReg:reg.rax.subReg(width), value:null};
    const xBX     = {physReg:reg.rbx.subReg(width), value:null};
    const xDX     = {physReg:reg.rdx.subReg(width), value:null};

    function setReg(reg)
    {
        if (reg.physReg === dvnd.value)
        {
            reg.value = dvnd;
            dvnd.reg  = reg;
        } else if (reg.physReg === dsor.value)
        {
            reg.value = dsor;
            dsor.reg  = reg;
        }
    };

    function setScratch(scratch)
    {
        if (xAX.value === null)
        {
            scratch.reg = xAX;
        } else if (xBX.value === null)
        {
            scratch.reg = xBX;
        } else
        {
            scratch.reg = xDX;
        }
    };

    function xchg(opnd1, opnd2)
    {
        if (opnd1 === scratch)
        {
            tltor.asm.mov(opnd2.reg.physReg, scratch.reg.physReg);
        } else if (opnd2 === scratch)
        {
            tltor.asm.mov(opnd1.reg.physReg, scratch.reg.physReg);
        } else
        {
            tltor.asm.xchg(opnd1.reg.physReg, opnd2.reg.physReg);
        }

        var treg = opnd1.reg;
        var tvalue = opnd1.value;

        opnd1.reg       = opnd2.reg;
        opnd1.reg.value = opnd1;
        opnd1.value     = opnd1.reg.physReg;

        opnd2.reg       = treg;
        opnd2.reg.value = opnd2;
        opnd2.value     = treg.physReg;
    };

    function moveOpndToReg(opnd, reg)
    {
        // Move the operand value in the given register
        tltor.asm.mov(opnd.value, reg.physReg);
        
        reg.value  = opnd;
        opnd.reg   = reg;
        opnd.value = reg.physReg;
    };

    setReg(xAX);
    setReg(xBX);
    setReg(xDX);

    setScratch(scratch);

    // If the dividend is not xAX register
    if (xAX.value !== dvnd)
    {
        // If xAX already contains an operand,
        // move it to the scratch register
        if (xAX.value !== null && xAX.value !== scratch)
        {
            xchg(xAX.value, scratch);
        }

        // Move the dividend value in xAX
        moveOpndToReg(dvnd, xAX);
    }

    // If the divisor is in xDX or is a 
    // memory location, move it to 
    // xBX, otherwise use it directly
    if (xDX.value === dsor || tltor.asm.isImmediate(dsor.value))
    {
        moveOpndToReg(dsor, xBX);
    }

    // If the output should be unsigned, use unsigned divide, otherwise
    // use signed divide 
    if (this.type.isUnsigned())
    {
        // Extend the value into EDX
        tltor.asm.mov($(0), xDX.physReg);

        tltor.asm.div(dsor.value, this.type.getSizeBits(tltor.params));
    }
    else
    {
        // Sign-extend EAX into EDX:EAX using CDQ
        tltor.asm.cdq();

        tltor.asm.idiv(dsor.value, this.type.getSizeBits(tltor.params));
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

AndInstr.prototype.genCode = function (tltor, opnds)
{
    const dest = this.regAlloc.dest;

    if (opnds[0].type === x86.type.REG && opnds[0] === opnds[1])
    {
        if (opnds[0] !== dest)
        {
            tltor.asm.mov(opnds[0], dest);
        }
    } 
    else if (opnds[0].type === x86.type.REG && opnds[0] === dest)
    {
        tltor.asm.and(opnds[1], dest);
    } 
    else if (opnds[1].type === x86.type.REG && opnds[1] === dest)
    {
        tltor.asm.and(opnds[0], dest);
    } 
    else
    {
        tltor.asm.
        mov(opnds[0], dest).
        and(opnds[1], dest);
    }
};

OrInstr.prototype.genCode = function (tltor, opnds)
{
    const dest = this.regAlloc.dest;

    if (opnds[0].type === x86.type.REG && opnds[0] === opnds[1])
    {
        if (opnds[0] !== dest)
        {
            tltor.asm.mov(opnds[0], dest);
        }
    }
    else if (opnds[0].type === x86.type.REG && opnds[0] === dest)
    {
        tltor.asm.or(opnds[1], dest);
    }
    else if (opnds[1].type === x86.type.REG && opnds[1] === dest)
    {
        tltor.asm.or(opnds[0], dest);
    } 
    else
    {
        tltor.asm.
        mov(opnds[0], dest).
        or(opnds[1], dest);
    }
};

XorInstr.prototype.genCode = function (tltor, opnds)
{
    const dest = this.regAlloc.dest;

    if (opnds[0].type === x86.type.REG && opnds[0] === dest)
    {
        tltor.asm.xor(opnds[1], dest);
    }
    else if (opnds[1].type === x86.type.REG && opnds[1] === dest)
    {
        tltor.asm.xor(opnds[0], dest);
    } 
    else
    {
        tltor.asm.
        mov(opnds[0], dest).
        xor(opnds[1], dest);
    }
};

irToAsm.shiftMaker(LsftInstr, "sal"); 
irToAsm.shiftMaker(RsftInstr, "sar"); 
irToAsm.shiftMaker(UrsftInstr, "shr"); 

LtInstr.prototype.genCode = function (tltor, opnds)
{
    const dest = this.regAlloc.dest;

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
        (opnds[0].type === x86.type.IMM_VAL &&
        opnds[1].type === x86.type.IMM_VAL))
    {
        tltor.asm.
        mov(opnds[0], dest).
        cmp(opnds[1], dest);
    } 
    else if (opnds[0].type === x86.type.IMM_VAL)
    {
        tltor.asm.
        cmp(opnds[0], dest).
        mov(tltor.falseVal, dest).
        cmovnl(tltor.trueVal, dest);

        return;
    } 
    else
    {
        tltor.asm.cmp(opnds[1], opnds[0], width);
    }

    tltor.asm.
    mov(tltor.falseVal, dest).
    cmovl(tltor.trueVal, dest);
};

LeInstr.prototype.genCode = function (tltor, opnds)
{
    const dest = this.regAlloc.dest;

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
        (opnds[0].type === x86.type.IMM_VAL &&
        opnds[1].type === x86.type.IMM_VAL))
    {
        tltor.asm.
        mov(opnds[0], dest).
        cmp(opnds[1], dest);
    } 
    else if (opnds[0].type === x86.type.IMM_VAL)
    {
        tltor.asm.
        mov(opnds[0], dest).
        cmp(opnds[1], dest);
    } 
    else
    {
        tltor.asm.cmp(opnds[1], opnds[0], width);
    }

    tltor.asm.
    mov(tltor.falseVal, dest).
    cmovle(tltor.trueVal, dest);
};

GtInstr.prototype.genCode = function (tltor, opnds)
{
    const dest = this.regAlloc.dest;

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
        (opnds[0].type === x86.type.IMM_VAL &&
        opnds[1].type === x86.type.IMM_VAL))
    {
        tltor.asm.
        mov(opnds[0], dest).
        cmp(opnds[1], dest);
    } 
    else if (opnds[0].type === x86.type.IMM_VAL)
    {
        tltor.asm.
        mov(opnds[0], dest).
        cmp(opnds[1], dest);
    } 
    else
    {
        tltor.asm.cmp(opnds[1], opnds[0], width);
    }

    tltor.asm.
    mov(tltor.falseVal, dest).
    cmovnle(tltor.trueVal, dest);
};

GeInstr.prototype.genCode = function (tltor, opnds)
{
    const dest = this.regAlloc.dest;

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
        (opnds[0].type === x86.type.IMM_VAL &&
        opnds[1].type === x86.type.IMM_VAL))
    {
        tltor.asm.
        mov(opnds[0], dest).
        cmp(opnds[1], dest);
    } 
    else if (opnds[0].type === x86.type.IMM_VAL)
    {
        tltor.asm.
        mov(opnds[0], dest).
        cmp(opnds[1], dest);
    } 
    else
    {
        tltor.asm.cmp(opnds[1], opnds[0], width);
    }

    tltor.asm.
    mov(tltor.falseVal, dest).
    cmovnl(tltor.trueVal, dest);
};

EqInstr.prototype.genCode = function (tltor, opnds)
{
    const dest = this.regAlloc.dest;

    // Get the operand width
    var width;
    if (opnds[0].width !== undefined)
        width = opnds[0].width();
    else if (opnds[1].width !== undefined)
        width = opnds[1].width();
    else
        width = this.uses[0].type.getSizeBits(tltor.params);

    if (opnds[0].type === x86.type.REG && opnds[0].value === 0) 
    {
        tltor.asm.test(opnds[1], opnds[1]);
    } 
    else if (opnds[1].type === x86.type.REG && opnds[1].value === 0)
    {
        tltor.asm.test(opnds[0], opnds[0]);
    } 
    else if ((opnds[0].type === x86.type.MEM || tltor.asm.isImmediate(opnds[0])) &&
             (opnds[1].type === x86.type.MEM || tltor.asm.isImmediate(opnds[1])))
    {
        tltor.asm.
        mov(opnds[0], dest).
        cmp(opnds[1], dest);
    } 
    else if (tltor.asm.isImmediate(opnds[1]))
    {
        tltor.asm.cmp(opnds[1], opnds[0], width);
    }
    else
    {
        tltor.asm.cmp(opnds[0], opnds[1], width);
    }

    tltor.asm.
    mov(tltor.falseVal, dest).
    cmovz(tltor.trueVal, dest);
};

NeInstr.prototype.genCode = function (tltor, opnds)
{
    const dest = this.regAlloc.dest;

    // Get the operand width
    var width;
    if (opnds[0].width !== undefined)
        width = opnds[0].width();
    else if (opnds[1].width !== undefined)
        width = opnds[1].width();
    else
        width = this.uses[0].type.getSizeBits(tltor.params);

    if (opnds[0].type === x86.type.REG && opnds[1].value === 0) 
    {
        tltor.asm.test(opnds[0], opnds[0]);
    } 
    else if (opnds[1].type === x86.type.REG && opnds[0].value === 0)
    {
        tltor.asm.test(opnds[1], opnds[1]);
    } 
    else if ((opnds[0].type === x86.type.MEM &&
               opnds[1].type === x86.type.MEM) ||
               (tltor.asm.isImmediate(opnds[0]) &&
                tltor.asm.isImmediate(opnds[1])))
    {
        tltor.asm.
        mov(opnds[0], dest).
        cmp(opnds[1], dest);
    } 
    else if (tltor.asm.isImmediate(opnds[1]))
    {
        tltor.asm.cmp(opnds[1], opnds[0], width);
    }
    else
    {
        tltor.asm.cmp(opnds[0], opnds[1], width);
    }

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

    // Let's arbitrarily take the last phys reg as a scratch register
    const scratchIndex = backendCfg.physReg.length - 1;
    const scratch = backendCfg.physReg[scratchIndex];

    // Number of available register for register allocation
    const avbleRegNb = backendCfg.physReg.length;

    // Stack register
    const stack = backendCfg.stack;

    // Array of registers reserved for passing arguments
    const argsReg = backendCfg.argsReg;

    // Number of registers used for passing arguments
    const argRegNb = backendCfg.argsIndex.length;

    // Register to be used for the function pointer
    const funcPtrIndex = backendCfg.funcPtrIndex;
    const funcPtrReg = backendCfg.physReg[funcPtrIndex];
    
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

    // Make sure we still have a register left for scratch
    assert (
        argRegNb < avbleRegNb,
        'no register left for scratch'
    );

    // Make sure it is not used to pass arguments
    assert (
        !(scratchIndex in backendCfg.argsIndex),
        'invalid scratch register index'
    );

    assert (
        scratch !== funcPtrReg,
        'function pointer reg conflicts with scratch'
    );

    const funcPtrWrongReg = (opnds[0].type === x86.type.REG && opnds[0] !== funcPtrReg);

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

    // Test if this is a static call
    var staticCall = (funcPtr.type === x86.type.LINK);

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

        for (i = argRegNb, spoffset = 0; i < funcArgs.length; ++i, spoffset += refByteNb)
        {
            var arg = funcArgs[i];

            if (arg.type === x86.type.MEM)
            {
                // Source memory location
                // Adjust the offset to take the displacement of the stack pointer
                // into account
                arg = Object.create(arg);
                arg.disp += stackOffset;

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
                // Make a copy of the object with the same properties
                arg = Object.create(arg);

                // Adjust the offset to take the displacement of the stack pointer
                // into account
                arg.disp += stackOffset;
            }

            map.add(arg, reg);
        }
    }

    map.orderAndInsertMoves( 
        function (move)
        {
            tltor.asm.mov(move.uses[0], move.uses[1]);
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
    const funcPtrIndex = backendCfg.funcPtrIndex;
    const funcPtrReg = backendCfg.physReg[funcPtrIndex];
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
        scratchRegs.length >= 2, 
        "Insufficient number of scratch registers available"
    );
    const index   = scratchRegs[0];
    const temp    = scratchRegs[1];

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

    const altStack = reg.rbp.subReg(width);

    const scratchReg = reg.rdi.subReg(width);

    const xSP = reg.rsp.subReg(width);

    const xAX = reg.rax.subReg(width);

    const stackAlignByteNb = backendCfg.stackAlignByteNb;

    const cfct = this.uses[0];

    const fctAddr = cfct.funcPtr;

    const callDest  = tltor.asm.linked(
                    cfct.funcName, 
                    function (dstAddr) { return dstAddr
                                                .addOffset(4)
                                                .getAddrOffsetBytes(fctAddr); },
                    fctAddr.width());
                    
    const numArgs = opnds.length - 1;        

    assert(width === 32, "Only 32-bits FFI calls are supported for now"); 

    assert(altStack !== scratchReg, 'alt stack reg is the same as scratch reg');
    backendCfg.argsReg.forEach(function (r) { assert(altStack !== r, 'invalid alt stack reg'); });
    backendCfg.argsReg.forEach(function (r) { assert(scratchReg !== r, 'invalid scratch reg'); });

    // Spill an operand on the context if it is in the scratch register
    tltor.spillOnCtx([scratchReg, altStack], opnds);

    // Iteration
    var i;
    var offset;

    // Invariant: opnds are constants, registers or memory location
    //            containing C-valid object or primitive values

    // Stack space taken by arguments
    var argStackSpace = 0;

    // Offsets to write each argument at
    var argOffsets = [];

    // For each argument
    for (i = 0; i < numArgs; ++i)
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

    // TODO: Find a way to calculate the mask for 64 bits pointer values
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

    // Write argument on stack in reverse order
    for (i = 0; i < numArgs; ++i)
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

    // Prepare stack pointer for C calling convention
    if (stack !== xSP)
    {
        tltor.asm.
        mov(stack, xSP);
    }

    // Call the C function
    tltor.asm.
    call(callDest);

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
    // TODO: for now, move from input to output
    // Eventually, should always use same register... noop

    const dest = this.regAlloc.dest;

    if (opnds[0] === dest)
    {
        // Do nothing
    }
    else
    {
        tltor.asm.
        mov(opnds[0], dest);
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

    if (opnds[2].type !== x86.type.REG && !tltor.asm.isImmediate(opnds[2]))
    {
        print(opnds[2].type);
    }
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
        tltor.asm.mov(opnds[2], memLoc, typeSize);
    }

    /*
    else if (opnds[2].type === x86.type.MEM)
    {
        // For a store memory to memory, we need a temporary register 
        const tempOffset = backendCfg.ctxLayout.getFieldOffset(["temp"]);
        const temp = mem(tempOffset, context);

        // On x86 32 bits, only AL, BL, CL, DL can be accessed directly
        // in case the typeSize is 8
        const avlbRegs = (tltor.asm.target === x86.target.x86) ? [
            reg.rax.subReg(width),
            reg.rbx.subReg(width),
            reg.rcx.subReg(width),
            reg.rdx.subReg(width)
        ] : backendCfg.physReg;
        arraySetRem(avlbRegs, context);
        arraySetRemAll(avlbRegs, opnds); 

        assert(avlbRegs.length > 0, "No scratch register available");
        const scratch = avlbRegs[0];

        tltor.asm.
        mov(scratch, temp).
        mov(opnds[2], scratch).
        mov(scratch.subReg(typeSize), memLoc, typeSize).
        mov(temp, scratch);
    } */ 

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
    // Assembler imports
    const reg = x86.Assembler.prototype.register;
    const $ = x86.Assembler.prototype.immediateValue;

    // Configuration imports
    const target = tltor.params.target; 
    const backendCfg = target.backendCfg;
    const width = target.ptrSizeBits;

    const xAX = reg.rax.subReg(width);
    const xBX = reg.rbx.subReg(width);
    const xCX = reg.rcx.subReg(width);
    const xDX = reg.rdx.subReg(width);

    const ctxAlign = tltor.params.staticEnv.getBinding("CTX_ALIGN").value;
    const ctx = backendCfg.context; 
    const dest = this.regAlloc.dest;

    assert(ctx === xAX || ctx === xBX || ctx === xCX || ctx === xDX,
           "Invalid register for context object");
    //assert(ctx === dest, "Invalid register assigned to context object");
    assert(ctxAlign === 256, "Invalid alignment value for context object");
    
    tltor.asm.
    mov(ctx, dest);
};

SetCtxInstr.prototype.genCode = function (tltor, opnds)
{
    // Configuration imports
    const backendCfg = tltor.params.target.backendCfg;

    tltor.asm.mov(opnds[0], backendCfg.context);
};

MoveInstr.prototype.genCode = function (tltor, opnds)
{
    // Assembler imports
    const reg = x86.Assembler.prototype.register;

    // Configuration imports
    const target = tltor.params.target; 
    const backendCfg = target.backendCfg;
    const width = target.ptrSizeBits;

    const xAX = reg.rax.subReg(width);

    if (opnds[0].type === x86.type.MEM &&
        opnds[1].type === x86.type.MEM)
    {
        tltor.asm.
        mov(xAX, tltor.temp).
        mov(opnds[0], xAX).
        mov(xAX, opnds[1]).
        mov(tltor.temp, xAX);
    } else
    {
        tltor.asm.
        mov(opnds[0], opnds[1], tltor.params.target.ptrSizeBits);
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
