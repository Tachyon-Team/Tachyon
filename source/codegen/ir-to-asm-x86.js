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
irToAsm.getEntryPoint = function (irfunc, name, params)
{
    if (name === undefined)
        name = "default";
    
    const width = params.target.ptrSizeBits;
    var ep;

    function setEntryPoint (ep, name)
    {
        if (name === undefined)
            name = "default";

        this.entryPoints[name] = ep;
    };

    function getEntryPoint (name)
    {
        if (name === undefined)
            name = "default";

        return this.entryPoints[name];
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

    // Store the compilation parameters on the translator
    that.params = params;

    that.asm = new x86.Assembler(params.target.ptrSizeBits === 64 ? 
                                 x86.target.x86_64 : x86.target.x86);
    that.asm.codeBlock.bigEndian = false;
    that.fct = null;

    // Use the context register value as a true (nonzero) boolean
    that.trueVal = params.target.backendCfg.stack;

    // The false boolean must be 0
    that.falseVal = $(0);

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
            // Get the function entry point
            var entryPoint = irToAsm.getEntryPoint(
                opnd,
                undefined, 
                that.params
            );
            
            // Only static calls to IRFunctions should
            // calculate an offset
            if (this instanceof CallInstr && index === 0)
            {
                entryPoint = Object.create(entryPoint);
                entryPoint.linkValue = function (dstAddr) 
                { 
                    var bytes = dstAddr
                                .addOffset(offset)
                                .getAddrOffsetBytes(this.srcAddr);
                    return bytes;
                };
            }

            return entryPoint;            
        }

        // If this is a string
        if (opnd instanceof ConstValue && typeof opnd.value === "string")
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

            if (instr.regAlloc.opnds === undefined)
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

irToAsm.translator.prototype.prelude = function ()
{
    // TODO: Correctly handle a number of arguments
    //       passed lesser than the number of arguments
    //       expected
    const reg = x86.Assembler.prototype.register;
    const mem = x86.Assembler.prototype.memory;
    const $ = x86.Assembler.prototype.immediateValue;

    const that = this; 
    const target = this.params.target;
    const width = target.ptrSizeBits;
    const refByteNb = target.ptrSizeBytes;
    const backendCfg = target.backendCfg;
    const stack = backendCfg.stack;

    const cstack =  reg.rsp.subReg(width);
    const scratch = backendCfg.physReg[backendCfg.physReg.length - 1];

    const argsRegNb = backendCfg.argsReg.length;
    const spillNb = this.fct.regAlloc.spillNb;
    const spoffset = spillNb + (this.fct.usesArguments === true ? 
                                argsRegNb : 0);


    // Add an entry point for static calls
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
            tltor.asm.
            mov(cstack, stack);
        }
    }

    // TODO: Correctly handle a number of arguments
    //       passed lesser than the number of arguments
    //       expected
    if (spoffset > 0)
    {
        this.asm.sub($(spoffset*refByteNb), stack);
    }

    if (this.fct.usesArguments)
    {

        /*
        At first we have the following stack frame:

        -----------------
        Spills            <-- stack pointer
        -----------------
        Empty
        -----------------
        Return Address      
        -----------------
        Arguments (only those not passed in registers)
        -----------------


        In the end, we want the following stack frame:

        -----------------
        Spills            <-- stack pointer
        -----------------
        Return Address      
        -----------------
        Arguments passed in registers
        Arguments passed on stack
        -----------------
        */

        const retAddrOrigOffset = (spoffset*refByteNb);
        const retAddrNewOffset  = (spillNb*refByteNb);

        // Change return address location on stack
        this.asm.
        mov(mem(retAddrOrigOffset, stack), scratch).
        mov(scratch, mem(retAddrNewOffset, stack));

        // Push each argument passed in registers on stack
        // (overwrites the previous return address location)
        backendCfg.argsReg.forEach(function (reg, index) {
            that.asm.
            mov(reg, mem((index + spillNb + 1)*refByteNb, stack));
        });
    }
};

//=============================================================================
//
// Translation of IR instructions to x86 machine code
//
//=============================================================================

/* code generation for each ir instruction */
PhiInstr.prototype.genCode = function (tltor, opnds)
{
    // Do nothing
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
    const callSiteArgIndex = (tltor.fct.usesArguments === true ?
                              argIndex : (argIndex - argRegNb));
    
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

    if ((argIndex < argRegNb) && !tltor.fct.usesArguments)
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
    // Configuration imports
    const backendCfg = tltor.params.target.backendCfg;

    // Register used for the output value
    const dest = this.regAlloc.dest;
    const stack = backendCfg.stack;
    const refByteNb = stack.width() >> 3;
 
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
        mov(opnds[1], backendCfg.temp).
        mov(opnds[0], dest).
        sub(backendCfg.temp, dest);
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

        tltor.asm.mul(op1, this.type.getSizeBits(tltor.params.target));
    }

    // Otherwise, a signed result is expected
    else
    {
        if (opnds[0].type === x86.type.IMM_VAL)
        {
            tltor.asm.imul(opnds[1], dst, opnds[0], this.type.getSizeBits(tltor.params.target));
        }

        else if (opnds[1].type === x86.type.IMM_VAL)
        {
            tltor.asm.imul(opnds[0], dst, opnds[1], this.type.getSizeBits(tltor.params.target));
        }

        else if (opnds[0] === dst)
        {
            tltor.asm.imul(opnds[1], dst, undefined, this.type.getSizeBits(tltor.params.target));
        }

        else if (opnds[1] === dst)
        {
            tltor.asm.imul(opnds[0], dst, undefined, this.type.getSizeBits(tltor.params.target));
        }

        else
        {
            tltor.asm.mov(opnds[0], dst);
            tltor.asm.imul(opnds[1], dst, undefined, this.type.getSizeBits(tltor.params.target));
        }
    }
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

        tltor.asm.div(dsor.value, this.type.getSizeBits(tltor.params.target));
    }
    else
    {
        // Sign-extend EAX into EDX:EAX using CDQ
        tltor.asm.cdq();

        tltor.asm.idiv(dsor.value, this.type.getSizeBits(tltor.params.target));
    }
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

    if (dest != opnds[0])
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

LsftInstr.prototype.genCode = function (tltor, opnds)
{
    const dest = this.regAlloc.dest;

    var shiftAmt;
    if (opnds[0].type == x86.type.IMM_VAL)
        shiftAmt = opnds[1].value % 256;
    else
        shiftAmt = opnds[1];

    // FIXME: when both operands are non-constant, should block CL register

    if (opnds[0] === dest)
    {
        tltor.asm.sal(shiftAmt, dest);
    }
    else
    {
        tltor.asm.
        mov(opnds[0], dest).
        sal(shiftAmt, dest);
    }
};

RsftInstr.prototype.genCode = function (tltor, opnds)
{
    const dest = this.regAlloc.dest;

    var shiftAmt;
    if (opnds[0].type == x86.type.IMM_VAL)
        shiftAmt = opnds[1].value % 256;
    else
        shiftAmt = opnds[1];

    // Use the arithmetic right shift
    if (opnds[0] === dest)
    {
        tltor.asm.sar(shiftAmt, dest);
    }
    else
    {
        tltor.asm.
        mov(opnds[0], dest).
        sar(shiftAmt, dest);
    }
};

UrsftInstr.prototype.genCode = function (tltor, opnds)
{
    const dest = this.regAlloc.dest;

    var shiftAmt;
    if (opnds[0].type == x86.type.IMM_VAL)
        shiftAmt = opnds[1].value % 256;
    else
        shiftAmt = opnds[1];

    // Use the logical right shift
    if (opnds[0] === dest)
    {
        tltor.asm.shr(shiftAmt, dest);
    }
    else
    {
        tltor.asm.
        mov(opnds[0], dest).
        shr(shiftAmt, dest);
    }
};

LtInstr.prototype.genCode = function (tltor, opnds)
{
    const dest = this.regAlloc.dest;

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
        tltor.asm.cmp(
            opnds[1],
            opnds[0],
            (opnds[0].width === undefined && opnds[1].width === undefined)?
            this.type.getSizeBits(tltor.params.target):undefined
        );
    }

    tltor.asm.
    mov(tltor.falseVal, dest).
    cmovl(tltor.trueVal, dest);
};

LeInstr.prototype.genCode = function (tltor, opnds)
{
    const dest = this.regAlloc.dest;

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
        tltor.asm.cmp(opnds[1], opnds[0], this.uses[0].type.getSizeBits(tltor.params.target));
    }

    tltor.asm.
    mov(tltor.falseVal, dest).
    cmovle(tltor.trueVal, dest);
};

GtInstr.prototype.genCode = function (tltor, opnds)
{
    const dest = this.regAlloc.dest;

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
        tltor.asm.cmp(opnds[1], opnds[0], this.type.getSizeBits(tltor.params.target));
    }

    tltor.asm.
    mov(tltor.falseVal, dest).
    cmovnle(tltor.trueVal, dest);
};

GeInstr.prototype.genCode = function (tltor, opnds)
{
    const dest = this.regAlloc.dest;

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
        tltor.asm.cmp(opnds[1], opnds[0], this.type.getSizeBits(tltor.params.target));
    }

    tltor.asm.
    mov(tltor.falseVal, dest).
    cmovnl(tltor.trueVal, dest);
};

EqInstr.prototype.genCode = function (tltor, opnds)
{
    const dest = this.regAlloc.dest;

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
        tltor.asm.cmp(opnds[1], opnds[0], this.uses[0].type.getSizeBits(tltor.params.target));
    }
    else
    {
        tltor.asm.cmp(opnds[0], opnds[1]);
    }

    tltor.asm.
    mov(tltor.falseVal, dest).
    cmovz(tltor.trueVal, dest);
};

NeInstr.prototype.genCode = function (tltor, opnds)
{
    const dest = this.regAlloc.dest;

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
        tltor.asm.cmp(opnds[1], opnds[0], this.uses[1].type.getSizeBits(tltor.params.target));
    }
    else
    {
        tltor.asm.cmp(opnds[0], opnds[1]);
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

    // Register used for the return value
    const offset = tltor.fct.regAlloc.spillNb;
    const retValReg = (tltor.fct.cProxy === false) ? 
                      backendCfg.retValReg : reg.rax.subReg(width);
    const cstack = reg.rsp.subReg(width);

    // Remove all spilled values from stack
    if (offset > 0)
    {
        tltor.asm.add($(offset*refByteNb), backendCfg.stack);
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
        if (backendCfg.stack !== cstack)
        {
            tltor.asm.
            mov(backendCfg.stack, cstack);
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
    }
  
    // Return address is just under the stack pointer
    if (tltor.fct.usesArguments)
    {
        // Stack frame has been modified, pop the 
        // arguments passed in registers that were 
        // moved to the stack
        const argsRegNb = backendCfg.argsReg.length;
        tltor.asm.ret($(argsRegNb*refByteNb));
    } else
    {
        tltor.asm.ret();
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
        cmp($(0), opnds[0], this.uses[0].type.getSizeBits(tltor.params.target)).
        je(falseLabel).
        jmp(trueLabel);
    }
};

// For now, acts as a return 
ThrowInstr.prototype.genCode = RetInstr.prototype.genCode;

//CatchInstr

CallInstr.prototype.genCode = function (tltor, opnds)
{
    // Assembler imports
    const $ = x86.Assembler.prototype.immediateValue;
    const mem = x86.Assembler.prototype.memory;

    // Configuration imports
    const target = tltor.params.target;
    const backendCfg = target.backendCfg;
    const refByteNb = target.ptrSizeBytes;

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

    // If the function pointer is in a register that will be
    // used for the arguments or scratch
    if (opnds[0].type === x86.type.REG && opnds[0] !== funcPtrReg)
    {
        tltor.asm.mov(opnds[0], funcPtrReg);
        var funcPtr = funcPtrReg;
    }
    else
    {
        var funcPtr = opnds[0];
    }

    // Test if this is a static call
    var staticCall = (funcPtr.type === x86.type.LINK);

    // Get the function arguments
    var funcArgs = opnds.slice(1);


    // Index for the last argument passed in a register 
    const lastArgIndex = argRegNb - 1;

    // Number of operands that must be spilled at the call site
    var spillNb = funcArgs.length - argRegNb;
    spillNb = (spillNb < 0) ? 0 : spillNb;
    
    // Stack pointer offset for all spilled operands
    const spillOffset = spillNb * refByteNb;

    // Stack pointer offset
    var spoffset;

    // Temporary opnd
    var opnd;

    // Allocate space on stack for extra args
    if (spillOffset > 0)
    {
        tltor.asm.sub($(spillOffset), stack);

        for (i = argRegNb, spoffset = 0; i < funcArgs.length; ++i, spoffset += refByteNb)
        {
            var arg = funcArgs[i];

            if (arg.type === x86.type.MEM)
            {
                // Source memory location
                // Adjust the offset to take the displacement of the stack pointer
                // into account
                arg = Object.create(arg);
                arg.disp += spillOffset;

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
            if (arg.type === x86.type.MEM)
            {
                // Make a copy of the object with the same properties
                arg = Object.create(arg);

                // Adjust the offset to take the displacement of the stack pointer
                // into account
                arg.disp += spillOffset;
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
    const ctxAlign = tltor.params.staticEnv.getBinding("CTX_ALIGN").value;

    // Store the number of arguments in the lower bits of the context register
    assert(ctxAlign === 256, "Invalid alignment for context object");
    assert(funcArgs.length < ctxAlign,
           "Too many arguments for call instruction, number of arguments" +
           " is currently limited to " + (ctxAlign - 1));

    // TODO: Check why this fails for some static calls
    if (!staticCall)
    {
        tltor.asm.mov($(funcArgs.length), ctx.subReg(8));
    }

    // Call the function by its address
    tltor.asm.call(funcPtr);
    
    // Remove return address and extra args
    if (spillOffset > 0)
    {
        tltor.asm.add($(spillOffset), stack);
    }

    // If this function has a continuation label
    if (this.targets[0] !== undefined)
    {
        // Label for the continuation
        const continue_label = tltor.label(this.targets[0], this.targets[0].label);

        // Jump to continue_label
        tltor.asm.jmp(continue_label);
    }
};

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

        var opndSizeBytes = this.uses[i+1].type.getSizeBytes(tltor.params.target); 

        argStackSpace += opndSizeBytes;

        // Align the offset for the next argument
        var rem = argStackSpace % refByteNb;
        if (rem != 0)
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

        var opndSizeBits = this.uses[i+1].type.getSizeBits(tltor.params.target);

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
        this.uses[0].type === IRType.rptr,
        'load can only operate on raw pointers'
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
    if (this.type.getSizeBits(tltor.params.target) < IRType.pint.getSizeBits(tltor.params.target))
    {
        // If we are loading a signed value
        if (this.type.isSigned())
        {
            // Sign-extend the value
            tltor.asm.movsx(memLoc, dst, this.type.getSizeBits(tltor.params.target));
        }
        else
        {
            // Zero-extend the value
            tltor.asm.movzx(memLoc, dst, this.type.getSizeBits(tltor.params.target));
        }
    }
    else
    {
        // Load the value directly
        tltor.asm.mov(memLoc, dst, this.type.getSizeBits(tltor.params.target));
    }
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
        opnds[2].type === x86.type.REG || opnds[2].type === x86.type.IMM_VAL,
        'cannot perform store from memory to memory'
    );

    assert (
        this.uses[0].type === IRType.rptr,
        'store can only operate on raw pointers'
    );

    // Assembler imports
    const mem = x86.Assembler.prototype.memory; 
    const reg = x86.Assembler.prototype.register;  

    // Configuration imports
    const target = tltor.params.target; 
    const width = target.ptrSizeBits;

    const xAX = reg.rax.subReg(width);

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
    var typeSize = this.uses[2].type.getSizeBits(tltor.params.target);

    // If the value to store is an immediate
    if (opnds[2].type === x86.type.IMM_VAL)
    {
        // Store it directly
        tltor.asm.mov(opnds[2], memLoc, typeSize);
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
            // xAX has been reserved for this case
            tltor.asm.mov(opnds[2], xAX);    
            srcReg = reg.al;
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
    
    // Mask the lower bits of the context object to ensure a valid reference
    tltor.asm.
    mov($(0), ctx.subReg(8)).
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
    const temp = backendCfg.temp;

    const xAX = reg.rax.subReg(width);


    if (opnds[0].type === x86.type.MEM &&
        opnds[1].type === x86.type.MEM)
    {
        tltor.asm.
        mov(xAX, temp).
        mov(opnds[0], xAX).
        mov(xAX, opnds[1]).
        mov(temp, xAX);
    } else
    {
        tltor.asm.
        mov(opnds[0], opnds[1], tltor.params.target.ptrSizeBits);
    }
};

GetArgValInstr.prototype.genCode = function (tltor, opnds)
{
    assert(tltor.fct.usesArguments || tltor.fct.funcName === "makeArgObj",
           "get_arg_val should only be used in function using the arguments" + 
           " object"
    );
    // Assembler imports
    const mem = x86.Assembler.prototype.memory; 

    // Configuration imports
    const target = tltor.params.target; 
    const backendCfg = target.backendCfg;
    const refByteNb = target.ptrSizeBytes;

    const dest = this.regAlloc.dest;
    const stack = backendCfg.stack;
    const spillNb = tltor.fct.regAlloc.spillNb;
    const spoffset = (spillNb + 1) * refByteNb;

    if (opnds[0].type === x86.type.REG)
    {
        tltor.asm.
        mov(mem(spoffset, stack, opnds[0], refByteNb), dest);
    } else
    {
        assert(opnds[0].type === x86.type.MEM,
               "Invalid index operand type");
        tltor.asm.
        mov(opnds[0], dest).
        mov(mem(spoffset, stack, dest, refByteNb), dest);
    }
};


GetNumArgsInstr.prototype.genCode = function (tltor, opnds)
{
    // Assembler imports
    const $ = x86.Assembler.prototype.immediateValue;

    // Configuration imports
    const backendCfg = tltor.params.target.backendCfg;

    const dest = this.regAlloc.dest;
    const ctx  = backendCfg.context;

    const ctxAlign = tltor.params.staticEnv.getBinding("CTX_ALIGN").value;
    const mask = ctxAlign - 1;

    // For now, assume the number of arguments is always representable
    // on the number of bits available on the context register
    tltor.asm.
    mov($(mask), dest).
    and(ctx, dest);
};
