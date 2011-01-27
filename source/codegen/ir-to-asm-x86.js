/**
@fileOverview

Translate the low-level IR to machine dependent assembly code.

@copyright
Copyright (c) 2010 Tachyon Javascript Engine, All Rights Reserved
*/

/** @namespace */
var irToAsm = {};

(function () { // local namespace

const reg = x86.Assembler.prototype.register;
const ESP = reg.esp;
const EBP = reg.ebp;
const EAX = reg.eax;
const EBX = reg.ebx;
const ECX = reg.ecx;
const EDX = reg.edx;
const ESI = reg.esi;
const EDI = reg.edi;
const $   = x86.Assembler.prototype.immediateValue;
const mem = x86.Assembler.prototype.memory;

/** @namespace */
irToAsm.config = {};

// Constant values
// TODO: Remove once put_prop and get_prop are not used anymore
irToAsm.config.NULL  = $(0);

/*
// Global object configuration
// TODO: Remove once the global object exist in the heap
irToAsm.config.maxGlobalEntries = 16;
*/

// Register configuration
// TODO: replace stack handling in ir_call and ir_ret to allow
//       using EBP instead of ESP
irToAsm.config.stack   = ESP;
irToAsm.config.context = ESI;

// Registers available for register allocation.
irToAsm.config.physReg = [EAX, EBX, ECX, EDX, EBP, EDI];

// Reserved registers are indexes into the physReg array since the 
// register allocation algorithm assumes an index into the physReg
// array to stay platform-independent.

// Register index for call's return value
irToAsm.config.retValIndex = 0;
// Registers for the corresponding CallInstr operands. The first
// operands will be assigned those registers in their order of 
// appearance. The remaining operands will be passed on the stack.
// The first position corresponds to arg 0 index, the second to arg 1, etc.
irToAsm.config.argsIndex    = [2, 1, 0, 3];

// For convenience in the ir-to-asm code, references to registers
// are derived from the argsIndex
irToAsm.config.retValReg = irToAsm.config.physReg[irToAsm.config.retValIndex];
irToAsm.config.argsReg = 
    irToAsm.config.argsIndex.map( 
        function (index) { return irToAsm.config.physReg[index]; });


// TODO: Would we want to have distinct caller and callee save registers?

// Target
irToAsm.config.target = x86.target.x86;

// Let's reserve a temporary location on the context object for cases where
// all registers are in use
irToAsm.config.temp = mem(3*(irToAsm.config.target === x86.target.x86 ? 4 : 8),
                          irToAsm.config.context);

irToAsm.config.stackAlignByteNb = 16;

/** 
    @private
    Returns an entry point for the function.
*/
irToAsm.getEntryPoint = function (irfunc, name, config)
{
    if (name === undefined)
        name = "default";
    
    var ep;
    const width = (config.target === x86.target.x86) ? 32 : 64;
    const offset = width >> 3;

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
                    function (dstAddr) { 
                        var bytes = dstAddr
                                    .addOffset(offset)
                                    .getAddrOffsetBytes(this.srcAddr);
                      return bytes;},
                    width);
        irfunc.linking.setEntryPoint(ep, name);
    }

    return ep;
};


/**
@class
Returns an object allocating stack slots for spilling during
register allocation.
*/
irToAsm.spillAllocator = function (config)
{
    var that = Object.create(irToAsm.spillAllocator.prototype);
    that.slots = [];
    that.config = config;
    return that;
};
/** Returns a new assembly memory object */
irToAsm.spillAllocator.prototype.newSlot = function ()
{
    // Memory is byte addressed
    var offset = (this.slots.length * this.config.stack.width()) >> 3;
    var s = mem(offset, this.config.stack);
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
irToAsm.translator = function (config, params)
{
    assert(
        config !== undefined,
        'invalid translator config'
    );

    var that = Object.create(irToAsm.translator.prototype);

    // Store the compilation parameters on the translator
    that.params = params;

    that.asm = new x86.Assembler(config.target);
    that.asm.codeBlock.bigEndian = false;
    that.fct = null;

    that.config = config;

    // Use the context register value as a true (nonzero) boolean
    that.trueVal = config.stack;

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

    // Maintain the function object throughout the translation
    // to have to information from register allocation 
    this.fct = fct;

    function replace(opnd)
    {
        if (opnd instanceof ConstValue && typeof opnd.value === "string" )
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
                opnds = instr.uses.map(replace);
                instr.genCode(that, opnds);
            } 
            else
            {
                // Replace constants by immediate values
                opnds = instr.regAlloc.opnds.map(replace);

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
        s,
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
 
    const byteLength = this.config.stack.width() >> 3;
    const cstack = this.asm.target === x86.target.x86 ? ESP : reg.rsp;
    var spillNb = this.fct.regAlloc.spillNb;

    // Add an entry point for static calls
    var lobj = irToAsm.getEntryPoint(this.fct, undefined, this.config);
    this.asm.provide(lobj);

    if (this.fct.cProxy)
    {
        if (this.asm.target === x86.target.x86)
        {
            // We follow 32 bits Windows, Linux, BSD and Mac OS X
            // callee-save convention
            this.asm.
            push(EBX).
            push(ESI).
            push(EDI).
            push(EBP);
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
        if (cstack !== this.config.stack)
        {
            tltor.asm.
            mov(cstack, this.config.stack);
        }
    }

    // TODO: Correctly handle a number of arguments
    //       passed lesser than the number of arguments
    //       expected
    if (spillNb > 0)
    {
        this.asm.sub($(spillNb*byteLength), this.config.stack);
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
    // Register used for the return value
    const dest = this.regAlloc.dest;

    // Index of the current argument
    const argIndex = this.argIndex;

    // Array of registers reserved for passing arguments
    const argsReg = tltor.config.argsReg;

    // Number of registers used for passing arguments
    const argRegNb = (tltor.fct.cProxy === true) ? 0 
                     : tltor.config.argsIndex.length;

    // Number of bytes in a reference
    const refByteNb = tltor.config.stack.width() >> 3;

    // Number of variables spilled during register allocation
    const regAllocSpillNb = tltor.fct.regAlloc.spillNb; 

    // Index on the call site argument space
    const callSiteArgIndex = argIndex - argRegNb;
    
    // Offset due to C calling convention 
    const ccallOffset = (tltor.fct.cProxy === true) ?
                        ((tltor.asm.target === x86.target.x86) ? 4 : 6) : 0;

    // Offset to the argument on the stack
    const spoffset = (regAllocSpillNb + callSiteArgIndex + 
                      ccallOffset + 1) * refByteNb;

    // Stack pointer
    const stack = tltor.config.stack;

    // Ignore if the argument is not required
    if (dest === null)
    {
        return;
    }

    if (argIndex < argRegNb)
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
    const stack = tltor.config.stack;
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
        mov(opnds[1], tltor.config.temp).
        mov(opnds[0], dest).
        sub(tltor.config.temp, dest);
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
    // Register used for the output value
    const dst = this.regAlloc.dest;

    // If an unsigned integer result is expected
    if (this.type.isUnsigned())
    {
        // Make sure that one of the operands is in EAX
        if (opnds[0] === EAX)
        {
            var op1 = opnds[1];
        }
        else if (opnds[1] === EAX)
        {
            var op1 = opnds[0];
        }
        else
        {
            // Put operand 0 in eax
            tltor.asm.mov(opnds[0], EAX);
            var op1 = opnds[1];
        }
        
        // If operand 1 is an immediate value, put it into EDX
        if (op1.type === x86.type.IMM_VAL)
        {
            tltor.asm.mov(op1, EDX);
            var op1 = EDX;
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
    // Register used for the return value
    const dest = this.regAlloc.dest;

    // In the end, we want to have:
    // - Dividend in EAX
    // - Divisor NOT in EAX or EDX

    var dvnd = opnds[0];
    var dsor = opnds[1];

    // If the divisor is in EAX or EDX
    if (dsor === EAX || dsor === EDX)
    {
        // Swap the divisor with EBX
        tltor.asm.xchg(dsor, EBX);

        // If the dividend is in EBX, it is now where the divisor was
        if (dvnd === EBX) 
            dvnd = dsor;

        // The divisor is now in EBX
        dsor = EBX;
    }

    // If the dividend is not in EAX, move it there
    if (dvnd !== EAX)
    {
        tltor.asm.mov(dvnd, EAX);
        dvnd = EAX;
    }

    // If the divisor is an immediate value, put it into EBX.
    // The dividend cannot be in EBX, it would have been moved
    // into EAX during the previous step
    if (dsor.type === x86.type.IMM_VAL)
    {
        tltor.asm.mov(dsor, EBX);
        dsor = EBX;
    }

    // If the output should be unsigned, use unsigned divide, otherwise
    // use signed divide 
    if (this.type.isUnsigned())
    {
        // Extend the value into EDX
        tltor.asm.mov($(0), EDX);

        tltor.asm.div(dsor, this.type.getSizeBits(tltor.params.target));
    }
    else
    {
        // Sign-extend EAX into EDX:EAX using CDQ
        tltor.asm.cdq();

        tltor.asm.idiv(dsor, this.type.getSizeBits(tltor.params.target));
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
        mov(opnds[1], dest).
        cmp(opnds[0], dest);
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
        mov(opnds[1], dest).
        cmp(opnds[0], dest);
    } 
    else if (opnds[0].type === x86.type.IMM_VAL)
    {
        tltor.asm.
        mov(opnds[0], dest).
        cmp(opnds[1], dest);
    } 
    else
    {
        tltor.asm.cmp(opnds[1], opnds[0]);
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
        tltor.asm.cmp(opnds[1], opnds[0], this.type.getSizeBits(tltor.params.target));
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
    // Register used for the return value
    const offset = tltor.fct.regAlloc.spillNb;
    const refByteNb = tltor.config.stack.width() >> 3;
    const retValReg = (tltor.fct.cProxy === false) ? tltor.config.retValReg 
                      : ((tltor.asm.target === x86.target.x86) ? EAX : reg.rax);
    const cstack = tltor.asm.target === x86.target.x86 ? ESP : reg.rsp;

    // Remove all spilled values and return address from stack
    if (offset > 0)
    {
        tltor.asm.add($(offset*refByteNb), tltor.config.stack);
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
        if (tltor.config.stack !== cstack)
        {
            tltor.asm.
            mov(tltor.config.stack, cstack);
        }

        if (tltor.asm.target === x86.target.x86)
        {
            // We follow 32 bits Windows, Linux, BSD and Mac OS X
            // callee-save convention
            tltor.asm.
            pop(EBP).
            pop(EDI).
            pop(ESI).
            pop(EBX);
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
    tltor.asm.ret();
};

IfInstr.prototype.genCode = function (tltor, opnds)
{
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

    // Register used for the return value
    const dest = this.regAlloc.dest;

    // Let's arbitrarily take the last phys reg as a scratch register
    const scratchIndex = tltor.config.physReg.length - 1;
    const scratch = tltor.config.physReg[scratchIndex];

    // Number of available register for register allocation
    const avbleRegNb = tltor.config.physReg.length;

    // Stack register
    const stack = tltor.config.stack;

    // Array of registers reserved for passing arguments
    const argsReg = tltor.config.argsReg;

    // Number of registers used for passing arguments
    const argRegNb = tltor.config.argsIndex.length;

    // Number of operands
    const opndNb = opnds.length;

    // Used for loop iterations
    var i;

    // Register object
    var reg;

    // Used for moving operands in the right registers
    var map;

    assert(
        dest === tltor.config.retValReg || dest === null,
        'invalid destination register for function call'
    );

    // If this is a static call, likely to a primitive function
    if (opnds[0] instanceof IRFunction)
    {
        // Special cases for some static functions until 
        // we have proper support for object allocation.
        
        // Those static functions use a special calling convention
        // to avoid passing the function address and a
        // 'this' reference in registers.  Since they
        // are function calls, all available registers
        // are preserved before the call, allowing
        // usage of all the physical registers to pass
        // arguments.

        const name = opnds[0].funcName;

        if (name === "makeClos")
        {
            assert(
                this.uses[1].isUndef() && opnds[2] instanceof IRFunction,
                'invalid uses for call to makeClos'
            );

            assert(
                dest !== null,
                "makeClose should have a destination register"
            );

            const irfunc = opnds[2];
            const lobj   = irToAsm.getEntryPoint(irfunc, "default", config).clone();

            // linkValue function for make closure calls
            lobj.linkValue = function getSrcAddrBytes()
            {
                return this.srcAddr.getBytes();
            }

            tltor.asm.mov(lobj, dest);

            // Return early
            return;
        }
    }
 
    // Number of bytes in a reference
    const refByteNb = tltor.config.stack.width() >> 3;

    // Register for the function address
    const funcObjReg = tltor.config.argsReg[0];

    // Index for the last argument passed in a register 
    const lastArgIndex = argRegNb - 1;

    // Number of operands that must be spilled at the call site
    var spillNb = opndNb - argRegNb;
    spillNb = (spillNb < 0) ? 0 : spillNb;
    
    // Stack pointer offset for all spilled operands
    const spillOffset = spillNb * refByteNb;

    // Stack pointer offset
    var spoffset;

    // Temporary opnd
    var opnd;

    // Make sure we still have a register left for scratch
    assert (
        argRegNb < avbleRegNb,
        'no register left for scratch'
    );

    // Make sure it is not used to pass arguments
    assert (
        !(scratchIndex in tltor.config.argsIndex),
        'invalid scratch register index'
    );

    // Allocate space on stack for extra args
    if (spillOffset > 0)
    {
        tltor.asm.sub($(spillOffset), stack);

        for (i = argRegNb, spoffset = 0;
                i < opndNb; 
                ++i, spoffset += refByteNb)
        {

            if (opnds[i].type === x86.type.MEM)
            {

                // Source memory location
                // Adjust the offset to take the displacement of the stack pointer
                // into account
                opnd = Object.create(opnds[i]);
                opnd.disp += spillOffset;

                tltor.asm.
                mov(opnd, scratch).
                mov(scratch, mem(spoffset, stack));
            } 
            else
            {
                tltor.asm.
                mov(opnds[i], mem(spoffset, stack), stack.width());
            }
        }
    }

    // Move arguments in the right registers
    map = allocator.mapping();

    for (i = 0; i < argRegNb && i < opndNb; ++i)
    {
        // If this is a static function call
        if (opnds[i] instanceof IRFunction)
        {
            // Pass undefined as the function object
            opnd = $(ConstValue.getConst(undefined).getImmValue(tltor.params));
        }
        else
        {
            opnd = opnds[i];
        }
        
        reg = argsReg[i];
        
        if (opnd !== reg)
        {
            // Fix the offset since the stack pointer has been moved
            if (opnd.type === x86.type.MEM)
            {
                // Make a copy of the object with the same properties
                opnd = Object.create(opnd);

                // Adjust the offset to take the displacement of the stack pointer
                // into account
                opnd.disp += spillOffset;
            }

            map.add(opnd, reg);
        }
    }

    map.orderAndInsertMoves( 
        function (move)
        {
            tltor.asm.mov(move.uses[0], move.uses[1]);
        },
        scratch
    );

    // If this is a static call
    if (opnds[0] instanceof IRFunction)
    {
        // Get the function entry point
        const entryPoint = irToAsm.getEntryPoint(
            this.uses[0],
            undefined, 
            tltor.config
        );

        // Call the function by its address
        tltor.asm.call(entryPoint);
    }
    else
    {
        // The first operand should be the function address
        assert (
            opnds[0].type === x86.type.REG || 
            opnds[0].type === x86.type.MEM,
            "Invalid CallInstr function operand '" + opnds[0] + "'"
        );
        
        // Call function address
        tltor.asm.call(funcObjReg);
    }
    
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
    const argsReg = tltor.config.argsReg;

    const refByteNb = tltor.config.stack.width() >> 3;

    const stack = tltor.config.stack;

    const context = tltor.config.context;

    const altStack = EBP;

    const scratchReg = EDI;

    const stackAlignByteNb = tltor.config.stackAlignByteNb;

    const cfct = this.uses[0];

    const fctAddr = cfct.funcPtr; 

    const callDest  = tltor.asm.linked(
                    cfct.funcName, 
                    function (dstAddr) { return dstAddr
                                                .addOffset(4)
                                                .getAddrOffsetBytes(fctAddr); },
                    fctAddr.width());
                    
    const numArgs = opnds.length - 1;        

    assert(stack.width() === 32, "Only 32-bits FFI calls are supported for now"); 

    assert(altStack !== scratchReg, 'alt stack reg is the same as scratch reg');
    tltor.config.argsReg.forEach(function (r) { assert(altStack !== r, 'invalid alt stack reg'); });
    tltor.config.argsReg.forEach(function (r) { assert(scratchReg !== r, 'invalid scratch reg'); });

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
        var rem = argStackSpace % tltor.params.target.ptrSizeBytes;
        if (rem != 0)
            argStackSpace += tltor.params.target.ptrSizeBytes - rem;
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
    if (stack !== ESP)
    {
        tltor.asm.
        mov(stack, ESP);
    }

    // Call the C function
    tltor.asm.
    call(callDest);

    // Move return value into Tachyon calling convention register
    if (tltor.config.retValReg !== EAX)
    {
        tltor.asm.
        mov(EAX, tltor.config.retValReg);
    }

    // Restore runtime specific registers
    tltor.asm.
    mov(mem(argStackSpace, ESP), altStack).
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

    const dst = this.regAlloc.dest;

    assert (
        this.uses[0].type === IRType.rptr,
        'store can only operate on raw pointers'
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

        // Store the value to memory
        tltor.asm.mov(srcReg, memLoc, typeSize);
    }
};

GetCtxInstr.prototype.genCode = function (tltor, opnds)
{
    // No op
};

SetCtxInstr.prototype.genCode = function (tltor, opnds)
{
    tltor.asm.mov(opnds[0], tltor.config.context);
};

MoveInstr.prototype.genCode = function (tltor, opnds)
{
    const temp = tltor.config.temp;

    if (opnds[0].type === x86.type.MEM &&
        opnds[1].type === x86.type.MEM)
    {
        tltor.asm.
        mov(EAX, temp).
        mov(opnds[0], EAX).
        mov(EAX, opnds[1]).
        mov(temp, EAX);
    } else
    {
        tltor.asm.
        mov(opnds[0], opnds[1], tltor.params.target.ptrSizeBits);
    }
};

})(); // end of local namespace
