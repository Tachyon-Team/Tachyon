/**
@fileOverview

Translate the low-level IR to machine dependent assembly code.

@copyright
Copyright (c) 2010 Tachyon Javascript Engine, All Rights Reserved
*/

/** @namespace */
var irToAsm = irToAsm || {};

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
irToAsm.config.TRUE  = $(1); 
irToAsm.config.FALSE = $(0);
irToAsm.config.NULL  = $(0);
irToAsm.config.UNDEFINED = $(0);

// Global object configuration
irToAsm.config.maxGlobalEntries = 4;

// Register configuration
irToAsm.config.scratch = EDI;
// TODO: replace stack handling in ir_call and ir_ret to allow
//       using EBP instead of ESP
irToAsm.config.stack   = ESP;
irToAsm.config.context = ESI;


// Registers available for register allocation.
irToAsm.config.physReg    = [EAX, EBX, ECX, EDX, EBP];

// Reserved registers are indexes into the physReg array since the 
// register allocation algorithm assumes an index into the physReg
// array to stay platform-independent.

// Register index for call's return value
irToAsm.config.retValIndex = 0;
// Registers for the corresponding CallInstr operands. The first
// operands will be assigned those registers in their order of 
// appearance. The remaining operands will be passed on the stack.
// The first position corresponds to arg 0 index, the second to arg 1, etc.
irToAsm.config.argsIndex    = [0, 1, 2];

// For convenience in the ir-to-asm code, references to registers
// are derived from the argsIndex
irToAsm.config.retValReg = irToAsm.config.physReg[irToAsm.config.retValIndex];
irToAsm.config.argsReg = 
    irToAsm.config.argsIndex.map( 
        function (index) { return irToAsm.config.physReg[index]; });


// TODO: Would we want to have distinct caller and callee save registers?

// Target
irToAsm.config.target = x86.target.x86;

/**
@class
Returns an object allocating stack slots for spilling during
register allocation.
*/
irToAsm.spillAllocator = function ()
{
    var that = Object.create(irToAsm.spillAllocator.prototype);
    that.slots = [];
    return that;
};
/** Returns a new assembly memory object */
irToAsm.spillAllocator.prototype.newSlot = function ()
{
    // Memory is byte addressed
    var offset = (this.slots.length * irToAsm.config.stack.width())/8;
    var s = mem(offset, irToAsm.config.stack);
    this.slots.push(s);
    return s;
};


/** 
@class
Returns a new translator object to translate IR to Assembly.
*/
irToAsm.translator = function ()
{
    var that = Object.create(irToAsm.translator.prototype);
    that.asm = new x86.Assembler(irToAsm.config.target);
    that.asm.codeBlock.bigEndian = false;
    that.strings = {};
    that.stringNb = 0;
    that.fct = null;

    that.globalLabel = that.asm.labelObj("GLOBAL_PRELUDE");
    that.contextLabel = that.asm.labelObj("CONTEXT_PRELUDE");
    that.putPropValLabel = that.asm.labelObj("PUT_PROP");
    that.getPropValLabel = that.asm.labelObj("GET_PROP");

    if (that.asm.is64bitMode())
    {
        // Global object properties
        that.G_NEXT_OFFSET = 0;  // Offset for the cell containing 
                                  // the next empty entry offset
        that.G_NEXT_OFFSET_WIDTH = 64;
        that.G_FIRST_OFFSET = 8; // Length value in bytes
        that.G_KEY_OFFSET   = 0; // Key offset is 0 (we iterate over keys)
        that.G_KEY_WIDTH = 64;
        that.G_VALUE_OFFSET = 8; // Value offset is 8 (key length is 8 bytes)
        that.G_VALUE_WIDTH = 64;
        that.G_ENTRY_LENGTH = 16; // Key (8 bytes) Value (8 bytes)

        // Register byte width
        that.REG_BYTE_WIDTH = 8;
    } else
    {
        // Global object properties
        that.G_NEXT_OFFSET = 0;  // Offset for the cell containing 
                                  // the next empty entry offset
        that.G_NEXT_OFFSET_WIDTH = 32;
        that.G_FIRST_OFFSET = 4; // Length value in bytes
        that.G_KEY_OFFSET   = 0; // Key offset is 0 (we iterate over keys)
        that.G_KEY_WIDTH = 32;
        that.G_VALUE_OFFSET = 4; // Value offset is 4 (key length is 4 bytes)
        that.G_VALUE_WIDTH = 32;
        that.G_ENTRY_LENGTH = 8; // Key (4 bytes) Value (4 bytes)

        // Register byte width
        that.REG_BYTE_WIDTH = 4;
    }

    return that;
};
/** @private assembler object */
irToAsm.translator.prototype.asm = null;
/** @private known strings so far */
irToAsm.translator.prototype.strings = {};
irToAsm.translator.prototype.stringNb = 0;
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

    var block;
    var instrIt;
    var opnds;
    var i;

    function replace(opnd)
    {
        if (opnd instanceof ConstValue && typeof opnd.value === "string" )
        {
            return $(that.stringValue(opnd.value));
        } else if (opnd instanceof ConstValue)
        {
            return $(opnd.getImmValue());
        } else 
        {
            return opnd;
        }

    };


    // Assign a unique label to this function
    // if it doesn't exist
    this.func_prelude(this.label(fct, "<func \"" + fct.funcName + "\">"));
    this.func_init();

    for (i=0; i < blockList.length; ++i)
    {
        block = blockList[i]; 

        // Generate the asm label for the current block 
        this.asm.label(this.label(block, block.label));

        // Generate all the asm instructions for each IR
        // instructions
        for (var instrIt = block.getInstrItr(); 
             instrIt.valid(); 
             instrIt.next())
        {
            instr = instrIt.get();

            if (instr instanceof MoveInstr)
            {
                opnds = instr.uses.map(replace);
                instr.genCode(this, opnds);
            } else
            {
                // Replace constants by immediate values
                opnds = instr.regAlloc.opnds.map(replace);

                assert(instr.genCode !== undefined);
                instr.genCode(this, opnds);
            }
        }

        this.asm.genListing("");
    }
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
    var value = this.strings[s];

    if (value === undefined)
    {
       value = this.stringNb++; 
       this.strings[s] = value;
    }

    return value;
};


irToAsm.translator.prototype.ir_lt = function (opnds, instr)
{
    const dest = instr.regAlloc.dest;
    
    if (dest === null)
    {
        return;
    }

    if (opnds[0].type === x86.type.IMM_VAL &&
        opnds[1].type === x86.type.IMM_VAL)
    {
        if (opnds[0].value < opnds[1].value)
        {
            this.asm.mov(irToAsm.config.TRUE, dest);
        } else
        {
            this.asm.mov(irToAsm.config.FALSE, dest);
        }
        return;
    }

    var cont = this.asm.labelObj();

    if (opnds[0].type === x86.type.MEM &&
        opnds[1].type === x86.type.MEM)
    {
        this.asm.
        mov(opnds[0], dest). 
        cmp(dest, opnds[1]); 
    } else
    {
        this.asm.
        cmp(opnds[1], opnds[0]);
    }

    this.asm.
    mov(irToAsm.config.TRUE, dest).
    jl(cont).
    mov(irToAsm.config.FALSE, dest).
    label(cont);

};

irToAsm.translator.prototype.ir_if = function (opnds, instr)
{
    const targets = instr.targets;
    var true_label = this.label(targets[0]);
    var false_label = this.label(targets[1]);

    this.asm.
    cmp(irToAsm.config.TRUE, opnds[0]).
    je(true_label).
    jmp(false_label);

};

irToAsm.translator.prototype.ir_jump = function (opnds, instr)
{
    const targets = instr.targets;
    var target = this.label(targets[0]);

    this.asm.jmp(target);

};

irToAsm.translator.prototype.ir_sub = function (opnds, instr)
{
    const that = this;
    const dest = instr.regAlloc.dest;

    if (dest === null)
    {
        return;
    }

    function xchg(opnd1, opnd2)
    {
        assert(!(opnd1.type === x86.type.MEM &&
                 opnd2.type === x86.type.MEM));

        assert(!opnd1.type === x86.type.IMM_VAL);
        assert(!opnd2.type === x86.type.IMM_VAL);
        
        that.asm.
        xor(opnd1, opnd2).
        xor(opnd2, opnd1).
        xor(opnd1, opnd2);
    };

    if (opnds[1] === dest && opnds[0].type !== x86.type.IMM_VAL)
    {
        xchg(opnds[1], opnds[0]);

        this.asm.sub(opnds[1], dest);

        xchg(opnds[1], opnds[0]);
    } else if (opnds[1] === dest && opnds[0].type === x86.type.IMM_VAL)
    {
        this.asm.
        mov(opnds[0], irToAsm.config.scratch).
        sub(opnds[1], irToAsm.config.scratch).
        mov(irToAsm.config.scratch, opnds[1]);
    } else if (opnds[0] === dest)
    {
        this.asm.
        sub(opnds[1], dest);
    } else
    {
        this.asm.
        mov(opnds[0], dest).
        sub(opnds[1], dest);
    }
};

irToAsm.translator.prototype.ir_add = function (opnds, instr)
{
    const dest = instr.regAlloc.dest;

    if (dest === null)
    {
        return;
    }

    if (opnds[1] === dest)
    {
        this.asm.
        add(opnds[0], dest);
    } else if (opnds[0] === dest)
    {
        this.asm.
        add(opnds[1], dest);
    } else
    {
        this.asm.
        mov(opnds[0], dest).
        add(opnds[1], dest);
    }

};
irToAsm.translator.prototype.get_prop_val = function ()
{
    assert(irToAsm.config.physReg.length >= 4);
    assert(value !== irToAsm.config.retValReg);

    const obj = irToAsm.config.physReg[0];
    const key = irToAsm.config.physReg[1];
    const value = irToAsm.config.physReg[2];
    const addr = irToAsm.config.physReg[3];

    const cont = this.asm.labelObj();

    this.asm.label(this.getPropValLabel);

    this.get_prop_addr(obj, key, addr);

    this.asm.
    cmp(irToAsm.config.NULL, addr).
    mov($((new ConstValue(undefined, IRType.box)).getImmValue()), 
          irToAsm.config.retValReg).
    je(cont).
    // The following instruction causes a bus error only
    // when addr is not a valid address
    //cmovne(mem(this.G_VALUE_OFFSET, addr), irToAsm.config.retValReg).
    mov(mem(this.G_VALUE_OFFSET,addr), irToAsm.config.retValReg).
    label(cont).
    ret();

};

irToAsm.translator.prototype.get_prop_addr = function (obj, key, addr)
{
    var loop = this.asm.labelObj();
    var end = this.asm.labelObj();
    var notFound = this.asm.labelObj();
    var cont = this.asm.labelObj();

    this.asm.
    mov(obj, addr).
    add($(this.G_FIRST_OFFSET), addr). // Retrieve address of first element
    add(mem(this.G_NEXT_OFFSET - this.G_FIRST_OFFSET, addr), 
        addr). // Retrieve beginning of next
    sub($(this.G_ENTRY_LENGTH), addr).       // Move to last element

    label(loop).                        // Loop from end to beginning
    sub($(this.G_FIRST_OFFSET), addr).
    cmp(obj, addr).           
    jl(end).

    add($(this.G_FIRST_OFFSET), addr).       // Address of current item
    cmp(key, mem(this.G_KEY_OFFSET, addr), 
        this.G_KEY_WIDTH).   // global[index] === key ?
    je(cont).                         // Item found on equal!

    sub($(this.G_ENTRY_LENGTH), addr).      // move to next value
    jmp(loop).

    label(end).
    mov(irToAsm.config.NULL, addr).        // no value found

    label(cont);
};

irToAsm.translator.prototype.put_prop_val = function ()
{
    assert(irToAsm.config.physReg.length >= 4);

    const obj = irToAsm.config.physReg[0];
    const key = irToAsm.config.physReg[1];
    const value = irToAsm.config.physReg[2];
    const addr = irToAsm.config.physReg[3];

    var loop = this.asm.labelObj();
    var found = this.asm.labelObj();

    this.asm.label(this.putPropValLabel);

    this.get_prop_addr(obj, key, addr);
    
    this.asm.
    cmp(irToAsm.config.NULL, addr).
    jne(found).
    mov(obj, addr).
    add($(this.G_FIRST_OFFSET), addr).          // Retrieve address of first element
    add(mem(this.G_NEXT_OFFSET, obj), addr). // Retrieve address of next element 
    // Inc entry nb
    add($(this.G_ENTRY_LENGTH), mem(this.G_NEXT_OFFSET, obj), this.G_NEXT_OFFSET_WIDTH). 
    mov(key, mem(this.G_KEY_OFFSET, addr), this.G_KEY_WIDTH).     // Add entry key
    label(found).                          
    mov(value, mem(this.G_VALUE_OFFSET, addr), this.G_VALUE_WIDTH). // Add/Update the entry value
    mov(value, irToAsm.config.retValReg).
    ret();

};

irToAsm.translator.prototype.dump_global_object = function ()
{
    this.asm.
    label(this.globalLabel);

    this.call_self();

    if (this.asm.is64bitMode())
    {
        this.asm.gen64(0);
    } else
    {
        this.asm.gen32(0); // Length
    }
   
    for (var i=0; i < irToAsm.config.maxGlobalEntries; ++i)
    {
        if (this.asm.is64bitMode())
        {
            this.asm.
            gen64(0). // Reserved space for key
            gen64(0); // Reserved space for value
        } else 
        {
            this.asm.
            gen32(0). // Reserved space for key
            gen32(0); // Reserved space for value

        }
    }
    this.asm.genListing("GLOBAL_OBJECT");
    
};

irToAsm.translator.prototype.dump_context_object = function ()
{
    this.asm.
    label(this.contextLabel);

    this.call_self();

    if (this.asm.is64bitMode())
    {
        this.asm.gen64(0).gen64(0).gen64(0);
    } else
    {
        this.asm.gen32(0).gen32(0).gen32(0);
    }
   
    this.asm.genListing("CONTEXT_OBJECT");
    
};

irToAsm.translator.prototype.call_self = function (offset)
{
    if (offset === undefined)
    {
        offset = 5;
    }
    const SELF = this.asm.labelObj();
    const retValReg = irToAsm.config.retValReg;

    this.asm.
    call(SELF).
    label(SELF).
    pop(retValReg).
    add($(offset),retValReg).
    ret().
    genListing("ADDR RETRIEVAL");

};

irToAsm.translator.prototype.ir_arg = function (opnds, instr)
{
    const dest = instr.regAlloc.dest;
    const argIndex = instr.argIndex;
    var reg;

    if (dest === null)
    {
        return;
    }

    reg = irToAsm.config.physReg[irToAsm.config.argsIndex[argIndex]];
    if (dest !== reg)
    {
        error("ir_arg: dest register '" + dest + 
              "' unexpected for argument index '" + argIndex + "'");
    }
    

};

irToAsm.translator.prototype.ir_call = function (opnds, instr)
{
    const dest = instr.regAlloc.dest;
    const targets = instr.targets;
    const that = this;
    const refByteLength = irToAsm.config.stack.width() / 8;
    const funcObjReg = irToAsm.config.argsReg[0];

    var spillNb = opnds.length - refByteLength;
    var offset = 1;
    var i;
    var continue_label = this.label(targets[0], targets[0].label);
    var reg;

    if (spillNb < 0)
    {
        spillNb = 0;
    }

    offset = (spillNb) * refByteLength;

    // Move arguments in the right registers
    var map = allocator.mapping();

    for (i=0; i < irToAsm.config.argsIndex.length && i < opnds.length; ++i)
    {

        reg = irToAsm.config.argsReg[i];
        if (opnds[i] !== reg)
        {
            map.add(opnds[i], reg);
        }
    }

    map.orderAndInsertMoves( function (move)
                             {
                                that.asm.mov(move.uses[0], move.uses[1]);
                             }, irToAsm.config.scratch);
   

    // Add extra arguments on the stack
    if (spillNb > 0)
    {
        error("TODO");
    }

    this.asm.

    // Move pointers on top of extra args
    sub($(offset), irToAsm.config.stack).

    // Call function address
    call(funcObjReg).

    // Remove return address and extra args
    add($(offset), irToAsm.config.stack).

    // Jump to continue_label
    jmp(continue_label);

};

irToAsm.translator.prototype.func_prelude = function (prelude_label)
{
    // Add the call self instructions to retrieve
    // the address of the function
    this.asm.
    label(prelude_label);

    this.call_self();
};

irToAsm.translator.prototype.func_init = function ()
{
    const byteLength = irToAsm.config.stack.width() / 8;
    var spillNb = this.fct.regAlloc.spillNb;
    if (spillNb > 0)
    {
        this.asm.sub($(spillNb*byteLength), irToAsm.config.stack);
    }
};

irToAsm.translator.prototype.ir_make_arg_obj = function (opnds, instr)
{
    const dest = instr.regAlloc.dest;
    assert(dest === null);
    // For now, let's ignore the argument object

};

irToAsm.translator.prototype.ir_make_clos = function (opnds, instr)
{
    const dest = instr.regAlloc.dest;
    const retValReg = irToAsm.config.retValReg;

    if (dest === null)
    {
        return;
    }

    assert(opnds[0] instanceof IRFunction); 

    assert(opnds[1].type === x86.type.REG);

    var fctLabel = this.label(opnds[0]);

    if (dest === retValReg)
    {
        if (opnds[1] === retValReg)
        {
            this.asm.mov(opnds[1], scratch);
        }
        this.asm.
        call(fctLabel);

        // Store the global object in the function prelude
        if (opnds[1] === retValReg)
        {
            this.asm.mov(scratch, mem(-this.REG_BYTE_WIDTH, retValReg));
        } else
        {
            this.asm.mov(opnds[1], mem(-this.REG_BYTE_WIDTH, retValReg));
        }
    } else 
    {
        this.asm.
        mov(retValReg, irToAsm.config.scratch).
        call(fctLabel).
        mov(retValReg, dest).
        mov(irToAsm.config.scratch, retValReg).
        mov(dest, irToAsm.config.scratch).

        // Store the global object in the function prelude
        mov(opnds[1], mem(-this.REG_BYTE_WIDTH, irToAsm.config.scratch));
    }

};

irToAsm.translator.prototype.ir_get_global = function (opnds, instr)
{
    const dest = instr.regAlloc.dest;

    if (dest === null)
    {
        return;
    }

    if (opnds[0].type === x86.type.REG)
    {
        this.asm.
        mov(mem(-this.REG_BYTE_WIDTH, opnds[0]), dest);
    } else if (opnds[0].type === x86.type.MEM)
    {
        this.asm.
        mov(opnds[0], irToAsm.config.scratch).
        mov(mem(-this.REG_BYTE_WIDTH, irToAsm.config.scratch), dest);
    }

};

irToAsm.translator.prototype.init = function (mainFct)
{
    const stack = irToAsm.config.stack;

    const retValReg = irToAsm.config.retValReg;
    const contextObjReg = irToAsm.config.context;

    var i;


    // Let's preserve all registers from the caller
    // except xAX (used for return value)
    if (this.asm.is64bitMode())
    {
        for (i=1; i < 16; ++i)
        {
            this.asm.push(reg.reg64(i));
        }
    } else
    {
        for (i=1; i < 8; ++i)
        {
            this.asm.push(reg.reg32(i));
        }
    }

    this.asm.
    genListing("INIT").

    // Initialise the context object
    call(this.contextLabel).
    mov(retValReg, contextObjReg). 

    call(this.globalLabel).

    // Save global object in context
    mov(retValReg, mem(0, contextObjReg)).

    // Retrieve the main function address
    call(this.label(mainFct, "<func MAIN>")).

    // Call the main function
    call(retValReg);

    // Let's restore all registers for the caller
    // except xAX (used for return value)
    if (this.asm.is64bitMode())
    {
        for (i=15; i >= 1; --i)
        {
            this.asm.pop(reg.reg64(i));
        }
    } else
    {
        for (i=7; i >= 1; --i)
        {
            this.asm.pop(reg.reg32(i));
        }
    }

    this.asm.ret();

    // Add the global object dump at the end of the init section
    this.dump_global_object();

    // Add the context object dump right after the global object
    this.dump_context_object();

    // Add the get property value code here
    this.get_prop_val();

    // Add the put property value code here
    this.put_prop_val();
};

/* code generation for each ir instruction */
PhiInstr.prototype.genCode = function (tltor, opnds)
{
    // Do nothing
};

ArgValInstr.prototype.genCode = function (tltor, opnds)
{

};

AddInstr.prototype.genCode = function (tltor, opnds)
{
    const dest = this.regAlloc.dest;

    if (this.uses[0] instanceof ConstValue &&
        this.uses[1] instanceof ConstValue)
    {
        // If the two operands are constants, directly assign
        // the sum of the operands to the register
        tltor.asm.mov($(opnds[0].value + opnds[1].value), dest);
    }

};

//SubInstr

//MulInstr

//DivInstr

//ModInstr

//AddOvfInstr

SubOvfInstr.prototype.genCode = function (tltor, opnds)
{

};

//MulOvfInstr

//NotInstr

AndInstr.prototype.genCode = function (tltor, opnds)
{

};

//OrInstr

//XorInstr

//LsftInstr

//RsftInstr

//UrsftInstr

//CompInstr

LtInstr.prototype.genCode = function (tltor, opnds)
{

};

//LeInstr

//GtInstr

//GeInstr

EqInstr.prototype.genCode = function (tltor, opnds)
{

};

NeInstr.prototype.genCode = function (tltor, opnds)
{

};

JumpInstr.prototype.genCode = function (tltor, opnds)
{

};

RetInstr.prototype.genCode = function (tltor, opnds)
{
    const dest = this.regAlloc.dest;
    const offset = tltor.fct.regAlloc.spillNb + 1;
    const refByteLength = irToAsm.config.stack.width() / 8;
    const retValReg = irToAsm.config.retValReg;

    // Remove all spilled values and return address from stack
    tltor.asm.add($(offset*refByteLength), irToAsm.config.stack);

    if (opnds[0] !== retValReg)
    {
        tltor.asm.mov(opnds[0], retValReg);
    }
  
    // Return address is just under the stack pointer
    tltor.asm.jmp(mem(-refByteLength, ESP)); 

};

IfInstr.prototype.genCode = function (tltor, opnds)
{

};

ThrowInstr.prototype.genCode = function (tltor, opnds)
{

};

//CatchInstr

CallInstr.prototype.genCode = function (tltor, opnds)
{
    const dest = this.regAlloc.dest;

    var funcName;
    var i;

    assert(dest === irToAsm.config.retValReg || 
           dest === null);

    if (opnds[0] instanceof IRFunction)
    {
        // Special cases for some static functions until 
        // we have proper support for object allocation
        name = opnds[0].funcName;
        if (name === "makeClos")
        {
            assert(this.uses[1].isUndef() &&
                   opnds[2] instanceof IRFunction);

            // Implicitly returns the function address
            // in return value register
            tltor.asm.call(tltor.label(opnds[2]));
        } else if (name === "getPropVal")
        {
            // Move arguments in the right registers
            var map = allocator.mapping();

            for (i = 2; i < 4; ++i)
            {
                if (opnds[i] !== irToAsm.config.physReg[i-2])
                {
                    map.add(opnds[i], irToAsm.config.physReg[i-2]);
                }
            }

            map.orderAndInsertMoves( function (move)
                                     {
                                        tltor.asm.mov(move.uses[0], move.uses[1]);
                                     }, irToAsm.config.physReg[3]);

            // Implicitly returns the property value
            // in return value register
            tltor.asm.call(tltor.getPropValLabel);
        } else if (name === "putPropVal")
        {
            // Move arguments in the right registers
            var map = allocator.mapping();

            for (i = 2; i < 5; ++i)
            {
                if (opnds[i] !== irToAsm.config.physReg[i-2])
                {
                    map.add(opnds[i], irToAsm.config.physReg[i-2]);
                }
            }

            map.orderAndInsertMoves( function (move)
                                     {
                                        tltor.asm.mov(move.uses[0], move.uses[1]);
                                     }, irToAsm.config.physReg[3]);

            // Implicitly returns the property value
            // in return value register
            tltor.asm.call(tltor.putPropValLabel);
        } else
        {
            error("Unknown static function '" + name + "'");
        }


    } else if (opnds[0].type === x86.type.REG || 
               opnds[0].type === x86.type.MEM)
    {
        // Regular function call

    } else
    {
        error("Invalid CallInstr function operand '" + opnds[0] + "'");
    }
};

//ConstructInstr

ICastInstr.prototype.genCode = function (tltor, opnds)
{

};

//IToFPInstr

//FPToIInstr

LoadInstr.prototype.genCode = function (tltor, opnds)
{
    const dest = this.regAlloc.dest;

    // Boxed return value case
    assert(this.type === IRType.box);

    if (opnds[0].type === x86.type.REG &&
        opnds[1].type === x86.type.REG)
    {
        assert(this.uses[0].type === IRType.rptr &&
               this.uses[1].type.isInt());

        //   Displacement in a register case 
        if (opnds[1] !== dest)
        {
            tltor.asm.mov(opnds[1], dest);
        }

        tltor.asm.
        add(opnds[0], dest).
        mov(mem(0, dest), dest);

    } else if (opnds[0].type === x86.type.REG &&
               opnds[1].type === x86.type.IMM_VAL)
    {
        assert(this.uses[0].type === IRType.rptr &&
               this.uses[1].type.isInt());

        //   Constant displacement
        tltor.asm.mov(mem(opnds[1].value, opnds[0]), dest);
    }


};

//StoreInstr

GetCtxInstr.prototype.genCode = function (tltor, opnds)
{
    // No op
};

//SetCtxInstr

MoveInstr.prototype.genCode = function (tltor, opnds)
{
    assert(!(opnds[0].type === x86.type.MEM &&
             opnds[1].type === x86.type.MEM));

    tltor.asm.mov(opnds[0], opnds[1]);
};


})(); // end of local namespace
