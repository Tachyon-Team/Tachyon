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

    // Maintain the function object throughoutt the translation
    // to have to information from register allocation 
    this.fct = fct;

    var block;
    var instrIt;
    var opnds;
    var i;

    function replace(opnd)
    {
        if (opnd instanceof ConstValue && opnd.isInt())
        {
            return $(opnd.value);
        } else if (opnd instanceof ConstValue && opnd.isUndef())
        {
            return irToAsm.config.UNDEFINED;
        } else if (opnd instanceof ConstValue && typeof opnd.value === "string" )
        {
            return $(that.stringValue(opnd.value));
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
                this.asm.mov(opnds[0], opnds[1]);
            } else if (instr instanceof PhiInstr)
            {
                // ignore
            } else
            {
                // Replace constants by immediate values
                opnds = instr.regAlloc.opnds.map(replace);

                this["ir_" + instr.mnemonic](opnds, instr);
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

irToAsm.translator.prototype.ir_get_prop_val = function (opnds, instr)
{
    const obj = opnds[0];
    const dest = instr.regAlloc.dest;
    const targets = instr.targets; 
    const continue_label = this.label(targets[0]);

    if (dest === null)
    {
        return;
    }

    var cont = this.asm.labelObj();

    this.get_prop_addr(opnds, irToAsm.config.scratch);

    this.asm.
    cmp(irToAsm.config.NULL, irToAsm.config.scratch).
    je(cont).
    mov(mem(this.G_VALUE_OFFSET, irToAsm.config.scratch), irToAsm.config.scratch).

    label(cont).
    mov(irToAsm.config.scratch, dest).
    jmp(continue_label);

};

irToAsm.translator.prototype.get_prop_addr = function (opnds, dest)
{
    const obj = opnds[0];
    const key = opnds[1];

    var loop = this.asm.labelObj();
    var end = this.asm.labelObj();
    var notFound = this.asm.labelObj();
    var cont = this.asm.labelObj();

    this.asm.
    mov(obj, irToAsm.config.scratch).
    add($(this.G_FIRST_OFFSET), irToAsm.config.scratch). // Retrieve address of first element
    add(mem(this.G_NEXT_OFFSET - this.G_FIRST_OFFSET, irToAsm.config.scratch), 
        irToAsm.config.scratch). // Retrieve beginning of next
    sub($(this.G_ENTRY_LENGTH), irToAsm.config.scratch).       // Move to last element

    label(loop).                        // Loop from end to beginning
    sub($(this.G_FIRST_OFFSET), irToAsm.config.scratch).
    cmp(obj, irToAsm.config.scratch).           
    jl(end).

    add($(this.G_FIRST_OFFSET), irToAsm.config.scratch).       // Address of current item
    cmp(key, mem(this.G_KEY_OFFSET, irToAsm.config.scratch), this.G_KEY_WIDTH).   // global[index] === key ?
    je(cont).                         // Item found on equal!

    sub($(this.G_ENTRY_LENGTH), irToAsm.config.scratch).      // move to next value
    jmp(loop).

    label(end).
    mov(irToAsm.config.NULL, irToAsm.config.scratch).        // no value found

    label(cont);

    if (irToAsm.config.scratch !== dest)
    {
        this.asm.mov(irToAsm.config.scratch, dest);
    }

};

irToAsm.translator.prototype.ir_put_prop_val = function (opnds, instr)
{
    const obj = opnds[0];
    const key = opnds[1];
    const value = opnds[2];
    const targets = instr.targets; 
    const continue_label = this.label(targets[0]);

    var loop = this.asm.labelObj();
    var found = this.asm.labelObj();

    this.get_prop_addr(opnds, irToAsm.config.scratch);

    this.asm.
    cmp(irToAsm.config.NULL, irToAsm.config.scratch).
    jne(found).
    mov(obj, irToAsm.config.scratch).
    add($(this.G_FIRST_OFFSET), irToAsm.config.scratch).          // Retrieve address of first element
    add(mem(this.G_NEXT_OFFSET, obj), irToAsm.config.scratch). // Retrieve address of next element 
    // Inc entry nb
    add($(this.G_ENTRY_LENGTH), mem(this.G_NEXT_OFFSET, obj), this.G_NEXT_OFFSET_WIDTH). 
    mov(key, mem(this.G_KEY_OFFSET, irToAsm.config.scratch), this.G_KEY_WIDTH).     // Add entry key
    label(found).                          
    mov(value, mem(this.G_VALUE_OFFSET, irToAsm.config.scratch), this.G_VALUE_WIDTH). // Add/Update the entry value
    jmp(continue_label);

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

irToAsm.translator.prototype.ir_ret = function (opnds, instr)
{
    const dest = instr.regAlloc.dest;
    const spillNb = this.fct.regAlloc.spillNb;
    const refByteLength = irToAsm.config.stack.width() / 8;
    const retValReg = irToAsm.config.retValReg;

    this.asm.add($(spillNb*refByteLength), irToAsm.config.stack);

    if (opnds[0] !== retValReg)
    {
        this.asm.mov(opnds[0], retValReg);
    }
   
    //this.asm.mov(mem(0,irToAsm.config.stack), EBX);
    //this.asm.jmp(EBX);
    this.asm.ret();
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
    // Add stack frame descriptor space
    // TODO

    // Add return address
    /*
    mov(EAX, irToAsm.config.scratch);

    this.call_self(15).

    this.asm.
    mov(EAX, mem(-(offset), irToAsm.config.stack)).
    mov(irToAsm.config.scratch, EAX).
    */

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
    
    this.call_self(9);
    
    // Reserve space for the global object associated
    // with this function
    if (this.asm.is64bitMode())
    {
        this.asm.gen64(0);
    } else
    {
        this.asm.gen32(0);
    }

    this.asm.genListing("FUNC GLOBAL OBJ");


};

irToAsm.translator.prototype.func_init = function ()
{
    const byteLength = irToAsm.config.stack.width() / 8;
    var spillNb = this.fct.regAlloc.spillNb;

    this.asm.sub($(spillNb*byteLength), irToAsm.config.stack);
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
    const globalObjReg = irToAsm.config.argsReg[1];
    const contextObjReg = irToAsm.config.context;

    const ret = this.asm.labelObj("MAIN RET");
    const fakeInstr1 = {regAlloc:{dest:retValReg}};

    const fakeBlock = {irToAsm:{label:ret}};
    const fakeInstr2 = {regAlloc:{dest:retValReg}, targets:[fakeBlock]};

    assert(globalObjReg !== retValReg, 
           "Invalid register permutation for argsIndex");


    var i;

    this.label(mainFct, "<func MAIN>");

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
    call(this.globalLabel).

    // We need to preserve the global object for the ir_call 
    sub($(this.REG_BYTE_WIDTH), stack).
    mov(retValReg, mem(0,stack)).
    mov(retValReg, globalObjReg);

    // Setup the main function
    this.ir_make_clos([mainFct, globalObjReg], fakeInstr1);

    // Retrieve the global object and restore stack to its original pos
    this.asm.
    mov(mem(0, stack), globalObjReg).
    add($(this.REG_BYTE_WIDTH), stack);

    // Initialise the context object with the global object
    // in the first slot
    this.asm.
    call(this.contextLabel).
    mov(retValReg, contextObjReg). 
    mov(globalObjReg, mem(0, contextObjReg));

    // Call the main function
    this.ir_call([retValReg, globalObjReg], fakeInstr2);

    // Return from the main function
    this.asm.
    label(ret);

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

};


})(); // end of local namespace
