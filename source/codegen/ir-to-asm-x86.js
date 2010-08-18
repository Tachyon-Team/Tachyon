/**
@fileOverview

Translate the low-level IR to machine dependent assembly code.

@copyright
Copyright (c) 2010 Tachyon Javascript Engine, All Rights Reserved
*/

var irToAsm = irToAsm || {};

// Constant values 
// TODO: Determine 32 bits values
irToAsm["TRUE"] = 0;
irToAsm["FALSE"] = 1;
irToAsm["UNDEFINED"] = 2;
irToAsm["NULL"] = 3;

irToAsm.translator = function (asm)
{
    var that = Object.create(irToAsm.translator.prototype);
    that.asm = asm;
    that.labels = {};
    that.strings = {};
    that.stringNb = 0;

    // TODO: Decide on a scratch register 
    that.scratch = null;

    return that;
};
/** @private assembler object */
irToAsm.translator.prototype.asm = null;
/** @private known labels so far */
irToAsm.translator.prototype.labels = {};
/** @private known strings so far */
irToAsm.translator.prototype.strings = {};
irToAsm.translator.prototype.stringNb = 0;
/** @private scratch register */
irToAsm.translator.prototype.scratch = null;
/** generate the corresponding assembly code for this list of blocks */
irToAsm.translator.prototype.generate = function (blockList)
{
    var block;
    var instrIt;

    for (i=0; i < order.length; ++i)
    {
        block = order[i]; 
        this.label(block.label);

        for (var instrIt = block.getInstrItr(); 
             instrIt.valid(); 
             instrIt.next())
        {
            instr = instrIt.get();
            // TODO: Replace constants in regAlloc.opnds by
            // immediate values
            this[instr.mnemonic](instr);
        }
    }
};
/** 
    @private
    Add a label to the assembly code. If a previous label was previously used,
    it will be reused.
*/
irToAsm.translator.prototype.label = function (name)
{
    var l = this.labels[name];
    if (l === undefined)
    {
        l = this.asm.labelObj(name);
        this.labels[name] = l;
    }

    this.asm.label(l);
}

/** @private */
irToAsm.translator.prototype.lt = function (instr)
{
    var opnds = instr.regAlloc.opnds;
    var dest = instr.regAlloc.dest; 
    var cont = this.asm.labelObj();

    assert(opnds.length === 2);
    assert(dest !== null);

    if (opnds[0].type === x86.type.MEM &&
        opnds[1].type === x86.type.MEM)
    {
        this.asm.
        mov(opnds[0], dest). 
        cmp(dest, opnds[1]); // TODO: Check the order of the args for cmp
    } else
    {
        this.asm.
        cmp(opnds[0], opnds[1]); // TODO: Check the order of the args for cmp
    }

    this.asm.
    mov(irToAsm["TRUE"], dest).
    jl(cont).
    mov(irToAsm["FALSE"], dest).
    label(cont);
};

/** @private */
irToAsm.translator.prototype["if"] = function (instr)
{
    var cond = instr.regAlloc.opnds[0];
    var trueLabel = this.label(instr.targets[0].label);
    var falseLabel = this.label(instr.targets[1].label);

    assert(opnds.length === 1);

    this.asm.
    cmp(cond, irToAsm["TRUE"]).
    je(trueLabel).
    jmp(falseLable);
};

/** @private */
irToAsm.translator.prototype.sub = function (instr)
{
    var opnds = instr.regAlloc.opnds;
    var dest = instr.regAlloc.dest; 

    assert(opnds.length === 2);
    assert(dest !== null);

    this.asm.
    mov(opnds[0], dest).
    sub(opnds[1], dest);
};

/** @private */
irToAsm.translator.prototype.add = function (instr)
{
    var opnds = instr.regAlloc.opnds;
    var dest = instr.regAlloc.dest; 

    assert(opnds.length === 2);
    assert(dest !== null);

    this.asm.
    mov(opnds[0], dest).
    add(opnds[1], dest);
};

/** @private */
irToAsm.translator.prototype.get_prop_val = function (instr)
{
    var opnds = instr.regAlloc.opnds;
    var obj = opnds[0];
    var key = opnds[1];
    var dest = instr.regAlloc.dest; 

    var loop = this.asm.labelObj();
    var notFound = this.asm.labelObj();
    var cont = this.asm.labelObj();

    var global = 0; // TODO: Determine a register for global

    const $ = this.asm.immediateValue;
    const mem = this.asm.memory;

    assert(opnds.length === 2);
    assert(dest !== null);

    // TODO: Handle global property correctly
    assert(obj === "global");

    // Linear search through global object properties
    // assuming global is array-like
    /* Pseudo-code
    for (var i=0; i < global.length; i=i+2)
    {
        if (global[i] === key)
        {
            return global[i+1];
        }
    }
    return null;
    */
    this.asm.
    mov($(0), dest).
    label(loop).
    mov(mem(1, global), this.scratch). // Retrieve address of first element
    add(dest, scratch).                // Add the current index 
    cmp(mem(scratch), key).            // global[index] === key ?
    jne(notFound).
    mov(mem(1, scratch), dest).        // return the current value
    jmp(cont).
    label(notFound).
    add($(2), dest).                   // move to next value
    cmp(mem(global), dest).            // while there is values left
    jl(loop).
    mov(irToAsm["NULL"], dest).        // no value found
    label(cont);
};
