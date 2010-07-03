/**
@fileOverview
Assembler for x86 machine code. Translates assembly code written
in a AT&T inspired syntax to binary code ready for execution.

The following code was inspired by the Gambit code 
generator written in Scheme.

@copyright
Copyright (c) 2010 Tachyon Javascript Engine, All Rights Reserved
*/

// TODO: Why can't we put those constructors on a global object named x86?
function x86_Assembler ()
{
    this.listing   = true;
    this.codeBlock = new asm_CodeBlock(true, 0, this.listing);
}

(function () { // local namespace

// Alias
const x86 = x86_Assembler.prototype;

x86.gen8  = function (n) { this.codeBlock.gen8(n);  return this;};
x86.gen16 = function (n) { this.codeBlock.gen16(n); return this;};
x86.gen32 = function (n) { this.codeBlock.gen32(n); return this;};
x86.gen64 = function (n) { this.codeBlock.gen64(n); return this;};

x86.genImmNum = function (k, width)
{
    // TODO: Find out what behavior should the signed-lo have
    function signed-lo(n, k) 
    {
        return k & (Math.pow(2,n) - 1);
    }

    if ( width === 8) 
    {
        this.gen8(signed-lo(8,k));   
    } else if (width === 16) 
    {
        this.gen16(signed-lo(16,k));
    } else if (width === 32)
    {
        this.gen32(signed-lo(32,k));
    }
    else 
    {
        this.gen64(signed-lo(64,k));
    }   
    return this;
}

// Types to allow testing for object type on x86 related
// objects
x86.type = {};
x86.type.IMM = 0;
x86.type.REG = 1;
x86.type.MEM = 2;
x86.type.GLO = 3;
x86.type.LBL = 4;

// Immediate object to represent immediate value
x86.immediate = function (value)
{
    // Enforce that modifications of the that object
    // won't screw up the prototype by creating a
    // new object for each instance whether they
    // have non-default properties or not
    var that = Object.create(x86.immediate.prototype);

    // Minimize memory usage by storing default values 
    // for properties only on the prototype and avoid
    // duplicating default values in each instance    
    if (value)   { that.value = value; }

    return that;
}
x86.immediate.prototype.value = 0;
x86.immediate.prototype.type  = x86.type.IMM;

// Memory object operand representing an access 
// to memory through a Displacement, Scale, Index and Base
x86.memory = function ( disp, base, index, scale )
{
    var that = Object.create(x86.memory.prototype);

    if (disp)   { that.disp   = disp; }
    if (base)   { that.base   = base;   }
    if (index)  { that.index  = index;  }
    if (scale)  { that.scale  = scale;  }
    
    return that;
}

// Let's use explicit null values to enforce
// presence of the properties while 
// being able to detect that these are defaults
x86.memory.prototype.base   = null;
x86.memory.prototype.index  = null;
// Idem for 0
x86.memory.prototype.disp   = 0;
x86.memory.prototype.scale  = 0;
x86.memory.prototype.type   = x86.type.MEM;



//-----------------------------------------------------------------------------

// x86 instruction encoding.
x86.ret   = function ()  { return this.noOpndInstr(0xc3, "ret");};
x86.cmc   = function ()  { return this.noOpndInstr(0xf5, "cmc");};
x86.clc   = function ()  { return this.noOpndInstr(0xf8, "clc");};
x86.stc   = function ()  { return this.noOpndInstr(0xf9, "stc");};
x86.cli   = function ()  { return this.noOpndInstr(0xfa, "cli");};
x86.sti   = function ()  { return this.noOpndInstr(0xfb, "sti");};
x86.cld   = function ()  { return this.noOpndInstr(0xfc, "cld");};
x86.std   = function ()  { return this.noOpndInstr(0xfd, "std");};


x86.noOpndInstr = function (opcode, mnemonic)
{
    this.gen8(opcode);
    if (this.listing)
        this.genListing(this.instrFormat(mnemonic, ""));
    return this;
};


x86.genListing = function(text) { this.codeBlock.genListing(text); };
x86.instrFormat = function(mnemonic, suffix) 
{ 
    return "    " + mnemonic + suffix; 
};


x86.opndSizeOverridePrefix = function (width) 
{
    if (width === 16) { this.gen8(0x66); }; return this;
}
x86.op    = function (op, mnemonic, src, dest, width) {return this;}
x86.opImm = function (op, mnemonic, k,   dest, width) 
{
    const that = this;

    function listing (width,n)
    {
        //TODO
    }

    function accumulator (width)
    {
        that.
        opndSizeOverridePrefix(width).
        gen8( ((width === 8) ? 0x04 : 0x05) + (op << 3) );
        // TODO: listing(width)
    }

    function general (width)
    {

    }

    return that;
};

})(); // end of local namespace

