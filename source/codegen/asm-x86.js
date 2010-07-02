/**
@fileOverview
Assembler for x86 machine code. Translates assembly code written
in a AT&T inspired syntax to binary code ready for execution.

The following code was inspired by the Gambit code 
generator written in Scheme.

@copyright
Copyright (c) 2010 Tachyon Javascript Engine, All Rights Reserved
*/

function x86_Assembler()
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

