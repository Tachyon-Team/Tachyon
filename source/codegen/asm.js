/**
@fileOverview
Generic assembler.

The following code was inspired by the Gambit code 
generator written in Scheme.

@copyright
Copyright (c) 2010 Tachyon Javascript Engine, All Rights Reserved
*/

function asm_CodeBlock(startPos, bigEndian, listing)
{
    this.startPos  = startPos;
    this.bigEndian = bigEndian;
    this.listing   = listing;

    this.code = [];
    this.pos  = 0;
}

(function () { // local namespace

// Alias
const asm = asm_CodeBlock.prototype;

asm.extend = function (x)
{
    this.code.push(x);
};


asm.gen8 = function (n)
{
    this.extend(n & 0xff);
};


asm.gen16 = function (n)
{
    if (this.bigEndian)
        this.gen16BE(n);
    else
        this.gen16LE(n);
};


asm.gen16BE = function (n)
{
    this.gen8(n >> 8);
    this.gen8(n);
};


asm.gen16LE = function (n)
{
    this.gen8(n);
    this.gen8(n >> 8);
};


asm.gen32 = function (n)
{
    if (this.bigEndian)
        this.gen32BE(n);
    else
        this.gen32LE(n);
};


asm.gen32BE = function (n)
{
    this.gen16(n >> 16);
    this.gen16(n);
};


asm.gen32LE = function (n)
{
    this.gen16(n);
    this.gen16(n >> 16);
};

asm.gen64 = function (n)
{
    // TODO: in JS n is a double, so only 52 bits are significant.
    if (this.bigEndian)
        this.gen64BE(n);
    else
        this.gen64LE(n);
};


asm.gen64BE = function (n)
{
    // TODO: in JS n is a double, so only 52 bits are significant.
    this.gen32(n >> 32);
    this.gen32(n);
};


asm.gen64LE = function (n)
{
    // TODO: in JS n is a double, so only 52 bits are significant.
    this.gen32(n);
    this.gen32(n >> 32);
};


asm.genListing = function(text)
{
    this.extend(text);
};


asm.assemble = function ()
{
    var c = this.code;
    this.pos = 0;
    this.code = [];
    this.flatten(c);

    // TODO: actually assemble the code!

    return this.pos; // return length of code in bytes
};


asm.flatten = function (x)
{
    if (x instanceof Array)
    {
        for (var i=0; i<x.length; i++)
            this.flatten(x[i]);
    }
    else
    {
        this.extend(x);
        if (typeof x == "number")
            this.pos++;
    }
};


asm.assembleToMachineCodeBlock = function ()
{
    var len = this.assemble();
    var block = allocMachineCodeBlock(len);
    var pos = 0;

    for (var i=0; i<this.code.length; i++)
    {
        var x = this.code[i];
        if (typeof x == "number")
            block[pos++] = x;
    }

    return block;
};

})(); // end of local namespace
