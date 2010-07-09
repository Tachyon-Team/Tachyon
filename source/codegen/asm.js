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
    this.startPos  = startPos  || 0;
    this.bigEndian = bigEndian || true;
    this.listing   = listing   || false;

    this.code = [];
    this.pos  = 0;
};


(function () { // local namespace

// Alias
const asm = asm_CodeBlock.prototype;

// helper functions
function error (msg)
{
    var err = message;
    for (var i=1; i<arguments.length; i++)
    {
        err += arguments[i];
    }
    throw err;
};

function assert (bool, message)
{
    if (!bool) 
    { 
        error(message, 
                              Array.prototype.slice.call(arguments, 2)); 
    } 
};

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

asm.type = {}
asm.type.LBL = 0;
asm.type.DEF = 1;
asm.type.LST = 2;

asm.label   = function (id, pos)
{
    var that = Object.create(asm.label.prototype);
    if (id)  { that.id  = id; };
    if (pos) { that._pos = pos;};

    return that;
};
asm.label.prototype.type = asm.type.LBL; 
asm.label.prototype.id   = "default";
asm.label.prototype._pos = null;
asm.label.name          = function ()
{
    if (typeof this.id === "string")
    {
        return this.id;
    } else if (typeof this.id === "number")
    {
        return "_" + this.id.toString();
    } else
    {
        error("label.name: this type of label id is not supported");
    } 
};

// TODO: Refactor to use getter and setter once the compiler support them
asm.label.prototype.getPos = function ()
{
    if (this._pos) { return this._pos; }
    else { error("label.pos: undefined label", this.id); };
}; 

asm.label.prototype.setPos = function (p)
{
    this._pos = p;
};

asm.genLabel = function (label)
{
    if (label._pos !== null) 
    { 
        error("genLabel: label multiply defined", label.id); 
    } else
    {
        label.setPos(0);
        this.extend(label);
    }
    return this;
};

asm.listing = function (text)
{
    var that = Object.create(asm.listing.prototype);
    if (text) { that.text = text; };
    return that;
};
asm.listing.prototype.type = asm.type.LST;
asm.listing.prototype.text = "";

asm.genListing = function (text)
{
    this.extend(this.listing(text));
    return this;
};

// checks is an array of check procedures in increasing order of generality
// prods  is an array of production procedures in increasing order of generality
asm.deferred = function (checks, prods)
{
    assert(checks.length === prods.length,
           "genDeferred: The number of checks procedure should be equal" +
           " to the number of prods procedure");
    var that = Object.create(asm.deferred.prototype);
    if (checks) { that.checks = checks.reverse(); };
    if (prods)  { that.prods  = prods.reverse();  };
    
    that.current = 0;
    that.size    = 0; 
    return that;
};

asm.deferred.prototype.type   = asm.type.DEF;
asm.deferred.prototype.checks = [function () { return 0; }];
asm.deferred.prototype.prods  = [function () {}];
asm.deferred.prototype.size   = 0;
// TODO: refactor to a property when getter setters are supported
asm.deferred.prototype.length = function ()
{
    return this.checks.length;
};

asm.genDeferred = function (checks, prods)
{
    this.extend(this.deferred(checks, prods));
    return this;     
};

/*
    Add enough "fill" bytes to reach
    the next address at "offset" from "multiple".
    Formally, force alignment to the next address congruent
    to "offset" modulo "multiple". 

    For example:
    -multiple: 5
    -pos: 3
    -offset: 1
    -fill: 0

      | ---- Multiple ----| 
    +---+---+---+---+---+---+---+
    | x | x | x |   | y | y | y |
    +---+---+---+---+---+---+---+
                  ^           ^
                  |           |           
                  pos         offset

    Should give:

      | ---- Multiple ----|
    +---+---+---+---+---+---+---+---+
    | x | x | x | 0 | 0 | 0 | y | y |
    +---+---+---+---+---+---+---+---+

*/
asm.align = function (multiple, offset, fill)
{
    var checks, prods;

    // Default values
    if (!offset) { offset = 0; };
    if (!fill)   { fill   = 0; };

    // TODO: Move to a library
    // Returns a positive reminder
    // no matter the sign of the operands
    function mod(dividend, divisor)
    {
        const r = dividend % divisor;
        return (r<0) ? divisor+r : r;
    };

    function nb_bytes (cb, pos) { return mod((-pos + offset), multiple); };

    function add_bytes (cb, pos)
    {
        for (var i=0; i < nb_bytes(pos); ++i)
        {
           cb.gen8(fill); 
        }
    };
    checks = [nb_bytes];
    prods  = [add_bytes]; 

    return this.genDeferred(checks,prods);
};


// Add enough zero bytes to the code block to align to
// to the given address
asm.origin = function (address, fill)
{
    if (!fill)       { fill = 0; };

    function nb_bytes (cb, pos) { return address - pos; };
    function add_bytes (cb, pos)
    {
        const len = nb_bytes(pos); 
        if (len < 0) 
        { 
            error("asm.origin: address '" + address +
                                  "' should be greater than position '" +
                                  pos + "'");
        }

        for (var i=0; i<len; ++i)
        {
            cb.gen8(fill);
        }
    };
    return this;
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


// (asm-assemble cb) assembles the code block.  After assembly, the
// label objects will be set to their final position and the
// alignment bytes and the deferred code will have been produced.  It
// is possible to extend the code block after assembly.  However, if
// any of the procedures "asm-label", "asm-align", and
// "asm-at-assembly" are called, the code block will have to be
// assembled once masas
asm.assemble2 = function ()
{
    var fixupList  = [];
    var span       = 0;
    var pos        = this.startPos;
    var hasChanged = false;
    var oldSize    = 0;
    var newSize    = 0;
    var oldCode    = this.code;
    
    // Create the fixup list and generate an initial position
    // assignment for labels    
    for (var i=0, curr=this.code[i]; i<this.code.length; ++i, curr=this.code[i])
    {
        if (typeof curr === "number") { span++; pos++; continue; };

        switch(curr.type)
        {
            case asm.type.LBL:
                curr.setPos(pos);
                fixupList.push([span, curr]); 
                span = 0;
                break;
            case asm.type.DEF:
                fixupList.push([span, curr]);
                span = 0;
                break;
            default:
                break;
        }
    }

    // Fix-point to determine labels and deferred code size
    while(hasChanged)
    {
        hasChanged = false;

        // Determine size of deferred code given current label positions
        pos = this.startPos;
        for(var i=0, span=fixupList[i][0], curr=fixupList[i][1]; 
            i<fixupList.length; 
            ++i, span=fixupList[i][0], curr=fixupList[i][1], pos += span)
        {
            if (curr.type !== asm.type.DEF) { continue; }

            oldSize = curr.size;
            // Try every check procedure until finding one that returns
            // a valid size
            for(var check=curr.checks[curr.current], newSize = check(this,pos);
                newSize === null && curr.current < curr.length();
                curr.current = curr.current + 1, newSize = check(this,pos)) {};

            if (curr.current === curr.length()) 
            { 
                error("asm.assemble: every check procedure tested without" +
                      " finding a valid one"); 
            }

            // Update deferred object
            if (oldSize !== newSize)
            {
                pos = pos + oldSize;
                curr.size = newSize;
                hasChanged = true;
            }
           
            // In every case, advance position according to old size 
            // TODO: WHY?
            pos = pos + oldSize;
        }

        // Determine label positions given new size of deferred code
        pos = this.startPos;
        for(var i=0, span=fixupList[i][0], curr=fixupList[i][1]; 
            i<fixupList.length; 
            ++i, span=fixupList[i][0], curr=fixupList[i][1], pos += span)
        {
            switch(curr.type)
            {
                case asm.type.LBL:
                    if (curr.getPos() !== pos) {curr.setPos(pos); hasChanged = true;};
                    break;
                case asm.type.DEF:
                    pos = pos + curr.size;
                    break; 
                default:
                    break;
            } 
        }
    }

    // Generate deferred code
    this.code = [];
    pos=this.startPos;
    for (var i=0, curr=this.code[i]; i < oldCode.length; ++i, curr=this.code[i])
    {
       if (typeof curr === "number") { this.extend(curr); }; 

        switch(curr.type)
        {
            case asm.type.LBL:
                if (curr.pos !== pos) 
                {
                    error("asm.assemble: inconsistency detected");
                };
                break;
            case asm.type.DEF:
                // TODO: Check if we really need a temporary array
                curr.prods[curr.current](this, pos);
                pos = pos + curr.size;
                break; 
            default:
                break;
        } 
    }
    return pos;
}


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
