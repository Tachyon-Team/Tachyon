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
    this.startPos     = startPos  || 0;
    this.bigEndian    = bigEndian || true;
    this.useListing   = listing   || false;

    this.code = [];
    this.pos  = 0;
};


(function () { // local namespace

// Alias
const asm = asm_CodeBlock.prototype;

// helper functions
function error (msg)
{
    var err = msg;
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
        error(message, Array.prototype.slice.call(arguments, 2)); 
    } 
};

asm.extend = function (x)
{
    this.code.push(x);
};


asm.gen8 = function (n)
{
    this.extend(n & 0xff);
    return this;
};


asm.gen16 = function (n)
{
    if (this.bigEndian)
        this.gen16BE(n);
    else
        this.gen16LE(n);
    return this;
};


asm.gen16BE = function (n)
{
    this.gen8(n >> 8);
    this.gen8(n);
    return this;
};


asm.gen16LE = function (n)
{
    this.gen8(n);
    this.gen8(n >> 8);
    return this;
};


asm.gen32 = function (n)
{
    if (this.bigEndian)
        this.gen32BE(n);
    else
        this.gen32LE(n);
    return this;
};


asm.gen32BE = function (n)
{
    this.gen16(n >> 16);
    this.gen16(n);
    return this;
};


asm.gen32LE = function (n)
{
    this.gen16(n);
    this.gen16(n >> 16);
    return this;
};

asm.gen64 = function (n)
{
    // TODO: in JS n is a double, so only 52 bits are significant.
    if (this.bigEndian)
        this.gen64BE(n);
    else
        this.gen64LE(n);
    return this;
};


asm.gen64BE = function (n)
{
    // TODO: in JS n is a double, so only 52 bits are significant.
    this.gen32(n >> 32);
    this.gen32(n);
    return this;
};


asm.gen64LE = function (n)
{
    // TODO: in JS n is a double, so only 52 bits are significant.
    this.gen32(n);
    this.gen32(n >> 32);
    return this;
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
asm.label.prototype.name = function ()
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
    if (this._pos !== null) { return this._pos; }
    else { error("label.pos: undefined label", this.id); };
}; 

asm.label.prototype.setPos = function (p)
{
    if (typeof p !== "number") { error("label.setPos: Invalid position"); };
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


// Produces a string representing the listing of the code block.
// When printed, the string should output with this schema:
//
// |- position -| |- hex code -|- optional listing -|
//   fixed width   fixed width    variable width
//
// Each instruction will be printed on one or two lines depending on the
// number of bytes needed to encode the instruction
//
// Precondition: Code Block must have been assembled
asm.listingString = function ()
{
    // Constants controlling the output layout
    const textCol   = 32;
    const posWidth  =  6;
    const posRadix  = 16;
    const byteWidth =  3;

    function printDigit (d) { return "0123456789abcdef"[d]; };
    function printByte  (b) 
    { 
        return printDigit(b >> 4) + printDigit(b%16) + " "; 
    };

    function printPos (p)
    {
        var s = new Array(posWidth);

        for (var i=posWidth-1; i>=0; i--)
        {
            s[i] = printDigit(p % posRadix);
            p = Math.floor(p / posRadix);
        }

        return s.join("");
    };

    function spaces (n) 
    {
        return new Array(n+1).join(" ");
    };


    // TODO: Check if we should have a different behavior on windows
    function newline () { return "\n"; }

    // Let's approximate the overhead for the additional storage
    // required to be 25 % ( position priting and new line chars)
    // Preallocate a buffered string for printing
    var s = new Array(Math.floor(this.code.length*1.25));
    var index = 0; // index to the buffered string

    // Variables for controlling the output printing
    var pos = this.startPos;
    var col = 0;
    
    for (var i=0; i < this.code.length; i++)
    {
        if (typeof this.code[i] === "number")
        {
            // The previous line was full, print the new position
            if (col === 0 || col >= (textCol - byteWidth))
            {
                if (col !== 0) { s[index++] = newline(); };
                s[index++] = printPos(pos) + " ";
                col = posWidth + 1;
            }
           
            // Print the next byte 
            s[index++] = printByte(this.code[i]);
            pos++;
            col = col + byteWidth;

        } else if (this.code[i].type === this.type.LST)
        {

            // Print the position again if we are at the beginning
            // of a line
            if (col === 0) 
            { 
                s[index++] = printPos(pos); 
                col = posWidth;
            }         

            // Fill with empty spaces the rest of the hex code
            // block and print the listing with a space separating
            // the hex code and the listing
            s[index++] = spaces(textCol - col - 1) + 
                         this.code[i].text + newline();
            col = 0;
        } else 
        {
            // TODO: Should print something to indicate which
            //       element were not assembled, for now
            //       we will just ignore them
        }
    }
    
    if (col > 0) { s[index++] = newline(); } 
    return s.join("");
};

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
    if (checks) { that.checks = checks; };
    if (prods)  { that.prods  = prods;  };
    
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
        for (var i=0; i < nb_bytes(cb, pos); ++i)
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

// assembles the code block.  After assembly, the
// label objects will be set to their final position and the
// alignment bytes and the deferred code will have been produced.  It
// is possible to extend the code block after assembly.  However, if
// any of the procedures "asm-label", "asm-align", and
// "asm-at-assembly" are called, the code block will have to be
// assembled once masas
asm.assemble = function ()
{
    var fixupList  = [];
    var span       = 0;
    var pos        = this.startPos;
    var hasChanged = true;
    var oldSize    = 0;
    var newSize    = 0;
    var oldCode    = this.code;
    var curr;
    var check;
    
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
        for(var i=0; i<fixupList.length; ++i)
        { 
            span=fixupList[i][0];
            curr=fixupList[i][1];
            pos += span;

            if (curr.type !== asm.type.DEF) { continue; }

            oldSize = curr.size;
            newSize = null;
            // Try every check procedure until finding one that returns
            // a valid size
            while (newSize === null && curr.current < curr.length())
            {
                check   = curr.checks[curr.current];
                newSize = check(this, pos);
            }

            if (newSize === null) 
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
        for(var i=0; i<fixupList.length; ++i)
        { 
            span=fixupList[i][0];
            curr=fixupList[i][1];
            pos += span;

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
    for (var i=0, curr=oldCode[i]; i < oldCode.length; ++i, curr=oldCode[i])
    {
        if (typeof curr === "number") 
        { 
            this.extend(curr); 
            pos = pos + 1;
        }
        else 
        {

            switch(curr.type)
            {
                case asm.type.LBL:
                    if (curr.getPos() !== pos) 
                    {
                        error("asm.assemble: inconsistency detected");
                    };
                    break;
                case asm.type.DEF:
                    curr.prods[curr.current](this, pos);
                    pos = pos + curr.size;
                    break; 
                default:
                    // Leave other objects in place
                    this.extend(curr);
                    break;
            } 
        };
    }
    return pos;
}

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
