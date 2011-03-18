/**
@fileOverview
Assembler for x86 machine code. Translates assembly code written
in a AT&T inspired syntax to binary code ready for execution.

The following code is based on the assembler of the Gambit x86 back-end.

@copyright
Copyright (c) 2010 Tachyon Javascript Engine, All Rights Reserved
*/

/** @namespace x86 code generator */
var x86 = {};

/** @namespace potential x86 targets */
x86.target = {};

/** x86 32 bit target */
x86.target.x86    = 0;

/** x86 64 bit target */
x86.target.x86_64 = 1;

/** @namespace */
x86.type = {};
/** Generic x86 object */
x86.type.OBJ = "X86_OBJ";
/** Immediate value */
x86.type.IMM_VAL = "X86_IMMEDIATE_VALUE";
/** Link object */
x86.type.LINK = "X86_LINK_OBJECT";
/** Register */
x86.type.REG = "X86_REGISTER";
/** Memory access */
x86.type.MEM = "X86_MEMORY";

/** @private test if num is a valid 8 bit signed value */
x86.isSigned8 = function (num) { return (num >= -128 && num <= 127); };

/** @private test if num is a valid 32 bit signed value */
x86.isSigned32 = function (num)
{
    return num_ge(num, getIntMin(32)) && num_le(num, getIntMax(32));
};

/**
    Returns a new assembler object.

    @class Assembler for x86 code
    @param {x86.target} target optional x86.target value,
                        defaults to x86.target.x86
*/
x86.Assembler = function (target)
{
    if (target === undefined)
        target = x86.target.x86;

    /** Flag for listing output */
    this.useListing = true;

    /** Generic underlying assembler object  */
    this.codeBlock  = new asm.CodeBlock(0, false, this.useListing);

    /** @private Current target for compilation,
        must not be modified once the object is constructed */
    // TODO: refactor to have a setter throw an exception once
    // getter and setter are supported
    this.target     = target;
};

x86.Assembler.prototype.isImmediate = function (obj)
{
    return obj.type === x86.type.IMM_VAL || obj.type === x86.type.LINK;
};

x86.Assembler.prototype.is32bitImm = function (obj)
{
    if(!this.isImmediate(obj))
    {
        return false;
    }

    if (obj.type === x86.type.IMM_VAL)
    {
        return x86.isSigned32(obj.value);
    } else if (obj.type === x86.type.LINK)
    {
        return obj.width() <= 32;
    }
};

/** Returns whether the current compilation target is x86_64 */
x86.Assembler.prototype.is64bitMode = function ()
{
    return this.target === x86.target.x86_64;
};

/** @private */
x86.Assembler.prototype.assert64bitMode = function ()
{
    assert(this.is64bitMode(),
               "instruction only valid for x86-64");
};

/** @private */
x86.Assembler.prototype.assert32bitMode = function ()
{
    assert(!this.is64bitMode(), "instruction only valid for x86");
};

/**
    Returns a new x86 generic object. Note: the lower case constructor
    means new is not necessary to create an object of this class

    @class x86 object
*/
x86.Assembler.obj = function ()
{
    return Object.create(x86.Assembler.obj.prototype);
};
/** x86 object type */
x86.Assembler.obj.prototype.type = x86.type.OBJ;

/** Returns a string representation containing the type and
    properties of the x86 object */
x86.Assembler.obj.prototype.toString = function ()
{
    var s = [];
    for (var p in this)
    {
        if (typeof this[p] !== "function")
        {
            s.push( ((this.hasOwnProperty(p)) ? "" : "*") + // Mark parent prop
                    p + ":" + String(this[p]));
        }
    }

    return this.type + "(" + s.join(", ") + ")";
};

/** Adds a byte to the code stream. Can be chained. */
x86.Assembler.prototype.gen8  = function (n)
{
    this.codeBlock.gen8(n);  return this;
};
/** Adds two bytes to the code stream. Can be chained.  */
x86.Assembler.prototype.gen16 = function (n)
{
    this.codeBlock.gen16(n); return this;
};
/** Adds three bytes to the code stream. Can be chained.  */
x86.Assembler.prototype.gen32 = function (n)
{
    this.codeBlock.gen32(n); return this;
};
/** Adds four bytes to the code stream. Can be chained.  */
x86.Assembler.prototype.gen64 = function (n)
{
    this.codeBlock.gen64(n); return this;
};

/** Returns the signed value of k coerced to a width of 8 */
x86.Assembler.prototype.signedLo8 = function (k)
{
    return ((k + 0x80) & 0xff) - 0x80;
};

/** Returns the signed value of k coerced to a width of 16 */
x86.Assembler.prototype.signedLo16 = function (k)
{
    return ((k + 0x8000) & 0xffff) - 0x8000;
};

/** Returns the signed value of k coerced to a width of 32 */
x86.Assembler.prototype.signedLo32 = function (k)
{
    return num_and(k, getIntMax(32, true));     
};

/** Returns the signed value of k coerced to a width of 64 */
x86.Assembler.prototype.signedLo64 = function (k)
{
    return num_and(k, getIntMax(64, true));
};

/** @private
    The value returned is the signed value 'k' coerced to the given
    width 'n'.

    ((k + 2^(n-1)) & (2^n - 1)) - (2^(n-1))

    For example, with width of 8:

      k         |      coerced value
    [...]
    -130        ->     126
    -129        ->     127
    -128..127   ->     identity
    128         ->     -128
    129         ->     -127
    [...]

    It gives the signed value of the given bit
    pattern generated from extracting the lowest n bits.

 */
x86.Assembler.prototype._genImmNum = function (k, width)
{
    var n;
    if (width === 8)
    {
        n = this.signedLo8(k);
        this.gen8(n);
        return n;
    } else if (width === 16)
    {
        n = this.signedLo16(k);
        this.gen16(n);
        return n;
    } else if (width === 32)
    {
        n = this.signedLo32(k);
        this.gen32(n);
        return n;
    }
    else
    {
        n = this.signedLo64(k);
        this.gen64(n);
        return n;
    }
};

/** Adds an immediate number to the code stream.
    The value returned is the signed value coerced to the given
    width.
    Cannot be chained.
    @param {Number} k
    @param {Number} width Width of the value in number of bits.
                          Maximum of 32. Defaults to 32.
*/
x86.Assembler.prototype.genImmNum = function (k, width)
{
    if (width === undefined)
        width = 32;

    return this._genImmNum(k, Math.min(32, width));
};


/**
    Adds a required object to the code stream for linking later.

    Can be chained.
    @param           linkObj  The object to link to at this position.
*/
x86.Assembler.prototype.require = function (linkObj)
{
    this.codeBlock.genRequired(linkObj);
    return this;
};

/**
    Adds a provided object to the code stream for linking later.

    Can be chained.
    @param linkObj The object to linking from at this position.
*/
x86.Assembler.prototype.provide = function (linkObj)
{
    this.codeBlock.genProvided(linkObj);
    return this;
};

/**
    Returns a new immediate value object. Note: the lower case constructor
    means new is not necessary to create an object of this class

    @class Immediate value object
    @augments x86.Assembler.obj
    @param {Number} value Defaults to 0.
*/
x86.Assembler.prototype.immediateValue = function (value)
{
    if (value === undefined)
        value = 0;

    assert(num_instance(value),
           "'value' argument must be a number");

    var that = Object.create(x86.Assembler.prototype.immediateValue.prototype);

    /** @private */
    that.value = value;

    return that;
};
x86.Assembler.prototype.immediateValue.prototype = x86.Assembler.obj();
/** x86 object type */
x86.Assembler.prototype.immediateValue.prototype.type  = x86.type.IMM_VAL;

x86.Assembler.prototype.immediateValue.prototype.toString = function (verbose)
{
    if (verbose === true && this.__proto__ !== undefined)
    {
        return this.__proto__.toString(true);
    } else
    {
        return String(this.value);
    }
};

/**
    Returns a new memory object. Note: the lower case constructor
    means new is not necessary to create an object of this class.

    @class Represents an access to memory
    @augments x86.Assembler.obj
    @param {Number} disp
    @param {x86.Assembler#register} base
    @param {x86.Assembler#register} index
    @param {Number} scale

*/
x86.Assembler.prototype.memory = function (disp, base, index, scale)
{
    if (disp === undefined)
        disp = 0;

    if (base === undefined)
        base = null;

    if (index === undefined)
        index = null;

    if (scale === undefined)
        scale = 1;

    assert(typeof disp === "number",
               "'disp' argument must be a number");
    assert((base === null)  || base.type === x86.type.REG,
               "'base' argument must be a register");
    assert((index === null) || index.type === x86.type.REG,
               "'index' argument must be a register");
    assert(scale === 1 || scale === 2 || scale === 4 || scale === 8,
               "'scale' argument must be 1,2,4 or 8");

    var that = Object.create(x86.Assembler.prototype.memory.prototype);

    /** @private */
    that.disp  = disp;
    /** @private */
    that.base  = base;
    /** @private */
    that.index = index;
    /** @private */
    that.scale = scale;

    return that;
};
x86.Assembler.prototype.memory.prototype        = x86.Assembler.obj();
x86.Assembler.prototype.memory.prototype.type   = x86.type.MEM;

x86.Assembler.prototype.memory.prototype.toString = function (verbose)
{
    if (verbose === true && this.__proto__ !== undefined)
    {
        return this.__proto__.toString(true);
    } 
    else
    {
        return "mem(" +
               (this.disp !== 0 ? this.disp + "," : "") +
                this.base +
               (this.index !== null ? "," + this.index : "") +
               (this.scale !== 1 ? "," + this.scale : "") + ")";
    }
};

x86.Assembler.prototype.linked = function (name, linkValue, width)
{
    var that = Object.create(x86.Assembler.prototype.linked.prototype);

    assert(typeof(name) === "string", "'name' argument must be a string");
    assert(typeof(linkValue) === "function", "'linkValue' argument must be a function");
    assert(typeof(width) === "number", "'width' argument must be a number");

    that.name = name;
    that.linkValue = linkValue;
    that.width = function () { return width; };

    return that;
};

x86.Assembler.prototype.linked.prototype        = x86.Assembler.obj();
x86.Assembler.prototype.linked.prototype.type   = x86.type.LINK;

x86.Assembler.prototype.linked.prototype.name   = "";
x86.Assembler.prototype.linked.prototype.linkValue  = function () { error("'linkValue' not set"); };
x86.Assembler.prototype.linked.prototype.width = function () { return 32; };
x86.Assembler.prototype.linked.prototype.srcAddr = null;
x86.Assembler.prototype.linked.prototype.setAddr = function (addr) { this.srcAddr = addr;};
x86.Assembler.prototype.linked.prototype.getAddr = function () { return this.srcAddr.copy();};
x86.Assembler.prototype.linked.prototype.clone   = function () { return Object.create(this); };


x86.Assembler.prototype.linked.prototype.toString = function (verbose)
{
    if (verbose === true && this.__proto__ !== undefined)
    {
        return this.__proto__.toString(true);
    } else
    {
        return "<" + this.name + ">";
    }
};

/**
    Returns a new memory object. Note: the lower case constructor
    means new is not necessary to create an object of this class.

    @class Represents a register
    @augments x86.Assembler.obj
    @param {String} name
    @param {Number} value
*/
x86.Assembler.prototype.register = function ( name, value )
{
    assert(name  !== undefined,
               "'name' argument not supplied");
    assert(value !== undefined && typeof value === "number",
               "'value' argument not supplied or wrong type");

    var that = Object.create(x86.Assembler.prototype.register.prototype);
    /** @private */
    that.name  = name;
    /** @private */
    that.value = value;

    x86.Assembler.prototype.register.registers[value] = that;
    return that;
};
x86.Assembler.prototype.register.prototype = x86.Assembler.obj();
x86.Assembler.prototype.register.prototype.type = x86.type.REG;

x86.Assembler.prototype.register.prototype.toString = function (verbose)
{
    if (verbose === true && this.__proto__ !== undefined)
    {
        return this.__proto__.toString(true);
    } else
    {
        return this.name.toUpperCase();
    }
};

/** @private cache of registers */
x86.Assembler.prototype.register.registers = [];

/** returns an 8 bit register */
x86.Assembler.prototype.register.reg8 = function (n)
{
    return this.registers[80 + n];
};
/** returns a 16 bit register */
x86.Assembler.prototype.register.reg16 = function (n)
{
    return this.registers[32 + n];
};
/** returns a 32 bit register */
x86.Assembler.prototype.register.reg32 = function (n)
{
    return this.registers[16 + n];
};
/** returns a 64 bit register */
x86.Assembler.prototype.register.reg64 = function (n)
{
    return this.registers[n];
};
/** returns a floating point register */
x86.Assembler.prototype.register.fpu = function (n)
{
    return this.registers[48 + n];
};

/** tells if a register is an 8 bit register */
x86.Assembler.prototype.register.prototype.isr8  = function ()
{
    return this.value >= 80;
};

/** tells if a register is a high 8 bit register */
x86.Assembler.prototype.register.prototype.isr8h = function ()
{
    return this.value >= 96;
};

/** tells if a register is an xmm register */
x86.Assembler.prototype.register.prototype.isxmm = function ()
{
    return this.value >= 64 && this.value < 80;
};

/** tells if a register is a mm register */
x86.Assembler.prototype.register.prototype.ismm  = function ()
{
    return this.value >= 56 && this.value < 64;
};

/** tells if a register is a fpu register */
x86.Assembler.prototype.register.prototype.isfpu = function ()
{
    return this.value >= 48 && this.value < 56;
};

/** tells if a register is a 16 bit register */
x86.Assembler.prototype.register.prototype.isr16 = function ()
{
    return this.value >= 32 && this.value < 48;
};

/** tells if a register is a 32 bit register */
x86.Assembler.prototype.register.prototype.isr32 = function ()
{
    return this.value >= 16 && this.value < 32;
};

/** tells if a register is a 64 bit register */
x86.Assembler.prototype.register.prototype.isr64 = function ()
{
    return this.value <  16;
};

// TODO: When getter setter are supported, refactor field and width to
// act as properties instead of a method. This will make it more
// similar to the way base, index and disp are accessed on memory object
/** returns the field value of the register */
x86.Assembler.prototype.register.prototype.field = function ()
{
    return this.value & 0xF;
};
/** returns the width value of the register */
x86.Assembler.prototype.register.prototype.width = function ()
{
    if      (this.value < 16) { return 64;}
    else if (this.value < 32) { return 32;}
    else if (this.value < 48) { return 16;}
    else if (this.value < 64) { return 80;}
    else if (this.value < 80) { return 128;}
    else                      { return 8;}
};
/** returns the (sub)-register corresponding to the given width */
x86.Assembler.prototype.register.prototype.subReg = function (width)
{
    var field = this.field();

    const reg = x86.Assembler.prototype.register;

    // Switch on the number of bits
    switch (width)
    {
        case 64:
        return reg.reg64(field);

        case 32:
        return reg.reg32(field);

        case 16:
        return reg.reg16(field);

        case 8:
        return reg.reg8(field);
    }
};

/** Predefined register object */
x86.Assembler.prototype.register.al =
    x86.Assembler.prototype.register("al",    80);
/** Predefined register object */
x86.Assembler.prototype.register.cl =
    x86.Assembler.prototype.register("cl",    81);
/** Predefined register object */
x86.Assembler.prototype.register.dl =
    x86.Assembler.prototype.register("dl",    82);
/** Predefined register object */
x86.Assembler.prototype.register.bl =
    x86.Assembler.prototype.register("bl",    83);
/** Predefined register object */
x86.Assembler.prototype.register.ah =
    x86.Assembler.prototype.register("ah",   100);
/** Predefined register object */
x86.Assembler.prototype.register.ch =
    x86.Assembler.prototype.register("ch",   101);
/** Predefined register object */
x86.Assembler.prototype.register.dh =
    x86.Assembler.prototype.register("dh",   102);
/** Predefined register object */
x86.Assembler.prototype.register.bh =
    x86.Assembler.prototype.register("bh",   103);
/** Predefined register object */
x86.Assembler.prototype.register.spl =
    x86.Assembler.prototype.register("spl",   84);
/** Predefined register object */
x86.Assembler.prototype.register.bpl =
    x86.Assembler.prototype.register("bpl",   85);
/** Predefined register object */
x86.Assembler.prototype.register.sil =
    x86.Assembler.prototype.register("sil",   86);
/** Predefined register object */
x86.Assembler.prototype.register.dil =
    x86.Assembler.prototype.register("dil",   87);
/** Predefined register object */
x86.Assembler.prototype.register.r8b =
    x86.Assembler.prototype.register("r8b",   88);
/** Predefined register object */
x86.Assembler.prototype.register.r9b =
    x86.Assembler.prototype.register("r9b",   89);
/** Predefined register object */
x86.Assembler.prototype.register.r10b =
    x86.Assembler.prototype.register("r10b",  90);
/** Predefined register object */
x86.Assembler.prototype.register.r11b =
    x86.Assembler.prototype.register("r11b",  91);
/** Predefined register object */
x86.Assembler.prototype.register.r12b =
    x86.Assembler.prototype.register("r12b",  92);
/** Predefined register object */
x86.Assembler.prototype.register.r13b =
    x86.Assembler.prototype.register("r13b",  93);
/** Predefined register object */
x86.Assembler.prototype.register.r14b =
    x86.Assembler.prototype.register("r14b",  94);
/** Predefined register object */
x86.Assembler.prototype.register.r15b =
    x86.Assembler.prototype.register("r15b",  95);
/** Predefined register object */
x86.Assembler.prototype.register.ax =
    x86.Assembler.prototype.register("ax",    32);
/** Predefined register object */
x86.Assembler.prototype.register.cx =
    x86.Assembler.prototype.register("cx",    33);
/** Predefined register object */
x86.Assembler.prototype.register.dx =
    x86.Assembler.prototype.register("dx",    34);
/** Predefined register object */
x86.Assembler.prototype.register.bx =
    x86.Assembler.prototype.register("bx",    35);
/** Predefined register object */
x86.Assembler.prototype.register.sp =
    x86.Assembler.prototype.register("sp",    36);
/** Predefined register object */
x86.Assembler.prototype.register.bp =
    x86.Assembler.prototype.register("bp",    37);
/** Predefined register object */
x86.Assembler.prototype.register.si =
    x86.Assembler.prototype.register("si",    38);
/** Predefined register object */
x86.Assembler.prototype.register.di =
    x86.Assembler.prototype.register("di",    39);
/** Predefined register object */
x86.Assembler.prototype.register.r8w =
    x86.Assembler.prototype.register("r8w",   40);
/** Predefined register object */
x86.Assembler.prototype.register.r9w =
    x86.Assembler.prototype.register("r9w",   41);
/** Predefined register object */
x86.Assembler.prototype.register.r10w =
    x86.Assembler.prototype.register("r10w",  42);
/** Predefined register object */
x86.Assembler.prototype.register.r11w =
    x86.Assembler.prototype.register("r11w",  43);
/** Predefined register object */
x86.Assembler.prototype.register.r12w =
    x86.Assembler.prototype.register("r12w",  44);
/** Predefined register object */
x86.Assembler.prototype.register.r13w =
    x86.Assembler.prototype.register("r13w",  45);
/** Predefined register object */
x86.Assembler.prototype.register.r14w =
    x86.Assembler.prototype.register("r14w",  46);
/** Predefined register object */
x86.Assembler.prototype.register.r15w =
    x86.Assembler.prototype.register("r15w",  47);
/** Predefined register object */
x86.Assembler.prototype.register.eax =
    x86.Assembler.prototype.register("eax",   16);
/** Predefined register object */
x86.Assembler.prototype.register.ecx =
    x86.Assembler.prototype.register("ecx",   17);
/** Predefined register object */
x86.Assembler.prototype.register.edx =
    x86.Assembler.prototype.register("edx",   18);
/** Predefined register object */
x86.Assembler.prototype.register.ebx =
    x86.Assembler.prototype.register("ebx",   19);
/** Predefined register object */
x86.Assembler.prototype.register.esp =
    x86.Assembler.prototype.register("esp",   20);
/** Predefined register object */
x86.Assembler.prototype.register.ebp =
    x86.Assembler.prototype.register("ebp",   21);
/** Predefined register object */
x86.Assembler.prototype.register.esi =
    x86.Assembler.prototype.register("esi",   22);
/** Predefined register object */
x86.Assembler.prototype.register.edi =
    x86.Assembler.prototype.register("edi",   23);
/** Predefined register object */
x86.Assembler.prototype.register.r8d =
    x86.Assembler.prototype.register("r8d",   24);
/** Predefined register object */
x86.Assembler.prototype.register.r9d =
    x86.Assembler.prototype.register("r9d",   25);
/** Predefined register object */
x86.Assembler.prototype.register.r10d =
    x86.Assembler.prototype.register("r10d",  26);
/** Predefined register object */
x86.Assembler.prototype.register.r11d =
    x86.Assembler.prototype.register("r11d",  27);
/** Predefined register object */
x86.Assembler.prototype.register.r12d =
    x86.Assembler.prototype.register("r12d",  28);
/** Predefined register object */
x86.Assembler.prototype.register.r13d =
    x86.Assembler.prototype.register("r13d",  29);
/** Predefined register object */
x86.Assembler.prototype.register.r14d =
    x86.Assembler.prototype.register("r14d",  30);
/** Predefined register object */
x86.Assembler.prototype.register.r15d =
    x86.Assembler.prototype.register("r15d",  31);
/** Predefined register object */
x86.Assembler.prototype.register.rax =
    x86.Assembler.prototype.register("rax",    0);
/** Predefined register object */
x86.Assembler.prototype.register.rcx =
    x86.Assembler.prototype.register("rcx",    1);
/** Predefined register object */
x86.Assembler.prototype.register.rdx =
    x86.Assembler.prototype.register("rdx",    2);
/** Predefined register object */
x86.Assembler.prototype.register.rbx =
    x86.Assembler.prototype.register("rbx",    3);
/** Predefined register object */
x86.Assembler.prototype.register.rsp =
    x86.Assembler.prototype.register("rsp",    4);
/** Predefined register object */
x86.Assembler.prototype.register.rbp =
    x86.Assembler.prototype.register("rbp",    5);
/** Predefined register object */
x86.Assembler.prototype.register.rsi =
    x86.Assembler.prototype.register("rsi",    6);
/** Predefined register object */
x86.Assembler.prototype.register.rdi =
    x86.Assembler.prototype.register("rdi",    7);
/** Predefined register object */
x86.Assembler.prototype.register.r8 =
    x86.Assembler.prototype.register("r8",     8);
/** Predefined register object */
x86.Assembler.prototype.register.r9 =
    x86.Assembler.prototype.register("r9",     9);
/** Predefined register object */
x86.Assembler.prototype.register.r10 =
    x86.Assembler.prototype.register("r10",   10);
/** Predefined register object */
x86.Assembler.prototype.register.r11 =
    x86.Assembler.prototype.register("r11",   11);
/** Predefined register object */
x86.Assembler.prototype.register.r12 =
    x86.Assembler.prototype.register("r12",   12);
/** Predefined register object */
x86.Assembler.prototype.register.r13 =
    x86.Assembler.prototype.register("r13",   13);
/** Predefined register object */
x86.Assembler.prototype.register.r14 =
    x86.Assembler.prototype.register("r14",   14);
/** Predefined register object */
x86.Assembler.prototype.register.r15 =
    x86.Assembler.prototype.register("r15",   15);
/** Predefined register object */
x86.Assembler.prototype.register.st =
    x86.Assembler.prototype.register("st",    48);
/** Predefined register object */
x86.Assembler.prototype.register.st_1 =
    x86.Assembler.prototype.register("st_1",  49);
/** Predefined register object */
x86.Assembler.prototype.register.st_2 =
    x86.Assembler.prototype.register("st_2",  50);
/** Predefined register object */
x86.Assembler.prototype.register.st_3 =
    x86.Assembler.prototype.register("st_3",  51);
/** Predefined register object */
x86.Assembler.prototype.register.st_4 =
    x86.Assembler.prototype.register("st_4",  52);
/** Predefined register object */
x86.Assembler.prototype.register.st_5 =
    x86.Assembler.prototype.register("st_5",  53);
/** Predefined register object */
x86.Assembler.prototype.register.st_6 =
    x86.Assembler.prototype.register("st_6",  54);
/** Predefined register object */
x86.Assembler.prototype.register.st_7 =
    x86.Assembler.prototype.register("st_7",  55);
/** Predefined register object */
x86.Assembler.prototype.register.mm0 =
    x86.Assembler.prototype.register("mm0",   56);
/** Predefined register object */
x86.Assembler.prototype.register.mm1 =
    x86.Assembler.prototype.register("mm1",   57);
/** Predefined register object */
x86.Assembler.prototype.register.mm2 =
    x86.Assembler.prototype.register("mm2",   58);
/** Predefined register object */
x86.Assembler.prototype.register.mm3 =
    x86.Assembler.prototype.register("mm3",   59);
/** Predefined register object */
x86.Assembler.prototype.register.mm4 =
    x86.Assembler.prototype.register("mm4",   60);
/** Predefined register object */
x86.Assembler.prototype.register.mm5 =
    x86.Assembler.prototype.register("mm5",   61);
/** Predefined register object */
x86.Assembler.prototype.register.mm6 =
    x86.Assembler.prototype.register("mm6",   62);
/** Predefined register object */
x86.Assembler.prototype.register.mm7 =
    x86.Assembler.prototype.register("mm7",   63);
/** Predefined register object */
x86.Assembler.prototype.register.xmm0 =
    x86.Assembler.prototype.register("xmm0",  64);
/** Predefined register object */
x86.Assembler.prototype.register.xmm1 =
    x86.Assembler.prototype.register("xmm1",  65);
/** Predefined register object */
x86.Assembler.prototype.register.xmm2 =
    x86.Assembler.prototype.register("xmm2",  66);
/** Predefined register object */
x86.Assembler.prototype.register.xmm3 =
    x86.Assembler.prototype.register("xmm3",  67);
/** Predefined register object */
x86.Assembler.prototype.register.xmm4 =
    x86.Assembler.prototype.register("xmm4",  68);
/** Predefined register object */
x86.Assembler.prototype.register.xmm5 =
    x86.Assembler.prototype.register("xmm5",  69);
/** Predefined register object */
x86.Assembler.prototype.register.xmm6 =
    x86.Assembler.prototype.register("xmm6",  70);
/** Predefined register object */
x86.Assembler.prototype.register.xmm7 =
    x86.Assembler.prototype.register("xmm7",  71);
/** Predefined register object */
x86.Assembler.prototype.register.xmm8 =
    x86.Assembler.prototype.register("xmm8",  72);
/** Predefined register object */
x86.Assembler.prototype.register.xmm9 =
    x86.Assembler.prototype.register("xmm9",  73);
/** Predefined register object */
x86.Assembler.prototype.register.xmm10 =
    x86.Assembler.prototype.register("xmm10", 74);
/** Predefined register object */
x86.Assembler.prototype.register.xmm11 =
    x86.Assembler.prototype.register("xmm11", 75);
/** Predefined register object */
x86.Assembler.prototype.register.xmm12 =
    x86.Assembler.prototype.register("xmm12", 76);
/** Predefined register object */
x86.Assembler.prototype.register.xmm13 =
    x86.Assembler.prototype.register("xmm13", 77);
/** Predefined register object */
x86.Assembler.prototype.register.xmm14 =
    x86.Assembler.prototype.register("xmm14", 78);
/** Predefined register object */
x86.Assembler.prototype.register.xmm15 =
    x86.Assembler.prototype.register("xmm15", 79);

/** Returns an {@link asm.CodeBlock#label} object. */
x86.Assembler.prototype.labelObj = function (text)
{
   return this.codeBlock.label(text);
};

//-----------------------------------------------------------------------------

// print formatting functions

/** @private Returns the right suffix depending on compilation target */
x86.Assembler.prototype._32or64bitSuffix = function ()
{
    return this.is64bitMode() ? "q" : "l";
};

/** @private Returns the right jump label suffix depending on length of jump */
x86.Assembler.prototype.jumpLabelSuffix = function (isShort)
{
    return isShort ? "" : this._32or64bitSuffix();
};

/** @private Returns an offset with an explicit sign as a string */
x86.offsetToString = function (offset)
{
    if (offset === 0)
    {
        return "";
    } else if (offset < 0)
    {
        return String(offset);
    } else
    {
        return "+" + String(offset);
    }
};

/** @private Returns the corresponding width suffix */
x86.widthSuffix = function (width)
{
    if      (width === 64) { return "q"; }
    else if (width === 32) { return "l"; }
    else if (width === 16) { return "w"; }
    else if (width === 8 ) { return "b"; }
    else                   { return "";  }
};

/** @private Returns the corresponding width suffix for a given register */
x86.regWidthSuffix = function (reg) { return x86.widthSuffix(reg.width()); };

/** @private Format an operand according to GNU (AT&T) assembly syntax */
x86.opndFormatGNU = function (opnd)
{
    var result;
    switch(opnd.type)
    {
        case x86.type.IMM_VAL:
            result = "$" + String(opnd.value);
            break;
        case x86.type.LINK:
            result = "<" + opnd.name + ">";
            break;
        case x86.type.REG:
            result = "%" + opnd.name;
            break;
        case x86.type.MEM:
            if (opnd.base !== null)
            {
                result = ((opnd.disp === 0) ? "" : opnd.disp) + "(" +
                         x86.opndFormatGNU(opnd.base) +
                         ((opnd.index === null) ?
                              "" : " + " + x86.opndFormatGNU(opnd.index)) +
                         ((opnd.index === null || opnd.scale === 1) ?
                              "" : "*" + opnd.scale.toString()) +
                         ")";
            } else
            {
                result = x86.offsetToString(opnd.disp);
            }
            break;
        default:
            result = opnd.toString();
            break;
    }
    return result;
};

/** @private Format an operand according to the current assembly syntax */
x86.opndFormat = function (opnd) { return x86.opndFormatGNU(opnd); };

/** @private Format an instruction according to GNU (AT&T) assembly syntax */
x86.instrFormatGNU = function (mnemonic, suffix, dest, src)
{
    var opnds = "";

    if (dest && src)
    {
        opnds = x86.opndFormatGNU(src) + "," + x86.opndFormatGNU(dest);
    } else  if (dest)
    {
        opnds = x86.opndFormatGNU(dest);
    }

    if (suffix)
    {
        return mnemonic + suffix + " " + opnds; // 1 or 2 generic instruction
    } else if (dest && !src &&
               (dest.type === x86.type.REG ||
                dest.type === x86.type.MEM))
    {
        return mnemonic + " *" + opnds; // call instruction
    } else
    {
        return mnemonic + " " + opnds;
    }
};

/** @private Format an instruction according to the current assembly syntax */
x86.instrFormat = function (mnemonic, suffix, dest, src)
{
    return x86.instrFormatGNU(mnemonic, suffix, dest, src);
};

/** @private Format a label */
x86.labelFormat = function (label)
{
    return label.name() + ":";
};

//-----------------------------------------------------------------------------

// x86 instruction encoding.

/** @private generic no operand instruction encoding */
x86.Assembler.prototype.noOpndInstr = function (opcode, mnemonic)
{
    this.gen8(opcode);
    if (this.useListing)
        this.genListing(x86.instrFormat(mnemonic, ""));
    return this;
};

/** Adds text to the listing output. Can be chained. */
x86.Assembler.prototype.genListing = function (text)
{
    this.codeBlock.genListing(text);
    return this;
};

/** @private
    Adds a prefix byte selecting a non-default operand size for
    the following instruction.
    This prefix allows mixing 16-bit, 32-bit and 64-bit data
    on an instruction-by-instruction basis.
*/
x86.Assembler.prototype.opndSizeOverridePrefix = function (width)
{
    if (width === 16)
    {
        this.gen8(0x66);
    };
    return this;
};

/** @private
    Adds a prefix byte selecting a non-default address size
    for the following instruction.
    This prefix allows mixing 16-bit and 32-bit or 32-bit and 64-bit
    addresses on an instruction-by-instruction basis.
*/
x86.Assembler.prototype.addrSizeOverridePrefix = function (opnd)
{
    if (opnd.type === x86.type.MEM &&
        opnd.base !== null         &&
        this.is64bitMode() !== opnd.base.isr64())
    {
        this.gen8(0x67);
    }
};
/** @private
    Adds the needed prefixes to an instruction.

    REX: use extended registers,
         use 64-bit versions of GPRs,
         use extended control and debug registers,
         use uniform byte registers
    Operand Size Override: see above
    Address Size Override: see above

*/
x86.Assembler.prototype.opndPrefix = function (width, field, opnd, forceRex)
{
    var rex = // If needed emit REX.W (64 bit operand size)
              (( width === 64 || ( opnd.type === x86.type.REG && opnd.isr64()))
               ? 8 : 0) +
              // If needed emit REX.R (Extension of the ModR/M reg field)
              (( field >> 3) << 2);

    // finish setting the rex value
    switch (opnd.type)
    {
        case x86.type.REG:
            // If needed emit REX.B (Extension of the ModR/M r/m field,
            // SIB base field, or Opcode reg field)
            rex += (opnd.field() >> 3);
            break;

        case x86.type.LINK:
            break;

        case x86.type.MEM:
            const base = opnd.base;
            if (base !== null)
            {
                assert(
                    (base.isr32() || (base.isr64() &&
                    this.is64bitMode())),
                    "invalid width base register " + base
                );

                // If needed emit REX.B (Extension of the ModR/M r/m field,
                // SIB base field, or Opcode reg field
                rex += (base.field() >> 3);

                const index = opnd.index;
                if (index !== null)
                {
                    assert(
                        (base.isr32() ? index.isr32() : index.isr64()),
                        "index register must have the" +
                        " same width as base " + base
                    );

                    rex += ((index.field() >> 3) << 1);
                }
            }
            break;

        default:
            error("unknown operand '" + opnd + "'");
    }

    this.opndSizeOverridePrefix(width);
    this.addrSizeOverridePrefix(opnd);

    if (forceRex || (rex !== 0))
    {
        this.assert64bitMode();
        this.gen8(0x40 + rex);
        return true;
    }
    return false;
};

/**
    @private
    Adds the ModRM, SIB and DISP bytes to the instruction.

    ModRM: Extension of the opcode or register access
    SIB:   Scale, Index, Base for memory access
    DISP:  Displacement byte for memory access
*/
x86.Assembler.prototype.opndModRMSIB = function (field, opnd)
{
    const modrm_rf = (7 & field) << 3;
    const that = this;

    /** @ignore */
    function absAddr()
    {
        if (that.is64bitMode())
        {
            that.gen8(modrm_rf + 4); // ModR/M
            that.gen8(0x25);         // SIB
        } else
        {
            that.gen8(modrm_rf + 5); // ModR/M
        }
    }

    var modrm;
    switch (opnd.type)
    {
        case x86.type.REG:
            modrm = modrm_rf + (7 & opnd.field());
            this.gen8(0xc0 + modrm); // ModR/M
            break;

        case x86.type.LINK:
            error("unimplemented for opnd of type LINK");
            break;

        case x86.type.MEM:
            const base  = opnd.base;
            const baseFieldLo = 7 & base.field();
            const index = opnd.index;
            const disp  = opnd.disp;
            const scale = opnd.scale;

            if (base !== null)
            {
                // index: Need a SIB when using an index
                // baseFieldLo: register or base = RSP/R12
                if (index !== null || (baseFieldLo === 4))
                {
                    // SIB Needed
                    modrm = modrm_rf + 4;
                    var sib = baseFieldLo;

                    if (index !== null)
                    {
                        // Compute the two scale bits
                        var scaleBits;
                        switch (scale)
                        {
                            case 1: scaleBits = 0; break;
                            case 2: scaleBits = 1; break;
                            case 4: scaleBits = 2; break;
                            case 8: scaleBits = 3; break;
                        }

                        assert(
                            !(index.field() === 4),
                            "SP not allowed as index " + index
                        );

                        sib += ((7 & index.field()) << 3) + (scaleBits << 6);
                    } 
                    else // !index
                    {
                        sib += 0x20;
                    }

                    if (x86.isSigned8(disp))
                    {
                        // use 8 bit displacement
                        if (!(disp === 0) ||        // non-null displacement
                              (base.field() === 5)) // or RBP
                        {
                            this.gen8(0x40 + modrm);// ModR/M
                            this.gen8(sib);         // SIB
                            this.gen8(disp);
                        } else
                        {
                            this.gen8(modrm);      // ModR/M
                            this.gen8(sib);        // SIB
                        }
                    } else // !x86.isSigned8(disp)
                    {
                        // use 32 bit displacement
                        this.gen8(0x80 + modrm);   // ModR/M
                        this.gen8(sib);            // SIB
                        this.gen32(disp);
                    }
                } else // !index && !baseFieldLo === 4
                {
                    // SIB Not Needed
                    modrm = modrm_rf + baseFieldLo;
                    if (x86.isSigned8(disp))
                    {
                        if (!(disp === 0) ||       // non-null displacement
                            (baseFieldLo === 5))   // or RBP/R13
                        {
                            // use 8 bit displacement
                            this.gen8(0x40 + modrm);// ModR/M
                            this.gen8(disp);
                        } else
                        {
                            this.gen8(modrm);       // ModR/M
                        }
                    } else // !x86.isSigned8(disp)
                    {
                        // use 32 bit displacement
                        this.gen8(0x80 + modrm);
                        this.gen32(disp);
                    }
                }
            } else // (base !== null)
            {
                // Absolute address, use disp32 ModR/M
                absAddr();
                this.gen32(disp);
            }
            break;

        default:
            error("unkown operand " + opnd);
    }
    return this;
};

/**
    @private
    Adds the corresponding prefix for two operands knowing one of the operand
    is a register
*/
x86.Assembler.prototype.opndPrefixRegOpnd = function (reg, opnd)
{
    const width  = reg.width();
    const field  = reg.field();
    const isExtLo8 = ((width === 8) && (field >= 4) && (!reg.isr8h()));

    if (opnd.type === x86.type.REG)
    {
        const isExtLo8Reg2 = ((width === 8) &&
                              (opnd.field() >= 4) &&
                              (!opnd.isr8h()));
        var isRex;
        assert(
            ((width === opnd.width()) ||
            reg.isxmm()),   // for cvtsi2ss/cvtsi2sd instructions
            "registers are not of the same width " + reg + " " + opnd
        );

        isRex = this.opndPrefix(width, field, opnd, (isExtLo8 || isExtLo8Reg2));

        assert(
            !(isRex && (reg.isr8h() || opnd.isr8h())),
            "cannot use high 8 bit register here " + reg + " " + opnd
        );

        return isRex;
    } else  // opnd.type !== x86.type.REG
    {
        return this.opndPrefix(width, field, opnd, isExtLo8);
    }
};

/** @private */
x86.Assembler.prototype.opndPrefixOpnd = function (width, opnd)
{
    if (opnd.type === x86.type.REG)
    {
        const field = opnd.field();
        const isExtLo8 = ((width === 8) && (field >= 4) && (!opnd.isr8h()));
        return this.opndPrefix(width, 0, opnd, isExtLo8);
    } else // opnd.type !== x86.type.REG
    {
        return this.opndPrefix(width, 0, opnd, false);
    }
};

/** @private */
x86.Assembler.prototype.opndModRMSIBRegOpnd = function (reg, opnd)
{
    return this.opndModRMSIB(reg.field(), opnd);
};

/**
    @private
    generic encoding for instructions with the source
    operand as an immediate value
*/
x86.Assembler.prototype.opImm = function (op, mnemonic, src, dest, width)
{
    const that = this;
    const isLink = src.type === x86.type.LINK;
    const k = (isLink) ? 0 : src.value;

    /** @ignore Adds the listing for the instruction */
    function listing(width,v)
    {
        if (that.useListing)
        {
            const value = (isLink) ? v : that.immediateValue(v);
            that.genListing(x86.instrFormat(mnemonic,
                                             x86.widthSuffix(width),
                                             dest,
                                             value));
        }
    }

    /** @ignore generate a constant value */
    function cstValue(width)
    {
        var value;

        if (isLink)
        {
            value = src;
            that.require(value);
        } else
        {
            value = that.genImmNum(k, width);
        }
        
        listing(width, value);
    }

    /**
        @ignore
        Generate encoding when the targeted register is AL or AX/EAX/RAX
    */
    function accumulator (width)
    {
        that.
        opndPrefixOpnd(width, dest);
        // opcode = #x04, #x0c, #x14, ..., #x3c (for AL)
        //       or #x05, #x0d, #x15, ..., #x3d (for AX/EAX/RAX)
        that.gen8(((width === 8) ? 0x04 : 0x05) + (op << 3));
        
        cstValue(width);
    }

    /**
        @ignore
        General case
    */
    function general(width)
    {
        that.opndPrefixOpnd(width, dest);

        if (width === 8 && !isLink)
        {
            that.
            gen8(0x80).            // opcode = 8 bit operation
            opndModRMSIB(op,dest); // ModR/M
            listing(width, that.genImmNum(k,8));
        } else if (x86.isSigned8(k) && !isLink)
        {
            that.
            gen8(0x83).            // opcode = sign extended 8 bit imm
            opndModRMSIB(op,dest); // ModR/M
            listing(width, that.genImmNum(k,8));
        } else
        {
            that.
            gen8(0x81).            // opcode = sign extended 16/32 bit imm
            opndModRMSIB(op,dest); // ModR/M

            cstValue(width);
        }
    }

    assert (
        (dest.type === x86.type.REG) ?
        (!width || (dest.width() === width)) : width,
        "missing or inconsistent operand width " + width
    );

    if (dest.type === x86.type.REG)
    {
        if ((dest.field() === 0) &&
            (dest.width() === 8 || !x86.isSigned8(k)))
        {
            accumulator(dest.width());
        } else
        {
            general(dest.width());
        }
    } else // dest.type !== x86.type.REG
    {
        general(width);
    }
    return that;
};

/**
    @private
    special case for encoding the mov instruction with an
    immediate value
*/
x86.Assembler.prototype.movImm = function (dest, src, width)
{

    const that = this;
    const isLink = src.type === x86.type.LINK;
    const k = (isLink) ? src : src.value;

    /** @ignore generate listing */
    function listing(width,v)
    {
        if (that.useListing)
        {
            const value = (isLink) ? v : that.immediateValue(v);
            that.genListing(x86.instrFormat("mov",
                                             x86.widthSuffix(width),
                                             dest,
                                             value));
        }
    }

    /** @ignore generate a constant value */
    function cstValue(width, max32)
    {
        var value;

        if (isLink)
        {
            value = src;
            that.require(value);
        } else if (max32 === true)
        {
            value = that.genImmNum(k, width);
        } else
        {
            value = that._genImmNum(k, width);
        }
        
        listing(width, value);
    }

    /** @ignore special case when the destination is a register */
    function register(width)
    {
        that.
        opndPrefixOpnd(width, dest); // prefix

        // opcode = #xb0-#xb7 (for 8 bit registers)
        //      or #xb8-#xbf (for 16/32/64 bit registers)
        that.gen8( ((width === 8) ? 0xb0 : 0xb8) + (7 & dest.field()) );

        cstValue(width, false);
    }

    /** @ignore general case */
    function general(width)
    {
        assert(
            that.is32bitImm(src), 
            "Immediate value '" + src + 
             "' should have a width less or equal to 32"
        );

        that.opndPrefixOpnd(width, dest);
        that.
        gen8((width === 8) ? 0xc6 : 0xc7).  // opcode
        opndModRMSIB(0,dest); // ModR/M

        cstValue(width, true);
    }

    assert((dest.type === x86.type.REG) ?
        (!width || (dest.width() === width)) : width,
        "missing or inconsistent operand width '" + width + "'"
    );

    if (dest.type === x86.type.REG)
    {
        if (dest.width() === 64 && 
            num_instance(k) &&
            x86.isSigned32(k))
        {
            general(dest.width());
        }
        else
        {
            register(dest.width());
        }
    }
    else // dest.type !== x86.type.REG
    {
        general(width);
    }

    return this;
};

/** @private generic two operands instruction encoding */
x86.Assembler.prototype.op = function (op, mnemonic, dest, src, width)
{
    // TODO: Add support for immediate label, see x86-mov
    const that = this;

    assert( dest.type === x86.type.REG ||
            dest.type === x86.type.MEM ||
            src.type  === x86.type.REG ||
            src.type  === x86.type.MEM,
            "one of dest or src must be a register or" +
            " a memory location");

    assert(!(dest.type === x86.type.MEM &&
             src.type  === x86.type.MEM),
             "dest and src cannot refer both to a " +
             "memory location");

    /** @ignore generate the instruction */
    function genOp(reg, opnd, isSwapped)
    {
        assert(!width || (reg.width() === width),
               "inconsistent operand width '" + width +
               "' and register width '" + reg.width() + "'");

        that.opndPrefixRegOpnd(reg, opnd);
        that.
        gen8((op << 3) +
             (isSwapped ? 0 : 2) +
             (reg.isr8() ? 0 : 1)).
        opndModRMSIBRegOpnd(reg, opnd);

        if (that.useListing)
        {
            that.genListing(x86.instrFormat(mnemonic,
                                             x86.regWidthSuffix(reg),
                                             (isSwapped) ? opnd : reg,
                                             (isSwapped) ? reg  : opnd));
        }
    }

    if (src.type === x86.type.IMM_VAL || src.type === x86.type.LINK)
    {

        if (op === 17)
        {
            this.movImm(dest, src, width);
        } 
        else
        {
            assert(
                this.is32bitImm(src), 
                "Immediate value '" + src + 
                 "' should have a width less or equal to 32"
            );
            this.opImm(op, mnemonic, src, dest, width);
        }
    }
    else if (src.type === x86.type.REG)
    {
        genOp(src, dest, true);
    } 
    else if (dest.type === x86.type.REG)
    {
        genOp(dest, src, false);
    }
    else
    {
        error("invalid operand combination " + dest + " " + src);
    }

    return this;
};

/**
    @private
    special case when a push instruction has an immediate value
    as a destination
*/
x86.Assembler.prototype.pushImm = function (dest)
{
    // width is always width of stack pointer

    const that = this;
    const k = dest.value;

    /** @ignore */
    function listing(n)
    {
        if (that.useListing)
        {
            that.genListing(x86.instrFormat("push",
                                             that._32or64bitSuffix(),
                                             that.immediateValue(n)));
        }
    }

    if (x86.isSigned8(k))
    {
        this.gen8(0x6a); // opcode
        listing(this._genImmNum(k, 8));
    } 
    else
    {
        this.gen8(0x68); // opcode
        listing(this._genImmNum(k, 32));
    }
};

/**
    @private
    encoding of push and pop instructions
*/
x86.Assembler.prototype.pushPop = function (opnd, isPop)
{
    const that = this;
    /** @ignore */
    function listing()
    {
        if (that.useListing)
        {
            that.genListing(x86.instrFormat(isPop ? "pop" : "push",
                                             that._32or64bitSuffix(),
                                             opnd));
        }
    }

    /** @ignore special case when the destination is a register */
    function register()
    {
        if (opnd.isr32())
        {
            that.assert32bitMode();
            assert(opnd.field() < 8,
                   "cannot push/pop extended register" +
                   " in 32 bit mode");
        } 
        else
        {
            that.assert64bitMode();
            if (opnd.field() >= 8)
            {
                that.gen8(0x41); // REX
            }
        }

        // opcode 0x50 - 0x5f
        that.gen8((isPop ? 0x58 : 0x50) + (7 & opnd.field()));
        listing();
    }

    /** @ignore general case */
    function general()
    {
        that.opndPrefix(0,0,opnd,false); // prefix (width is implicit)
        if(isPop) { that.gen8(0x8f); } else { that.gen8(0xff); } // opcode
        that.opndModRMSIB((isPop ? 0 : 6), opnd);
        listing();
    }

    if (!isPop && opnd.type === x86.type.IMM_VAL)
    {
        this.pushImm(opnd);
    } else if (opnd.type === x86.type.REG)
    {
        register();
    } else
    {
        general();
    }
    return this;
};

/**
    @private
    encoding of inc/dec instructions
*/
x86.Assembler.prototype.incDec = function (opnd, isInc, width)
{
    const that = this;

    /** @ignore generate listing */
    function listing(width)
    {
        if (that.useListing)
        {
            that.genListing(x86.instrFormat((isInc ? "inc" : "dec"),
                                             x86.widthSuffix(width),
                                             opnd));
        }
    }

    /** @ignore special case when the destination is a register */
    function register()
    {
        // No rex prefix must be used since inc or dec
        // opcode are used as rex prefixes
        that.
        gen8( (isInc ? 0x40 : 0x48) + (7 & opnd.field()) );
        listing(opnd.width());
    }

    /** @ignore general case */
    function general(width)
    {
        that.opndPrefixOpnd(width, opnd);
        that.
        gen8((width === 8) ? 0xfe : 0xff).  // opcode
        opndModRMSIB((isInc ? 0 : 1),opnd); // ModR/M
        listing(width);
    }

    assert((opnd.type === x86.type.REG) ?
            (!width || (opnd.width() === width)) : width,
            "missing or inconsistent operand width '" + width + "'");

    if (opnd.type === x86.type.REG)
    {
        if (opnd.width() === 64)
        {
            general(opnd.width());
        } else
        {
            register();
        }
    } else // opnd.type !== x86.type.REG
    {
        general(width);
    }
    return this;
};

x86.opcode = {};
// Escape opcode
x86.opcode.esc        = 0x0f;

// Unconditional jump/call opcodes
x86.opcode.jmpRel8    = 0xeb;
x86.opcode.jmpRel32   = 0xe9;
x86.opcode.callRel32  = 0xe8;

// Conditional jump opcodes (for the rel32 kind, add 0x10 with 0x0f opcode)
x86.opcode.joRel8     = 0x70;
x86.opcode.jnoRel8    = 0x71;
x86.opcode.jbRel8     = 0x72;
x86.opcode.jaeRel8    = 0x73;
x86.opcode.jeRel8     = 0x74;
x86.opcode.jneRel8    = 0x75;
x86.opcode.jbeRel8    = 0x76;
x86.opcode.jaRel8     = 0x77;
x86.opcode.jsRel8     = 0x78;
x86.opcode.jnsRel8    = 0x79;
x86.opcode.jpRel8     = 0x7a;
x86.opcode.jnpRel8    = 0x7b;
x86.opcode.jlRel8     = 0x7c;
x86.opcode.jgeRel8    = 0x7d;
x86.opcode.jleRel8    = 0x7e;
x86.opcode.jgRel8     = 0x7f;

// Conditional move opcodes
x86.opcode.cmovo      = 0x40;
x86.opcode.cmovno     = 0x41;
x86.opcode.cmovb      = 0x42;
//x86.opcode.cmovc      = x86.opcode.cmovb;
//x86.opcode.cmovnae    = x86.opcode.cmovb;
x86.opcode.cmovnb     = 0x43;
//x86.opcode.cmovnc     = x86.opcode.cmovnb;
//x86.opcode.cmovae     = x86.opcode.cmovnb;
x86.opcode.cmovz      = 0x44;
//x86.opcode.cmove      = x86.opcode.cmovz;
x86.opcode.cmovnz     = 0x45;
//x86.opcode.cmovne     = x86.opcode.cmovnz;
x86.opcode.cmovbe     = 0x46;
//x86.opcode.cmovna     = x86.opcode.cmovbe;
x86.opcode.cmovnbe    = 0x47;
//x86.opcode.cmova      = x86.opcode.cmovnbe;
x86.opcode.cmovs      = 0x48;
x86.opcode.cmovns     = 0x49;
x86.opcode.cmovp      = 0x4a;
//x86.opcode.cmovpe     = x86.opcode.cmovp;
x86.opcode.cmovnp     = 0x4b;
//x86.opcode.cmovpo     = x86.opcode.cmovnp;
x86.opcode.cmovl      = 0x4c;
//x86.opcode.cmovnge    = x86.opcode.cmovl;
x86.opcode.cmovnl     = 0x4d;
//x86.opcode.cmovge     = x86.opcode.cmovnl;
x86.opcode.cmovle     = 0x4e;
//x86.opcode.cmovng     = x86.opcode.cmovle;
x86.opcode.cmovnle    = 0x4f;
//x86.opcode.cmovg      = x86.opcode.cmovnle;

/** Adds a label to the code stream. Can be chained. */
x86.Assembler.prototype.label = function (lbl)
{
    assert(lbl.type === asm.type.LBL,
               "invalid label" + lbl);
    this.
    genListing(x86.labelFormat(lbl)).
    codeBlock.genLabel(lbl);
    return this;
};

/** @private Generic jump to label instruction encoding */
x86.Assembler.prototype.jumpLabel = function (opcode, mnemonic, label, offset)
{
    if (offset === undefined)
        offset = 0;

    const that = this;

    assert(label.type === asm.type.LBL,
           "invalid label '" + label + "'");

    /** @ignore */
    function labelDist(label, offsetLabel, pos, offsetPos)
    {
        return (label.getPos() + offsetLabel) - (pos + offsetPos);
    };

    /** @ignore */
    function listing(isShort)
    {
        if (that.useListing)
        {
            that.genListing(
                x86.instrFormat(mnemonic,
                                 that.jumpLabelSuffix(isShort),
                                 label.name() + x86.offsetToString(offset)));
        }
    };

    /** @ignore Short Displacement (-128..127 bytes) */
    function shortDispCheck(cb, pos)
    {
       return x86.isSigned8(labelDist(label, offset, pos, 2)) ? 2 : null;
    };

    /** @ignore for an invalid check */
    function alwaysNull(cb, pos) { return null; };

    /** @ignore generate a short displacement instruction */
    function shortDispProd(cb, pos)
    {
        cb.gen8(opcode);
        cb.gen8(labelDist(label, offset, pos, 2) & 0xFF);
        listing(true);
    };

    /** @ignore 32 bit relative address */
    function dispCheck(cb, pos)
    {
        return (opcode === x86.opcode.jmpRel8 ||
                opcode === x86.opcode.callRel32) ? 5 : 6;
    };

    /** @ignore generate a 32 bit relative address displacement instruction */
    function dispProd(cb, pos)
    {
        switch (opcode)
        {
            case x86.opcode.jmpRel8:
                cb.
                gen8(x86.opcode.jmpRel32).
                gen32(labelDist(label, offset, pos, 5));
                break;
            case x86.opcode.callRel32:
                cb.
                gen8(opcode).
                gen32(labelDist(label, offset, pos, 5));
                break;
            default:
                // opcode is for a conditional jump
                cb.
                gen8(x86.opcode.esc).
                gen8(opcode + 0x10).
                gen32(labelDist(label, offset, pos, 6));
                break;
        }
        listing(false);
    };

    var checks = [(opcode === x86.opcode.callRel32) ? alwaysNull:shortDispCheck,
                  dispCheck];
    var prods  = [shortDispProd, dispProd];
    this.codeBlock.genDeferred(checks, prods);
    return this;
};

/** @private Generic jump to a linked address */
x86.Assembler.prototype.jumpLink = function (opcode, mnemonic, linkObj)
{
    assert(linkObj.type === x86.type.LINK,
           "invalid link object '" + linkObj + "'");

    assert(linkObj.width() === 32, "Invalid width " + linkObj.width());

    switch (opcode)
    {
        case x86.opcode.jmpRel8:
            this.
            gen8(x86.opcode.jmpRel32);
            break;
        case x86.opcode.callRel32:
            this.
            gen8(opcode);
            break;
        default:
            // opcode is for a conditional jump
            this.
            gen8(x86.opcode.esc).
            gen8(opcode + 0x10);
            break;
    }

    this.
    require(linkObj).
    genListing(
        x86.instrFormat(mnemonic, x86.widthSuffix(linkObj.width()), linkObj));

    return this;
};
/** @private jump for the general case */
x86.Assembler.prototype.jumpGeneral = function (field, opnd)
{
    assert(!(opnd.type === x86.type.REG) ||
           (this.is64bitMode() ? opnd.isr64() : opnd.isr32()),
           "invalid width register" + opnd);

    this.opndPrefix(0,0,opnd,false);

    this.
    gen8(0xff).
    opndModRMSIB(field, opnd);

    if (this.useListing)
    {
        this.genListing(
            x86.instrFormat((field === 4) ? "jmp" : "call",
                             null,
                             opnd));
    }

    return this;
};




// No operand instructions
/** Can be chained. */
x86.Assembler.prototype.ret = function (opnd)
{
    if (opnd === undefined)
    {
        return this.noOpndInstr(0xc3, "ret");
    } else 
    {
        assert(opnd.type === x86.type.IMM_VAL,
               "Invalid operand type");
        this.
        gen8(0xc2).
        genImmNum(opnd.value, 16);

        this.
        genListing(x86.instrFormat("ret",
                                   "",
                                   opnd));
    }
};
/** Can be chained. */
x86.Assembler.prototype.cmc = function ()
{
    return this.noOpndInstr(0xf5, "cmc");
};
/** Can be chained. */
x86.Assembler.prototype.clc = function ()
{
    return this.noOpndInstr(0xf8, "clc");
};
/** Can be chained. */
x86.Assembler.prototype.stc = function ()
{
    return this.noOpndInstr(0xf9, "stc");
};
/** Can be chained. */
x86.Assembler.prototype.cli = function ()
{
    return this.noOpndInstr(0xfa, "cli");
};
/** Can be chained. */
x86.Assembler.prototype.sti = function ()
{
    return this.noOpndInstr(0xfb, "sti");
};
/** Can be chained. */
x86.Assembler.prototype.cld = function ()
{
    return this.noOpndInstr(0xfc, "cld");
};
/** Can be chained. */
x86.Assembler.prototype.std = function ()
{
    return this.noOpndInstr(0xfd, "std");
};


// Two operand instructions
/** Can be chained.
    @param src
    @param dest
    @param {Number} width optional
*/
x86.Assembler.prototype.add = function (src, dest, width)
{
    return this.op(0, "add",dest,src,width);
};
/** Can be chained.
    @param src
    @param dest
    @param {Number} width optional
*/
x86.Assembler.prototype.or = function (src, dest, width)
{
    return this.op(1, "or", dest,src,width);
};
/** Can be chained.
    @param src
    @param dest
    @param {Number} width optional
*/
x86.Assembler.prototype.adc = function (src, dest, width)
{
    return this.op(2, "adc",dest,src,width);
};
/** Can be chained.
    @param src
    @param dest
    @param {Number} width optional
*/
x86.Assembler.prototype.sbb = function (src, dest, width)
{
    return this.op(3, "sbb",dest,src,width);
};
/** Can be chained.
    @param src
    @param dest
    @param {Number} width optional
*/
x86.Assembler.prototype.and = function (src, dest, width)
{
    return this.op(4, "and",dest,src,width);
};
/** Can be chained.
    @param src
    @param dest
    @param {Number} width optional
*/
x86.Assembler.prototype.sub = function (src, dest, width)
{
    return this.op(5, "sub",dest,src,width);
};
/** Can be chained.
    @param src
    @param dest
    @param {Number} width optional
*/
x86.Assembler.prototype.xor = function (src, dest, width)
{
    return this.op(6, "xor",dest,src,width);
};
/** Can be chained. Note: src and dest are inverted compared to
    the intel syntax. Therefore, cmp(src, dest) will set the flags
    OF<>SF iff dest < src.

    @param src
    @param dest
    @param {Number} width optional
*/
x86.Assembler.prototype.cmp = function (src, dest, width)
{
    return this.op(7, "cmp",dest,src,width);
};
/** Can be chained.
    @param src
    @param dest
    @param {Number} width optional
*/
x86.Assembler.prototype.mov = function (src, dest, width)
{
    return this.op(17,"mov",dest,src,width);
};

/** Generic move with value extension. Can be chained.
    @param src
    @param dst
    @param signExt perform signed extension or zero extension
    @param {Number} width optional
*/
x86.Assembler.prototype.movxx = function (src, dst, signExt, width)
{
    assert(
        dst.type === x86.type.REG ||
        src.type  === x86.type.REG ||
        src.type  === x86.type.MEM,
        'src must be reg or mem, dst must be reg'
    );

    srcWidth = src.width? src.width():width;

    assert(
        srcWidth < dst.width(),
        'src width must be less than dst width'
    );

    /** @ignore generate the instruction */
    const that = this;
    function genOp(mnem, opb1, opb2, reg, opnd)
    {
        that.opndPrefix(reg.width(), reg.field(), opnd);

        that.gen8(opb1);
        if (opb2) that.gen8(opb2);

        that.opndModRMSIBRegOpnd(reg, opnd);

        if (that.useListing)
        {
            that.genListing(
                x86.instrFormat(
                    mnem,
                    x86.widthSuffix(srcWidth) + x86.regWidthSuffix(reg),
                    reg,
                    opnd
                )
           );
        }
    }

    if (srcWidth === 8 && signExt)
    {
        // 0F BE/r
        genOp('movsx', 15, 190, dst, src);
    }
    else if (srcWidth === 16 && signExt)
    {
        // 0F BF/r
        genOp('movsx', 15, 191, dst, src);
    }
    else if (srcWidth === 32 && dst.width() === 64 && signExt)
    {
        // 63/r
        genOp('movsxd', 99, undefined, dst, src);
    }
    else if (srcWidth === 8 && !signExt)
    {
        // 0F B6/r
        genOp('movzx', 15, 182, dst, src);
    }
    else if (srcWidth === 16 && !signExt)
    {
        // 0F B7/r
        genOp('movzx', 15, 183, dst, src);
    }
    else if (srcWidth === 32 && dst.width() === 64 && !signExt)
    {
        // Use a regular move
        that.mov(src, dst.subReg(32));
    }
    else
    {
        assert (
            false,
            'invalid operand combination'
        );
    }

    return this;
};

/** Move with sign extension. Can be chained.
    @param src
    @param dst
    @param {Number} width optional
*/
x86.Assembler.prototype.movsx = function (src, dst, width)
{
    return this.movxx(src, dst, true, width);
};

/** Move with zero extension. Can be chained.
    @param src
    @param dst
    @param {Number} width optional
*/
x86.Assembler.prototype.movzx = function (src, dst, width)
{
    return this.movxx(src, dst, false, width);
};

/** Can be chained. */
x86.Assembler.prototype.push = function (opnd)
{
    return this.pushPop(opnd, false);
};
/** Can be chained. */
x86.Assembler.prototype.pop  = function (opnd)
{
    return this.pushPop(opnd, true);
};

/** Can be chained. */
x86.Assembler.prototype.inc  = function (opnd, width)
{
    return this.incDec(opnd, true, width);
};

/** Can be chained. */
x86.Assembler.prototype.dec  = function (opnd, width)
{
    return this.incDec(opnd, false, width);
};

x86.Assembler.prototype.lea  = function (src, dest)
{
    assert(
        dest.type === x86.type.REG,
        "'dest' argument must be a register, instead received " + dest
    );
    assert(
        !dest.isr8(),
        "'dest' argument must not be an 8 bit register"
    );
    assert(
        src.type === x86.type.MEM,
        "'src' argument must be a memory operand," +
        " instead received " + dest
    );

    this.opndPrefixRegOpnd(dest, src);
    this.gen8(0x8d); // opcode
    this.opndModRMSIBRegOpnd(dest, src);

    if (this.useListing)
    {
        this.genListing(x86.instrFormat("lea",
                                        x86.regWidthSuffix(dest),
                                        dest,
                                        src));
    }
    return this;
};

/** Can be chained */
x86.Assembler.prototype.test = function (src, dest, width)
{
    const that = this;
    const k = src.value;

    assert(src.type === x86.type.IMM_VAL || src.type === x86.type.REG,
               "'src' must be an immediate value or a register instead of " +
               src);

    assert(dest.type === x86.type.REG || dest.type === x86.type.MEM,
               "'dest' must be a register or a memory location instead of " +
               dest);

    assert((dest.type === x86.type.REG) ?
                ((width === undefined) || (dest.width() === width)) :
                ((src.type === x86.type.REG) ?
                 ((width === undefined) || (src.width() === width)) :
                 (width !== undefined)),
               "missing or inconsistent operand width");

    function listing(width, n)
    {
        if (that.useListing)
        {
            that.genListing(x86.instrFormat("test",
                                             x86.widthSuffix(width),
                                             dest,
                                             that.immediateValue(n)));
        }
    };

    function accumulatorImm(width)
    {
        that.opndSizeOverridePrefix(width);
        that.gen8((width === 8) ? 0xa8 : 0xa9); // opcode
        listing(width, that.genImmNum(k, width));
    };

    function generalImm(width)
    {
        that.opndPrefixOpnd(width, dest);
        that.gen8((width === 8) ? 0xf6 : 0xf7); // opcode
        that.opndModRMSIB(0, dest);
        listing(width, that.genImmNum(k, width));
    };

    function generalReg(width)
    {
        that.opndPrefixOpnd(width, dest);
        that.gen8((width === 8) ? 0x84 : 0x85); // opcode
        that.opndModRMSIBRegOpnd(src, dest);
        that.genListing(x86.instrFormat("test",
                                        x86.widthSuffix(width),
                                        dest,
                                        src));
    };

    if (dest.type === x86.type.REG &&
        dest.field() === 0 &&
        src.type === x86.type.IMM_VAL)
    {
        accumulatorImm(dest.width());
    } else if (dest.type === x86.type.REG &&
               src.type === x86.type.IMM_VAL)
    {
        generalImm(dest.width());
    } else if (src.type === x86.type.REG)
    {
        generalReg(src.width());
    }
    return this;
};

/** Can be chained */
x86.Assembler.prototype.cmoveGeneral  = function (op, mnemonic, src, dest)
{
    assert(dest.type === x86.type.REG,
               "'dest' argument must be a register, instead received " + dest);
    assert(!dest.isr8(),
               "'dest' argument must not be an 8 bit register");
    assert(src.type === x86.type.MEM || src.type === x86.type.REG,
               "'src' argument must be a memory or a register operand," +
               " instead received " + src);

    this.opndPrefixRegOpnd(dest, src);
    this.gen8(x86.opcode.esc);
    this.gen8(op); // opcode
    this.opndModRMSIBRegOpnd(dest, src);

    if (this.useListing)
    {
        this.genListing(x86.instrFormat(mnemonic,
                                        x86.regWidthSuffix(dest),
                                        dest,
                                        src));
    }
    return this;
};

/** Can be chained.
    @param {x86.Assembler#register or label } opnd1
    @param {Number} opnd2 optional and ignored when opnd1 is a register
*/
x86.Assembler.prototype.jmp = function (opnd1, opnd2)
{
    switch (opnd1.type)
    {
        case x86.type.REG:
            return this.jumpGeneral(4, opnd1);
        case x86.type.MEM:
            return this.jumpGeneral(4, opnd1);
        case x86.type.LINK:
            return this.jumpLink(x86.opcode.jmpRel32, "jmp", opnd1);
        case asm.type.LBL:
            return this.jumpLabel(x86.opcode.jmpRel8, "jmp", opnd1, opnd2);
        default:
            error("invalid operand type " + opnd1.type);
    }
};

/** Can be chained.
    @param {x86.Assembler#register or label } opnd1
    @param {Number} opnd2 optional and ignored when opnd1 is a register
*/
x86.Assembler.prototype.call = function (opnd1, opnd2)
{
    switch (opnd1.type)
    {
        case x86.type.REG:
            return this.jumpGeneral(2, opnd1);
        case x86.type.MEM:
            return this.jumpGeneral(2, opnd1);
        case x86.type.LINK:
            return this.jumpLink(x86.opcode.callRel32, "call", opnd1);
        case asm.type.LBL:
            return this.jumpLabel(x86.opcode.callRel32, "call", opnd1, opnd2);
        default:
            error("invalid operand type " + opnd1.type);
    }
};

/** Can be chained */
x86.Assembler.prototype.jo = function (label, offset)
{
    return this.jumpLabel(x86.opcode.joRel8, "jo", label, offset);
};

/** Can be chained */
x86.Assembler.prototype.jno = function (label, offset)
{
    return this.jumpLabel(x86.opcode.jnoRel8, "jno", label, offset);
};

/** Can be chained */
x86.Assembler.prototype.jb = function (label, offset)
{
    return this.jumpLabel(x86.opcode.jbRel8, "jb", label, offset);
};

/** Can be chained */
x86.Assembler.prototype.jae = function (label, offset)
{
    return this.jumpLabel(x86.opcode.jaeRel8, "jae", label, offset);
};

/** Can be chained */
x86.Assembler.prototype.je = function (label, offset)
{
    return this.jumpLabel(x86.opcode.jeRel8, "je", label, offset);
};

/** Can be chained */
x86.Assembler.prototype.jne = function (label, offset)
{
    return this.jumpLabel(x86.opcode.jneRel8, "jne", label, offset);
};

/** Can be chained */
x86.Assembler.prototype.jbe = function (label, offset)
{
    return this.jumpLabel(x86.opcode.jbeRel8, "jbe", label, offset);
};

/** Can be chained */
x86.Assembler.prototype.ja = function (label, offset)
{
    return this.jumpLabel(x86.opcode.jaRel8, "ja", label, offset);
};

/** Can be chained */
x86.Assembler.prototype.js = function (label, offset)
{
    return this.jumpLabel(x86.opcode.jsRel8, "js", label, offset);
};

/** Can be chained */
x86.Assembler.prototype.jns = function (label, offset)
{
    return this.jumpLabel(x86.opcode.jnsRel8, "jns", label, offset);
};

/** Can be chained */
x86.Assembler.prototype.jp = function (label, offset)
{
    return this.jumpLabel(x86.opcode.jpRel8, "jp", label, offset);
};

/** Can be chained */
x86.Assembler.prototype.jnp = function (label, offset)
{
    return this.jumpLabel(x86.opcode.jnpRel8, "jnp", label, offset);
};

/** Can be chained */
x86.Assembler.prototype.jl = function (label, offset)
{
    return this.jumpLabel(x86.opcode.jlRel8, "jl", label, offset);
};

/** Can be chained */
x86.Assembler.prototype.jge = function (label, offset)
{
    return this.jumpLabel(x86.opcode.jgeRel8, "jge", label, offset);
};

/** Can be chained */
x86.Assembler.prototype.jle = function (label, offset)
{
    return this.jumpLabel(x86.opcode.jleRel8, "jle", label, offset);
};

/** Can be chained */
x86.Assembler.prototype.jg = function (label, offset)
{
    return this.jumpLabel(x86.opcode.jgRel8, "jg", label, offset);
};

/** Can be chained */
x86.Assembler.prototype.cmovo = function (src, dest)
{
    return this.cmoveGeneral(x86.opcode.cmovo, "cmovo", src, dest);
};

/** Can be chained */
x86.Assembler.prototype.cmovno = function (src, dest)
{
    return this.cmoveGeneral(x86.opcode.cmovno, "cmovno", src, dest);
};

/** Can be chained */
x86.Assembler.prototype.cmovb  = function (src, dest)
{
    return this.cmoveGeneral(x86.opcode.cmovb, "cmovb", src, dest);
};

///** Can be chained */
//x86.Assembler.prototype.cmovc  = function (src, dest)
//{
//    return this.cmoveGeneral(x86.opcode.cmovc, "cmovc", src, dest);
//};

///** Can be chained */
//x86.Assembler.prototype.cmovnae= function (src, dest)
//{
//    return this.cmoveGeneral(x86.opcode.cmovna, "cmovna", src, dest);
//};

/** Can be chained */
x86.Assembler.prototype.cmovnb = function (src, dest)
{
    return this.cmoveGeneral(x86.opcode.cmovnb, "cmovnb", src, dest);
};

///** Can be chained */
//x86.Assembler.prototype.cmovnc = function (src, dest)
//{
//    return this.cmoveGeneral(x86.opcode.cmovnc, "cmovnc", src, dest);
//};

///** Can be chained */
//x86.Assembler.prototype.cmovae = function (src, dest)
//{
//    return this.cmoveGeneral(x86.opcode.cmovae, "cmovae", src, dest);
//};

/** Can be chained */
x86.Assembler.prototype.cmovz  = function (src, dest)
{
    return this.cmoveGeneral(x86.opcode.cmovz, "cmovz", src, dest);
};

///** Can be chained */
//x86.Assembler.prototype.cmove  = function (src, dest)
//{
//    return this.cmoveGeneral(x86.opcode.cmove, "cmove", src, dest);
//};

/** Can be chained */
x86.Assembler.prototype.cmovnz = function (src, dest)
{
    return this.cmoveGeneral(x86.opcode.cmovnz, "cmovnz", src, dest);
};

///** Can be chained */
//x86.Assembler.prototype.cmovne = function (src, dest)
//{
//    return this.cmoveGeneral(x86.opcode.cmovne, "cmovne", src, dest);
//};

/** Can be chained */
x86.Assembler.prototype.cmovbe = function (src, dest)
{
    return this.cmoveGeneral(x86.opcode.cmovbe, "cmovbe", src, dest);
};

///** Can be chained */
//x86.Assembler.prototype.cmovna = function (src, dest)
//{
//    return this.cmoveGeneral(x86.opcode.cmovna, "cmovna", src, dest);
//};

/** Can be chained */
x86.Assembler.prototype.cmovnbe= function (src, dest)
{
    return this.cmoveGeneral(x86.opcode.cmovnbe, "cmovnbe", src, dest);
};

///** Can be chained */
//x86.Assembler.prototype.cmova  = function (src, dest)
//{
//    return this.cmoveGeneral(x86.opcode.cmova, "cmova", src, dest);
//};

/** Can be chained */
x86.Assembler.prototype.cmovs  = function (src, dest)
{
    return this.cmoveGeneral(x86.opcode.cmovs, "cmovs", src, dest);
};

/** Can be chained */
x86.Assembler.prototype.cmovns = function (src, dest)
{
    return this.cmoveGeneral(x86.opcode.cmovns, "cmovns", src, dest);
};

/** Can be chained */
x86.Assembler.prototype.cmovp  = function (src, dest)
{
    return this.cmoveGeneral(x86.opcode.cmovp, "cmovp", src, dest);
};

///** Can be chained */
//x86.Assembler.prototype.cmovpe = function (src, dest)
//{
//    return this.cmoveGeneral(x86.opcode.cmovpe, "cmovpe", src, dest);
//};

/** Can be chained */
x86.Assembler.prototype.cmovnp = function (src, dest)
{
    return this.cmoveGeneral(x86.opcode.cmovnp, "cmovnp", src, dest);
};

///** Can be chained */
//x86.Assembler.prototype.cmovpo = function (src, dest)
//{
//    return this.cmoveGeneral(x86.opcode.cmovpo, "cmovpo", src, dest);
//};

/** Can be chained */
x86.Assembler.prototype.cmovl  = function (src, dest)
{
    return this.cmoveGeneral(x86.opcode.cmovl, "cmovl", src, dest);
};

///** Can be chained */
//x86.Assembler.prototype.cmovnge= function (src, dest)
//{
//    return this.cmoveGeneral(x86.opcode.cmovng, "cmovng", src, dest);
//};

/** Can be chained */
x86.Assembler.prototype.cmovnl = function (src, dest)
{
    return this.cmoveGeneral(x86.opcode.cmovnl, "cmovnl", src, dest);
};

///** Can be chained */
//x86.Assembler.prototype.cmovge = function (src, dest)
//{
//    return this.cmoveGeneral(x86.opcode.cmovge, "cmovge", src, dest);
//};

/** Can be chained */
x86.Assembler.prototype.cmovle = function (src, dest)
{
    return this.cmoveGeneral(x86.opcode.cmovle, "cmovle", src, dest);
};

///** Can be chained */
//x86.Assembler.prototype.cmovng = function (src, dest)
//{
//    return this.cmoveGeneral(x86.opcode.cmovng, "cmovng", src, dest);
//};

/** Can be chained */
x86.Assembler.prototype.cmovnle= function (src, dest)
{
    return this.cmoveGeneral(x86.opcode.cmovnle, "cmovnle", src, dest);
};

///** Can be chained */
//x86.Assembler.prototype.cmovg  = function (src, dest)
//{
//    return this.cmoveGeneral(x86.opcode.cmovg, "cmovg", src, dest);
//};

/** @private */
x86.Assembler.prototype.shift =
function (opcodeExt, mnemonic, src, dest, width)
{
    assert(
        src.type === x86.type.IMM_VAL || src === this.register.cl,
        "'src' argument must be an immediate value or the cl register " +
        "instead of " + src
    );

    assert(
        (dest.type === x86.type.REG) ?
        (!width || (dest.width() === width)) : width,
        "missing or inconsistent operand width " + width
    );

    width = (dest.type === x86.type.REG) ? dest.width() : width;

    var k = src.value;

    var opcode = 0xc0 +                    // base opcode
                 ((width === 8) ? 0 : 1) +
                 ((src.type === x86.type.REG) ? 2 : 0) + 
                 ((k === 1 || src.type === x86.type.REG) ? 0x10 : 0);

    this.opndPrefixOpnd(width, dest);
    this.gen8(opcode);
    this.opndModRMSIB(opcodeExt, dest);

    if (k > 1 && src.type === x86.type.IMM_VAL)
    {
        this.gen8(k);
    }

    if (this.useListing)
    {
        var srcLst = 
        this.genListing(
            x86.instrFormat(
                mnemonic,
                x86.widthSuffix(width),
                dest,
                (src.type === x86.type.IMM_VAL) ? this.immediateValue(k) : src
            )
        );
    }

    return this;
};

/** Can be chained */
x86.Assembler.prototype.sal = function (src, dest, width)
{
    return this.shift(4, "sal", src, dest, width);
};

/** Can be chained */
x86.Assembler.prototype.sar = function (src, dest, width)
{
    return this.shift(7, "sar", src, dest, width);
};

/** Can be chained */
x86.Assembler.prototype.shl = x86.Assembler.prototype.sal;

/** Can be chained */
x86.Assembler.prototype.shr = function (src, dest, width)
{
    return this.shift(5, "shr", src, dest, width);
};

/** Can be chained */
x86.Assembler.prototype.rol = function (src, dest, width)
{
    return this.shift(0, "rol", src, dest, width);
};

/** Can be chained */
x86.Assembler.prototype.ror = function (src, dest, width)
{
    return this.shift(1, "ror", src, dest, width);
};

/** @private */
x86.Assembler.prototype.oneOpnd =
function (opcode, opcodeExt, mnemonic, dest, width)
{
    assert(
        (dest.type === x86.type.REG) ?
        (!width || (dest.width() === width)) : width,
        "missing or inconsistent operand width " + width
    );

    width = (dest.type === x86.type.REG) ? dest.width() : width;

    var opcode = opcode +                    // base opcode
                 ((width === 8) ? 0 : 1);

    this.opndPrefixOpnd(width, dest);
    this.gen8(opcode);
    this.opndModRMSIB(opcodeExt, dest);

    if (this.useListing)
    {
        this.genListing(x86.instrFormat(mnemonic,
                                        x86.widthSuffix(width),
                                        dest));
    }

    return this;
};

/** Can be chained */
x86.Assembler.prototype.not = function (dest, width)
{
    return this.oneOpnd(0xf6, 2, "not", dest, width);
};

/** Can be chained */
x86.Assembler.prototype.mul = function (src, width)
{
    return this.oneOpnd(0xf6, 4, "mul", src, width);
};

/** Can be chained */
x86.Assembler.prototype.imul = function (src, dst, imm, width)
{
    /*
    Multiple possible encodings for imul:

    /7:
    Multiply reg EAX/RAX by specified src reg/mem, put result in EDX,EAX/RDX,RAX

    /r:
    Multiply src with reg/mem with dest reg, put result in dest reg

    /r ib, /r iw, /r id:
    Multiply src reg/mem with imm value, put result in dest reg
    */

    const that = this;

    /** @ignore Generate the opcode and the ModRM encoding */
    function genOp(opb1, opb2, genConst)
    {
        // For /r:
        that.opndPrefixRegOpnd(dst, src);

        // Generate the opcode
        that.gen8(opb1);
        if (opb2) that.gen8(opb2);

        // For /r:
        that.opndModRMSIBRegOpnd(dst, src);

        // Generate the immediate value
        if (imm)
            genConst();

        if (that.useListing)
        {
            that.genListing(
                x86.instrFormat(
                    'imul',
                    x86.regWidthSuffix(dst),
                    src,
                    dst
                )
                +
                (imm? (',' + x86.opndFormatGNU(imm)):'')
            );
        }
    }

    // If both a source and destination were specified
    if (src && dst)
    {
        // If an immediate value was specified
        if (imm)
        {
            assert (
                imm.type === x86.type.IMM_VAL,
                'immediate value must have immediate type'
            );

            // If the immediate value fits in an int8
            if (num_ge(imm.value, getIntMin(8)) &&
                num_le(imm.value, getIntMax(8)))
            {
                // Generate the operation
                genOp(0x6b, undefined, function () { that.gen8(imm.value); });
            }

            // If the immediate value fits in an int32
            else if (num_ge(imm.value, getIntMin(32)) &&
                     num_le(imm.value, getIntMax(32)))
            {
                // Generate the operation
                genOp(0x69, undefined, function () { that.gen32(imm.value); });
            }

            else
            {
                assert (
                    false,
                    'immediate value does not fit in int32'
                );
            }
        }
        else
        {
            // Generate the opcode with no immediate value
            genOp(0x0f, 0xaf);
        }
    }

    // Otherwise, if only a source reg/mem location was specified
    else
    {
        assert (
            src,
            'imul requires a source operand'
        );

        // For /5:
        return this.oneOpnd(0xf6, 5, "imul", src, width);
    }
};

/** Can be chained */
x86.Assembler.prototype.cdq = function ()
{
    this.gen8(0x99);

    if (this.useListing)
    {
        this.genListing(x86.instrFormat('cdq'));
    }

    return this;
};

/** Can be chained */
x86.Assembler.prototype.cqo = function ()
{
    this.gen8(0x48);
    this.gen8(0x99);

    if (this.useListing)
    {
        this.genListing(x86.instrFormat('cqo'));
    }

    return this;
};

/** Can be chained */
x86.Assembler.prototype.div = function (src, width)
{
    return this.oneOpnd(0xf6, 6, "div", src, width);
};

/** Can be chained */
x86.Assembler.prototype.idiv = function (src, width)
{
    return this.oneOpnd(0xf6, 7, "idiv", src, width);
};

/** Can be chained */
x86.Assembler.prototype.xchg = function (src, dst, width)
{
    width = src.width ? src.width() : (dst.width ? dst.width() : width);

    assert (
        width && (width === 32 || width === 64),
        'only 32 and 64 bit encodings supported'
    );

    var that = this;

    /** @ignore special case for swapping with EAX/RAX */
    function genOp(dst)
    {
        that.opndPrefixOpnd(width, dst);

        // opcode = 0x90 + register indicator
        that.gen8(0x90 + (7 & dst.field()));

        if (that.useListing)
        {
            that.genListing(
                x86.instrFormat(
                    'xchg',
                    x86.regWidthSuffix(dst),
                    src,
                    (width === 32)? that.register.eax:that.register.rax
                )
            );
        }
    }

    // If swapping with EAX/RAX
    if (src.field() === 0)
    {
        genOp(dst);
    }
    else if (dst.field() === 0)
    {
        genOp(src);
    }

    // General case, /r encoding
    else
    {
        this.opndPrefixRegOpnd(dst, src);
        this.gen8(0x87);
        this.opndModRMSIBRegOpnd(dst, src);

        if (that.useListing)
        {
            this.genListing(
                x86.instrFormat(
                    'xchg',
                    x86.regWidthSuffix(dst),
                    src,
                    dst
                )
            );
        }
    }

    return this;
};



