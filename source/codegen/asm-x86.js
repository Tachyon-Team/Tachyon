/**
@fileOverview
Assembler for x86 machine code. Translates assembly code written
in a AT&T inspired syntax to binary code ready for execution.

The following code was inspired by the Gambit code 
generator written in Scheme.

@copyright
Copyright (c) 2010 Tachyon Javascript Engine, All Rights Reserved
*/

/** @namespace x86 code generator */
var x86 = x86 || {};

/** @namespace potential x86 targets */
x86.target = {};

/** x86 32 bit target */
x86.target.x86    = 0; 

/** x86 64 bit target */
x86.target.x86_64 = 1;

/** Throw an exception with message and args */
x86.error = function (message)
{
    var err = message;
    for (var i=1; i<arguments.length; i++)
    {
        err += arguments[i];
    }
    throw "x86Error: " + err;
};

/** Ensure a boolean condition is met, otherwise throw an exception */
x86.assert = function (bool, message)
{
    if (!bool) 
    { 
        x86.error.apply(null, Array.prototype.slice.call(arguments, 1));
    } 
};

/** @namespace */
x86.type = {};
/** Generic x86 object */
x86.type.OBJ = "X86_OBJ";
/** Immediate value */
x86.type.IMM_VAL = "X86_IMMEDIATE_VALUE";
/** Immediate label */
x86.type.IMM_LBL = "X86_IMMEDIATE_LABEL"; 
/** Register */
x86.type.REG = "X86_REGISTER"; 
/** Memory access */
x86.type.MEM = "X86_MEMORY";

// TODO: Clean if not useful
/** @private Might not be useful */
x86.type.GLO = "X86_GLOBAL";

/** @private test if num is a valid 8 bit signed value */
x86.isSigned8 = function (num) { return (num >= -128 && num <= 127); };

/** @private test if num is a valid 32 bit signed value */
x86.isSigned32 = function (num) 
{ 
    return (num >= -2147483648 && num <= 2147483647); 
};

/** 
    Returns a new assembler object.

    @class Assembler for x86 code 
    @param {x86.target} target optional x86.target value, 
                        defaults to x86.target.x86
*/
x86.Assembler = function (target)
{
    /** Flag for listing output */
    this.useListing = true;

    /** Generic underlying assembler object  */
    this.codeBlock  = new asm.CodeBlock(0, false, this.useListing);

    /** @private Current target for compilation,
        should not be modified once the object is constructed */
    // TODO: refactor to have a setter throw an exception once
    // getter and setter are supported
    this.target     = target || x86.target.x86; 
};

/** Returns whether the current compilation target is x86_64 */ 
x86.Assembler.prototype.is64bitMode = function () 
{
    return this.target === x86.target.x86_64;
};

/** @private */
x86.Assembler.prototype.assert64bitMode = function ()
{
    x86.assert(this.is64bitMode(), 
               "instruction only valid for x86-64");
};

/** @private */
x86.Assembler.prototype.assert32bitMode = function ()
{
    x86.assert(!this.is64bitMode(), "instruction only valid for x86");
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

/** @private */
x86.Assembler.prototype._genImmNum = function (k, width)
{
    // TODO: Find out what behavior should the signed-lo have
    /** @ignore */
    function signedLo(n, k) 
    {
        return (k & (Math.pow(2,n) - 1));
    };

    if (width === 8) 
    {
        this.gen8(signedLo(8,k));   
    } else if (width === 16) 
    {
        this.gen16(signedLo(16,k));
    } else if (width === 32)
    {
        this.gen32(signedLo(32,k));
    }
    else 
    {
        this.gen64(signedLo(64,k));
    }   
    return this;
};

/** Adds an immediate number to the code stream. Can be chained. 
    @param {Number} k
    @param {Number} width Width of the value in number of bits. 
                          Minimum of 32. Defaults to 32.
*/
x86.Assembler.prototype.genImmNum = function (k, width)
{
    this._genImmNum(k, Math.min(32, width || 32));
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
    x86.assert((value === undefined) || typeof value === "number",
               "'value' argument should be a number");

    var that = Object.create(x86.Assembler.prototype.immediateValue.prototype);

    /** @private */
    that.value = value || 0;

    return that;
};
x86.Assembler.prototype.immediateValue.prototype = x86.Assembler.obj();
/** x86 object type */
x86.Assembler.prototype.immediateValue.prototype.type  = x86.type.IMM_VAL;

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
x86.Assembler.prototype.memory = function ( disp, base, index, scale )
{
    x86.assert((disp === undefined)  || typeof disp === "number",
               "'disp' argument should be a number");
    x86.assert((base === undefined)  || base.type === x86.type.REG,
               "'base' argument should be a register");
    x86.assert((index === undefined) || index.type === x86.type.REG,
               "'index' argument should be a register");
    x86.assert((scale === undefined) || typeof scale === "number",
               "'scale' argument should be a number");

    var that = Object.create(x86.Assembler.prototype.memory.prototype);

    /** @private */
    that.disp  = disp  || 0;
    /** @private */
    that.base  = base  || null;
    /** @private */
    that.index = index || null;
    /** @private */
    that.scale = scale || 0;
    
    return that;
};
x86.Assembler.prototype.memory.prototype        = x86.Assembler.obj();
x86.Assembler.prototype.memory.prototype.type   = x86.type.MEM;

/** @private Undocumented until found useful */
x86.Assembler.prototype.global = function ( name, offset )
{
    var that = Object.create(x86.Assembler.prototype.global.prototype);
    if (name)   { that.name   = name; };
    if (offset) { that.offset = offset; };
    return that;
};
x86.Assembler.prototype.global.prototype = x86.Assembler.obj();
x86.Assembler.prototype.global.prototype.type   = x86.type.GLO;
x86.Assembler.prototype.global.prototype.name   = null;
x86.Assembler.prototype.global.prototype.offset = 0;

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
    x86.assert(name  !== undefined, 
               "'name' argument not supplied");
    x86.assert(value !== undefined && typeof value === "number", 
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
    return this.value & 0xF 
};
/** returns the width value of the register */
x86.Assembler.prototype.register.prototype.width    = function () 
{
    if      (this.value < 16) { return 64;} 
    else if (this.value < 32) { return 32;}
    else if (this.value < 48) { return 16;}
    else if (this.value < 64) { return 80;}
    else if (this.value < 80) { return 128;}
    else                      { return 8;}
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
    switch(opnd.type)
    {
        case x86.type.IMM_VAL: 
            return "$" + String(opnd.value);
        case x86.type.IMM_LBL: 
            // TODO: Implement Immediate Label formatting
            x86.error("opndFormatGNU: Immediate label formatting" +
                      " unimplemented");
        case x86.type.REG:
            return "%" + opnd.name;
        case x86.type.MEM:
            if (opnd.base !== null)
            {
                return ((opnd.disp === 0) ? "" : opnd.disp) + "(" +
                        x86.opndFormatGNU(opnd.base) + 
                       ((opnd.index === null) ? 
                            "" : ", " + x86.opndFormatGNU(opnd.index)) +
                       ((opnd.index === null || opnd.scale === 0) ? 
                            "" : opnd.scale.toString()) +
                        ")"; 
            } else 
            {
                return x86.offsetToString(opnd.disp);
            }
        case x86.type.GLO:
            return opnd.name + x86.offsetToString(opnd.offset);
        default:
            return opnd.toString();
    }
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

        case x86.type.GLO:
            break;

        case x86.type.MEM:
            const base = opnd.base; 
            if(base) 
            {
                x86.assert((base.isr32() || (base.isr64() && 
                            this.is64bitMode())),
                           "invalid width base register",
                           base); 
                // If needed emit REX.B (Extension of the ModR/M r/m field,
                // SIB base field, or Opcode reg field
                rex += (base.field() >> 3);
                
                const index = opnd.index;
                if(index) 
                {
                    x86.assert((base.isr32() ? index.isr32() : index.isr64()),
                           "index register must have the"+
                           " same width as base",
                           reg2);        
                    rex += ((index.field() >> 3) << 1);
                }
            }
            break;

        default:
            x86.error("unknown operand", opnd);
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

        case x86.type.GLO:
            // TODO: Remove if not needed
            x86.error("unimplemented for opnd of type global");
            break;

        case x86.type.MEM:
            const base  = opnd.base;
            const baseFieldLo = 7 & base.field();
            const index = opnd.index;
            const disp  = opnd.disp;
            const scale = opnd.scale;
            
            if (base) 
            {
                
                // index: Need a SIB when using an index
                // baseFieldLo: register or base = RSP/R12 
                if (index || (baseFieldLo === 4))
                {
                    // SIB Needed
                    modrm = modrm_rf + 4;
                    var sib   = baseFieldLo;
                    
                    if (index)
                    {
                        x86.assert(!(index.field() === 4),
                               "SP not allowed as index", index);
                        sib += ((7 & index.field()) << 3) +
                               (scale << 6);
                    } else // !index
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
            } else // (!base)
            {
                // Absolute address, use disp32 ModR/M 
                absAddr();
                this.gen32(disp);
            }
            break;

        default:
            x86.error("opndModRMSIB: unkown operand", opnd);
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
        x86.assert(((width === opnd.width()) || 
                    reg.isxmm()),   // for cvtsi2ss/cvtsi2sd instructions
                   "registers are not of the same width",
                   reg,opnd);

        isRex = this.opndPrefix(width, field, opnd, (isExtLo8 || isExtLo8Reg2));

        x86.assert(!(isRex && (reg.isr8h() || opnd.isr8h())),
                   "cannot use high 8 bit register here", 
                   reg, opnd);
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
        const isExtLo8 = ((width === 8) && (field >= 4) && (!reg.isr8h()));
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
    const k = src.value;

    /** @ignore Adds the listing for the instruction */
    function listing(width,n)
    {
        if (that.useListing) 
        {
            that.genListing(x86.instrFormat(mnemonic, 
                                             x86.widthSuffix(width),
                                             dest,
                                             src));
        }
    }

    /** 
        @ignore 
        Generate encoding when the targeted register is AL or AX/EAX/RAX
    */
    function accumulator (width)
    {
        that.
        opndSizeOverridePrefix(width).
        // opcode = #x04, #x0c, #x14, ..., #x3c (for AL)
        //       or #x05, #x0d, #x15, ..., #x3d (for AX/EAX/RAX)
        gen8(((width === 8) ? 0x04 : 0x05) + (op << 3));
        listing(width, that.genImmNum(k,width));
    }

    /**
        @ignore
        General case
    */
    function general(width)
    {
        that.opndPrefixOpnd(width, dest);

        if (width === 8) 
        {
            that.
            gen8(0x80).            // opcode = 8 bit operation
            opndModRMSIB(op,dest); // ModR/M
            listing(width, that.genImmNum(k,8));
        } else if (x86.isSigned8(k))
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
            listing(width, that.genImmNum(k,width));
        }
    }

    x86.assert((dest.type === x86.type.REG) ? 
            (!width || (dest.width() === width)) : width,
            "missing or inconsistent operand width", width);

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
    const k = src.value;

    /** @ignore generate listing */
    function listing(width,n)
    {
        if (that.useListing) 
        {
            that.genListing(x86.instrFormat("mov", 
                                             x86.widthSuffix(width),
                                             dest,
                                             src));
        }
    }

    /** @ignore special case when the destination is a register */
    function register(width)
    {
        that.
        opndPrefixOpnd(width, dest); // prefix
        // opcode = #xb0-#xb7 (for 8 bit registers)
        //      or #xb8-#xbf (for 16/32/64 bit registers)
        that.
        gen8( ((width === 8) ? 0xb0 : 0xb8) + (7 & dest.field()) );
        listing(width, that.genImmNum(k, width));
    }

    /** @ignore general case */
    function general(width)
    {
        that.opndPrefixOpnd(width, dest);
        that.
        gen8((width === 8) ? 0xc6 : 0xc7).  // opcode
        opndModRMSIB(0,dest); // ModR/M
        listing(width, that.genImmNum(k,width));
    }

    x86.assert((dest.type === x86.type.REG) ? 
            (!width || (dest.width() === width)) : width,
            "missing or inconsistent operand width '", width, "'");

    if (dest.type === x86.type.REG)
    {
        if (dest.width() === 64 &&
            x86.isSigned32(k))
        {
            general(dest.width());
        } else 
        {
            register(dest.width());
        }
    } else // dest.type !== x86.type.REG
    {
        general(width);
    }
    return this;
};

/** @private generic two operands instruction encoding */
x86.Assembler.prototype.op    = function (op, mnemonic, dest, src, width) 
{
    // TODO: Add support for immediate label, see x86-mov 
    const that = this;

    x86.assert( dest.type === x86.type.REG ||
            dest.type === x86.type.MEM ||
            src.type  === x86.type.REG ||
            src.type  === x86.type.MEM,
            "one of dest or src should be a register or" +
            " a memory location");

    x86.assert(!(dest.type === x86.type.MEM &&
             src.type  === x86.type.MEM),
             "dest and src cannot refer both to a " +
             "memory location"); 

    /** @ignore generate the instruction */
    function genOp(reg, opnd, isSwapped)
    {
        x86.assert(!width || (reg.width() === width),
               "inconsistent operand width '",width, 
               "' and register width '", reg.width(), "'");
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

    if (src.type === x86.type.IMM_VAL)
    {
        if (op === 17)
        {
            this.movImm(dest, src, width);
        } else
        {
            this.opImm(op, mnemonic, src, dest, width);
        }
    } else if (src.type === x86.type.REG)
    {
        genOp(src, dest, true);
    } else if (dest.type === x86.type.REG)
    {
        genOp(dest, src, false);
    } else 
    {
        x86.error("op: invalid operand combination", dest, src);
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
                                             dest));
        }
    }

    if (x86.isSigned8(k))
    {
        this.gen8(0x6a); // opcode
        listing(this._genImmNum(k, 8));
    } else
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
            x86.assert(opnd.field() < 8, 
                   "cannot push/pop extended register" +
                   " in 32 bit mode");
        } else
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
        that.opndModRMSIB((isPop? 0 : 6), opnd);
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

/** Adds a label to the code stream. Can be chained. */
x86.Assembler.prototype.label = function (lbl)
{ 
    x86.assert(lbl.type === asm.type.LBL,
               "invalid label", lbl);
    this.
    genListing(x86.labelFormat(lbl)).
    codeBlock.genLabel(lbl); 
    return this;
};

/** @private Generic jump to label instruction encoding */
x86.Assembler.prototype.jumpLabel = function (opcode, mnemonic, label, offset)
{
    const that = this;
    var offset = offset || 0;

    x86.assert(label.type === asm.type.LBL,
           "invalid label '", label, "'");

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

/** @private jump for the general case */
x86.Assembler.prototype.jumpGeneral = function (field, opnd)
{
    x86.assert(!(opnd.type === x86.type.REG) || 
           (this.is64bitMode() ? opnd.isr64() : opnd.isr32()),
           "invalid width register", opnd);  

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
x86.Assembler.prototype.ret = function ()  
{ 
    return this.noOpndInstr(0xc3, "ret");
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
    return this.op(0, "add",dest,src,width); }
;
/** Can be chained. 
    @param src
    @param dest
    @param {Number} width optional 
*/
x86.Assembler.prototype.or  = function (src, dest, width) 
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
/** Can be chained. 
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


/** Can be chained. */
x86.Assembler.prototype.push = function (opnd) 
{ 
    return this.pushPop(opnd, false); 
};
/** Can be chained. */
x86.Assembler.prototype.pop  = function (opnd) { return this.pushPop(opnd, true); };


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
        case asm.type.LBL:
            return this.jumpLabel(x86.opcode.jmpRel8, "jmp", opnd1, opnd2);
        default:
            x86.error("jmp: invalid operand type", opnd1.type); 
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
        case asm.type.LBL:
            return this.jumpLabel(x86.opcode.callRel32, "call",opnd1, opnd2);
        default:
            x86.error("call: invalid operand type", opnd1.type); 
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



