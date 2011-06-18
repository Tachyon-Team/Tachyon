/**
x86 namespace
*/
var x86 = x86 || {};

/**
@class Base class for x86 operands
*/
x86.Operand = function()
{
    /**
    @field Operand size in bits
    */
    this.size = 32;

    /**
    @field Indicates a REX prefix is needed to encode this operand.
    */
    this.rexNeeded = false;

    /**
    @field Indicates this operand is available with a REX prefix
    */
    this.rexAvail = true;
}

/**
@class Register operand
@extends x86.Operand
*/
x86.Register = function (name, regNo, size, lowReg, highReg, rexNeeded, rexAvail)
{
    if (rexNeeded === undefined)
        rexNeeded = false;

    if (rexAvail === undefined)
        rexAvail = true;

    assert (
        isNonNegInt(regNo) && regNo <= 15,
        'invalid register number'
    );

    assert (
        lowReg === undefined || lowReg instanceof x86.Register,
        'invalid low register'
    );

    this.name = name;

    this.regNo = regNo;

    this.size = size;

    this.lowReg = lowReg;

    this.highReg = highReg;

    this.rexNeeded = rexNeeded;

    this.rexAvail = rexAvail;
}
x86.Register.prototype = new x86.Operand();

/**
Produce a string representation of the register
*/
x86.Register.prototype.toString = function ()
{
    return this.name;
}

/**
@class Memory location operand
@extends x86.Operand
*/
x86.MemLoc = function (size, base, disp, index, scale)
{
    if (scale === undefined)
        scale = 1;

    assert (
        base === undefined || base instanceof x86.Register,
        'invalid SIB base'
    );

    assert (
        isInt(disp) && disp >= getIntMin(32) && disp <= getIntMax(32),
        'invalid mem loc disp'
    );

    assert (
        scale === 1 || scale === 2 || scale === 4 || scale === 8,
        'invalid SIB scale'
    );

    assert (
        base === undefined || base instanceof x86.Register,
        'invalid SIB index'
    );
    
    this.size = size;

    this.base = base;

    this.disp = disp;

    this.index = index;

    this.scale = scale;

    // Test if the rex prefix is needed
    this.rexNeeded = (base.rexNeeded || (index && index.rexNeeded));

    // Test whether or not an SIB byte is needed
    this.sibNeeded = (
        index || 
        scale != 1 ||
        base === x86.regs.esp ||
        base === x86.regs.rsp ||
        base === x86.regs.r12
    );

    // Required displacement size
    var dispSize = 0;

    // If ESP or RBP or R13 is used as the base, a displacement must be encoded
    if (base === x86.regs.ebp ||
        base === x86.regs.rbp || 
        base === x86.regs.r13)
        dispSize = 8;

    // Compute the required displacement size
    if (num_ne(disp, 0))
    {
        if (num_ge(disp, getIntMin(8)) && num_le(disp, getIntMax(8)))
            dispSize = 8;
        else if (num_ge(disp, getIntMin(16)) && num_le(disp, getIntMax(16)))
            dispSize = 16;
        else if (num_ge(disp, getIntMin(32)) && num_le(disp, getIntMax(32)))
            dispSize = 32;
    }

    this.dispSize = dispSize;
}
x86.MemLoc.prototype = new x86.Operand();

/**
@class Immediate operand
@extends x86.Operand
*/
x86.Immediate = function (value)
{
    /**
    @field Field value
    */
    this.value = value;

    // Compute the smallest size this immediate fits in
    if (num_ge(value, getIntMin(8)) && num_le(value, getIntMax(8)))
        this.size = 8;
    else if (num_ge(value, getIntMin(16)) && num_le(value, getIntMax(16)))
        this.size = 16;
    else if (num_ge(value, getIntMin(32)) && num_le(value, getIntMax(32)))
        this.size = 32;
}
x86.Immediate.prototype = new x86.Operand();

/**
Produce a string representation of the immediate
*/
x86.Immediate.prototype.toString = function ()
{
    return String(this.value);
}

/**
Write the immediate value into a code block
*/
x86.Immediate.prototype.writeImm = function (codeBlock, immSize)
{
    assert (
        this.size <= immSize,
        'immediate size too small'
    );

    if (immSize === 8)
        codeBlock.writeByte(this.value);
    if (immSize === 16)
        codeBlock.writeWord(this.value);

    //
    // TODO: 32-bit constants
    //
}

/**
@class Label reference operand
@extends x86.Operand
*/
x86.LabelRef = function (label)
{
    assert (
        label instanceof x86.Label,
        'invalid label object'
    );

    /**
    @field Label object
    */
    this.label = label

    // Initially set the size to 32
    this.size = 32;
}
x86.LabelRef.prototype = new x86.Operand();

/**
Namespace of supported x86 registers
*/
x86.regs = {};

// General-purpose registers
x86.regs.al     = new x86.Register('ax', 0, 8);
x86.regs.cl     = new x86.Register('cx', 1, 8);
x86.regs.dl     = new x86.Register('dx', 2, 8);
x86.regs.bl     = new x86.Register('bx', 3, 8);
x86.regs.ah     = new x86.Register('ah', 4, 8, undefined, undefined, false, false);
x86.regs.ch     = new x86.Register('ch', 5, 8, undefined, undefined, false, false);
x86.regs.dh     = new x86.Register('dh', 6, 8, undefined, undefined, false, false);
x86.regs.bh     = new x86.Register('bh', 7, 8, undefined, undefined, false, false);
x86.regs.spl    = new x86.Register('spl', 4, 8, undefined, undefined, true);
x86.regs.bpl    = new x86.Register('bpl', 5, 8, undefined, undefined, true);
x86.regs.sil    = new x86.Register('sil', 6, 8, undefined, undefined, true);
x86.regs.dil    = new x86.Register('dil', 7, 8, undefined, undefined, true);
x86.regs.ax     = new x86.Register('ax', 0, 16, x86.regs.al, x86.regs.ah);
x86.regs.cx     = new x86.Register('cx', 1, 16, x86.regs.cl, x86.regs.ch);
x86.regs.dx     = new x86.Register('dx', 2, 16, x86.regs.dl, x86.regs.dh);
x86.regs.bx     = new x86.Register('bx', 3, 16, x86.regs.bl, x86.regs.bh);
x86.regs.sp     = new x86.Register('sp', 4, 16, x86.regs.spl);
x86.regs.bp     = new x86.Register('bp', 5, 16, x86.regs.bpl);
x86.regs.si     = new x86.Register('si', 6, 16, x86.regs.sil);
x86.regs.di     = new x86.Register('di', 7, 16, x86.regs.dil);
x86.regs.eax    = new x86.Register('eax', 0, 32, x86.regs.ax);
x86.regs.ecx    = new x86.Register('ecx', 1, 32, x86.regs.cx);
x86.regs.edx    = new x86.Register('edx', 2, 32, x86.regs.dx);
x86.regs.ebx    = new x86.Register('ebx', 3, 32, x86.regs.bx);
x86.regs.esp    = new x86.Register('esp', 4, 32, x86.regs.sp);
x86.regs.ebp    = new x86.Register('ebp', 5, 32, x86.regs.bp);
x86.regs.esi    = new x86.Register('esi', 6, 32, x86.regs.si);
x86.regs.edi    = new x86.Register('edi', 7, 32, x86.regs.di);
x86.regs.r8l    = new x86.Register('r8l', 8, 8, undefined, true);
x86.regs.r9l    = new x86.Register('r9l', 9, 8, undefined, true);
x86.regs.r10l   = new x86.Register('r10l', 10, 8, undefined, true);
x86.regs.r11l   = new x86.Register('r11l', 11, 8, undefined, true);
x86.regs.r12l   = new x86.Register('r12l', 12, 8, undefined, true);
x86.regs.r13l   = new x86.Register('r13l', 13, 8, undefined, true);
x86.regs.r14l   = new x86.Register('r14l', 14, 8, undefined, true);
x86.regs.r15l   = new x86.Register('r15l', 15, 8, undefined, true);
x86.regs.r8w    = new x86.Register('r8w', 8, 16, x86.regs.r8l, undefined, true);
x86.regs.r9w    = new x86.Register('r9w', 9, 16, x86.regs.r9l, undefined, true);
x86.regs.r10w   = new x86.Register('r10w', 10, 16, x86.regs.r10l, undefined, true);
x86.regs.r11w   = new x86.Register('r11w', 11, 16, x86.regs.r11l, undefined, true);
x86.regs.r12w   = new x86.Register('r12w', 12, 16, x86.regs.r12l, undefined, true);
x86.regs.r13w   = new x86.Register('r13w', 13, 16, x86.regs.r13l, undefined, true);
x86.regs.r14w   = new x86.Register('r14w', 14, 16, x86.regs.r14l, undefined, true);
x86.regs.r15w   = new x86.Register('r15w', 15, 16, x86.regs.r15l, undefined, true);
x86.regs.r8d    = new x86.Register('r8d', 8, 32, x86.regs.r8w, undefined, true);
x86.regs.r9d    = new x86.Register('r9d', 9, 32, x86.regs.r9w, undefined, true);
x86.regs.r10d   = new x86.Register('r10d', 10, 32, x86.regs.r10w, undefined, true);
x86.regs.r11d   = new x86.Register('r11d', 11, 32, x86.regs.r11w, undefined, true);
x86.regs.r12d   = new x86.Register('r12d', 12, 32, x86.regs.r12w, undefined, true);
x86.regs.r13d   = new x86.Register('r13d', 13, 32, x86.regs.r13w, undefined, true);
x86.regs.r14d   = new x86.Register('r14d', 14, 32, x86.regs.r14w, undefined, true);
x86.regs.r15d   = new x86.Register('r15d', 15, 32, x86.regs.r15w, undefined, true);
x86.regs.rax    = new x86.Register('rax', 0, 64, x86.regs.eax, undefined, true);
x86.regs.rcx    = new x86.Register('rcx', 1, 64, x86.regs.ecx, undefined, true);
x86.regs.rdx    = new x86.Register('rdx', 2, 64, x86.regs.edx, undefined, true);
x86.regs.rbx    = new x86.Register('rbx', 3, 64, x86.regs.ebx, undefined, true);
x86.regs.rsp    = new x86.Register('rsp', 4, 64, x86.regs.esp, undefined, true);
x86.regs.rbp    = new x86.Register('rbp', 5, 64, x86.regs.ebp, undefined, true);
x86.regs.rsi    = new x86.Register('rsi', 6, 64, x86.regs.esi, undefined, true);
x86.regs.rdi    = new x86.Register('rdi', 7, 64, x86.regs.edi, undefined, true);
x86.regs.r8     = new x86.Register('r8', 8, 64, x86.regs.r8d, undefined, true);
x86.regs.r9     = new x86.Register('r9', 9, 64, x86.regs.r9d, undefined, true);
x86.regs.r10    = new x86.Register('r10', 10, 64, x86.regs.r10d, undefined, true);
x86.regs.r11    = new x86.Register('r11', 11, 64, x86.regs.r11d, undefined, true);
x86.regs.r12    = new x86.Register('r12', 12, 64, x86.regs.r12d, undefined, true);
x86.regs.r13    = new x86.Register('r13', 13, 64, x86.regs.r13d, undefined, true);
x86.regs.r14    = new x86.Register('r14', 14, 64, x86.regs.r14d, undefined, true);
x86.regs.r15    = new x86.Register('r15', 15, 64, x86.regs.r15d, undefined, true);

// Instruction pointer, for RIP addressing
x86.regs.rip    = new x86.Register('rip', 13, 64, undefined, undefined, true);

