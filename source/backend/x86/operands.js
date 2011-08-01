/* _________________________________________________________________________
 *
 *             Tachyon : A Self-Hosted JavaScript Virtual Machine
 *
 *
 *  This file is part of the Tachyon JavaScript project. Tachyon is
 *  distributed at:
 *  http://github.com/Tachyon-Team/Tachyon
 *
 *
 *  Copyright (c) 2011, Universite de Montreal
 *  All rights reserved.
 *
 *  This software is licensed under the following license (Modified BSD
 *  License):
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions are
 *  met:
 *    * Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *    * Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions and the following disclaimer in the
 *      documentation and/or other materials provided with the distribution.
 *    * Neither the name of the Universite de Montreal nor the names of its
 *      contributors may be used to endorse or promote products derived
 *      from this software without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 *  IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 *  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 *  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL UNIVERSITE DE
 *  MONTREAL BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 *  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 *  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 *  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 *  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 *  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 *  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * _________________________________________________________________________
 */

/**
@fileOverview
Representation of x86 instruction operands, including registers,
memory locations, label references and immediate values.

@author
Maxime Chevalier-Boisvert
*/

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
x86.Register = function (name, type, regNo, size, lowReg, highReg, rexNeeded, rexAvail)
{
    if (rexNeeded === undefined)
        rexNeeded = false;

    if (rexAvail === undefined)
        rexAvail = true;

    assert (
        type === 'gp' ||
        type === 'ip' ||
        type === 'fp' ||
        type === 'xmm',
        'invalid register type'
    );

    assert (
        isNonNegInt(regNo) && regNo <= 15,
        'invalid register number'
    );

    assert (
        lowReg === undefined || lowReg instanceof x86.Register,
        'invalid low register'
    );

    this.name = name;

    this.type = type;

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
Get the low sub-register of a given size
*/
x86.Register.prototype.getSubOpnd = function (size)
{
    assert (
        size === this.size || (size < this.size && this.lowReg),
        'no sub-reg of size ' + size + ' for ' + this
    );

    if (size === this.size)
        return this;
    else
        return this.lowReg.getSubOpnd(size);
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
        size === 8 ||
        size === 16 ||
        size === 32 ||
        size === 64 ||
        size === 128,
        'invalid mem loc size: ' + size
    );

    assert (
        base === undefined || base instanceof x86.Register,
        'invalid SIB base'
    );

    if (disp === undefined)
        disp = 0;

    assert (
        isInt(disp) && num_ge(disp, getIntMin(32)) && num_le(disp, getIntMax(32)),
        'invalid mem loc disp'
    );

    assert (
        scale === 1 || scale === 2 || scale === 4 || scale === 8,
        'invalid SIB scale'
    );

    assert (
        base === undefined || 
        base instanceof x86.Register ||
        (base.type === 'gp' || base.type === 'ip')       
        (base.size === 32 || base.size === 64),
        'invalid SIB base: ' + base
    );

    assert (
        index === undefined || 
        index instanceof x86.Register ||
        index.type === 'gp' ||
        (index.size === 32 || index.size === 64),
        'invalid SIB index: ' + index
    );

    assert (
        !(base && index && base.size !== index.size),
        'cannot use base and index registers of different sizes'
    );

    assert (
        !(base && base.size < 32) && !(index && index.size < 32),
        'cannot use 8 or 16 bit registers as base or index'
    );

    assert (
        index !== x86.regs.esp &&
        index !== x86.regs.rsp,
        'cannot use esp/rsp as the index register'
    );
   
    assert (
        !(!base && index === x86.regs.r12),
        'cannot use as index without a base register'
    );

    // Test if the rex prefix is needed
    this.rexNeeded = (base && base.rexNeeded) || (index && index.rexNeeded);

    // Test if the operand is available with the rex prefix
    this.rexAvail = (!base || base.rexAvail) && (!index || index.rexAvail);

    assert (
        !(this.rexNeeded && !this.rexAvail),
        'invalid index and base combination w.r.t. REX pefix'
    );

    /**
    @field Memory location size
    */ 
    this.size = size;

    /**
    @field Base register
    */
    this.base = base;

    /**
    @field Displacement value
    */
    this.disp = disp;

    /**
    @field Index register
    */
    this.index = index;

    /**
    @field Scale value
    */
    this.scale = scale;

    /**
    @field Required displacement size
    */
    this.dispSize = 0;

    // If EBP or RBP or R13 is used as the base, displacement must be encoded
    if (base === x86.regs.ebp ||
        base === x86.regs.rbp || 
        base === x86.regs.r13)
        this.dispSize = 8;

    // If using displacement only or if using an index only or if using
    // RIP as the base, use disp32
    if ((!base && !index) || 
        (!base && index) ||
        (base === x86.regs.rip))
    {
        this.dispSize = 32;
    }

    // Compute the required displacement size
    else if (num_ne(disp, 0))
    {
        if (num_ge(disp, getIntMin(8)) && num_le(disp, getIntMax(8)))
            this.dispSize = 8;
        else if (num_ge(disp, getIntMin(32)) && num_le(disp, getIntMax(32)))
            this.dispSize = 32;
        else
            error('displacement does not fit within 32 bits');
    }
}
x86.MemLoc.prototype = new x86.Operand();

/**
Produce a string representation of the memory location
*/
x86.MemLoc.prototype.toString = function ()
{
    var str = '';

    switch (this.size)
    {
        case 8:     str += 'byte'; break;
        case 16:    str += 'word'; break;
        case 32:    str += 'dword'; break;
        case 64:    str += 'qword'; break;
        case 128:   str += 'oword'; break;
        default:
        error('unknown operand size');
    }

    if (this.base)
    {
        if (str != '')
            str += ' ';

        str += this.base;
    }

    if (this.disp)
    {
        var disp = this.disp;

        if (str != '')
        {
            if (num_lt(disp, 0))
            {
                str += ' - ';
                disp = num_mul(disp, -1);
            }
            else
            {
                str += ' + '
            }
        }

        str += disp;
    }

    if (this.index)
    {
        if (str != '')
            str += ' + ';

        if (this.scale !== 1)
            str += this.scale + ' * ';

        str += this.index;
    }

    str = '[' + str + ']';

    return str;
}

/**
Get the low sub-location of a given size
*/
x86.MemLoc.prototype.getSubOpnd = function (size)
{
    assert (
        size <= this.size,
        'no sub-loc of size ' + size + ' for ' + this
    );

    if (size === this.size)
    {
        return this;
    }
    else
    {
        return new x86.MemLoc(
            size,
            this.base,
            this.disp,
            this.index,
            this.scale
        );
    }
}

/**
Test whether or not an SIB byte is needed
*/
x86.MemLoc.prototype.sibNeeded = function (x86_64)
{
    return (
        this.index || 
        this.scale != 1 ||
        (x86_64 && !this.base && !this.index) ||
        this.base === x86.regs.esp ||
        this.base === x86.regs.rsp ||
        this.base === x86.regs.r12
    );
}

/**
@class Immediate operand
@extends x86.Operand
*/
x86.Immediate = function (value)
{
    assert (
        isInt(value),
        'immediate must be integer'
    );

    assert (
        num_ge(value, getIntMin(64)) && num_le(value, getIntMax(64, true)),
        'immediate does not fit within 64 bits: ' + num_to_string(value)
    );

    /**
    @field Field value
    */
    this.value = value;

    /**
    @field Immediate value type. 
    Can be either 'imm' or 'moffs'
    */
    this.type = 'imm';

    // Compute the smallest size this immediate fits in
    if (num_ge(value, getIntMin(8)) && num_le(value, getIntMax(8)))
        this.size = 8;
    else if (num_ge(value, getIntMin(16)) && num_le(value, getIntMax(16)))
        this.size = 16;
    else if (num_ge(value, getIntMin(32)) && num_le(value, getIntMax(32)))
        this.size = 32;
    else if (num_ge(value, getIntMin(64)) && num_le(value, getIntMax(64)))
        this.size = 64;
    else 
        this.size = undefined;

    /**
    @field Size of the constant if treated as unsigned
    */
    this.unsgSize = undefined;

    // If the constant is positive, compute its unsigned size
    if (num_ge(value, 0))
    {
        if (num_le(value, getIntMax(8, true)))
            this.unsgSize = 8;
        else if (num_le(value, getIntMax(16, true)))
            this.unsgSize = 16;
        else if (num_le(value, getIntMax(32, true)))
            this.unsgSize = 32;
        else if (num_le(value, getIntMax(64, true)))
            this.unsgSize = 64;
    }
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
        this.size <= immSize || this.unsgSize <= immSize,
        'immediate size too small for value'
    );

    codeBlock.writeInt(this.value, immSize);
}

/**
Link-time value operand
@extends x86.Immediate
*/
x86.LinkValue = function (value, ptrSize)
{
    assert (
        value instanceof ConstValue ||
        value instanceof IRFunction,
        'invalid link value'
    );

    assert (
        ptrSize === 32 || ptrSize === 64,
        'invalid pointer size'
    );

    this.value = value;

    this.size = ptrSize;
}
x86.LinkValue.prototype = new x86.Immediate(0);

/**
Produce a string representation of the link value
*/
x86.LinkValue.prototype.toString = function ()
{
    return capStrLen(this.value.getValName(), 16, true);
}

/**
Write the link value value into a code block
*/
x86.LinkValue.prototype.writeImm = function (codeBlock, immSize)
{
    assert (
        this.size === immSize,
        'incorrect immediate size for link value'
    );

    // Write the link value into the code block
    codeBlock.writeLink(this.value, immSize);
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

    /**
    @field Relative offset to the label
    */
    this.relOffset = 0;

    // Initially set the size of the offset to 8,
    // this size may be changed by the assembler
    this.size = 8;
}
x86.LabelRef.prototype = new x86.Operand();

/**
Produce a string representation of the label reference
*/
x86.LabelRef.prototype.toString = function ()
{
    return this.label.name + ' (' + this.size + ')';
};

/**
Namespace of supported x86 registers
*/
x86.regs = {};

// General-purpose registers
x86.regs.al     = new x86.Register('al', 'gp', 0, 8);
x86.regs.cl     = new x86.Register('cl', 'gp', 1, 8);
x86.regs.dl     = new x86.Register('dl', 'gp', 2, 8);
x86.regs.bl     = new x86.Register('bl', 'gp', 3, 8);
x86.regs.ah     = new x86.Register('ah', 'gp', 4, 8, undefined, undefined, false, false);
x86.regs.ch     = new x86.Register('ch', 'gp', 5, 8, undefined, undefined, false, false);
x86.regs.dh     = new x86.Register('dh', 'gp', 6, 8, undefined, undefined, false, false);
x86.regs.bh     = new x86.Register('bh', 'gp', 7, 8, undefined, undefined, false, false);
x86.regs.spl    = new x86.Register('spl', 'gp', 4, 8, undefined, undefined, true);
x86.regs.bpl    = new x86.Register('bpl', 'gp', 5, 8, undefined, undefined, true);
x86.regs.sil    = new x86.Register('sil', 'gp', 6, 8, undefined, undefined, true);
x86.regs.dil    = new x86.Register('dil', 'gp', 7, 8, undefined, undefined, true);
x86.regs.ax     = new x86.Register('ax', 'gp', 0, 16, x86.regs.al, x86.regs.ah);
x86.regs.cx     = new x86.Register('cx', 'gp', 1, 16, x86.regs.cl, x86.regs.ch);
x86.regs.dx     = new x86.Register('dx', 'gp', 2, 16, x86.regs.dl, x86.regs.dh);
x86.regs.bx     = new x86.Register('bx', 'gp', 3, 16, x86.regs.bl, x86.regs.bh);
x86.regs.sp     = new x86.Register('sp', 'gp', 4, 16, x86.regs.spl);
x86.regs.bp     = new x86.Register('bp', 'gp', 5, 16, x86.regs.bpl);
x86.regs.si     = new x86.Register('si', 'gp', 6, 16, x86.regs.sil);
x86.regs.di     = new x86.Register('di', 'gp', 7, 16, x86.regs.dil);
x86.regs.eax    = new x86.Register('eax', 'gp', 0, 32, x86.regs.ax);
x86.regs.ecx    = new x86.Register('ecx', 'gp', 1, 32, x86.regs.cx);
x86.regs.edx    = new x86.Register('edx', 'gp', 2, 32, x86.regs.dx);
x86.regs.ebx    = new x86.Register('ebx', 'gp', 3, 32, x86.regs.bx);
x86.regs.esp    = new x86.Register('esp', 'gp', 4, 32, x86.regs.sp);
x86.regs.ebp    = new x86.Register('ebp', 'gp', 5, 32, x86.regs.bp);
x86.regs.esi    = new x86.Register('esi', 'gp', 6, 32, x86.regs.si);
x86.regs.edi    = new x86.Register('edi', 'gp', 7, 32, x86.regs.di);
x86.regs.r8l    = new x86.Register('r8l', 'gp', 8, 8, undefined, true);
x86.regs.r9l    = new x86.Register('r9l', 'gp', 9, 8, undefined, true);
x86.regs.r10l   = new x86.Register('r10l', 'gp', 10, 8, undefined, true);
x86.regs.r11l   = new x86.Register('r11l', 'gp', 11, 8, undefined, true);
x86.regs.r12l   = new x86.Register('r12l', 'gp', 12, 8, undefined, true);
x86.regs.r13l   = new x86.Register('r13l', 'gp', 13, 8, undefined, true);
x86.regs.r14l   = new x86.Register('r14l', 'gp', 14, 8, undefined, true);
x86.regs.r15l   = new x86.Register('r15l', 'gp', 15, 8, undefined, true);
x86.regs.r8w    = new x86.Register('r8w', 'gp', 8, 16, x86.regs.r8l, undefined, true);
x86.regs.r9w    = new x86.Register('r9w', 'gp', 9, 16, x86.regs.r9l, undefined, true);
x86.regs.r10w   = new x86.Register('r10w', 'gp', 10, 16, x86.regs.r10l, undefined, true);
x86.regs.r11w   = new x86.Register('r11w', 'gp', 11, 16, x86.regs.r11l, undefined, true);
x86.regs.r12w   = new x86.Register('r12w', 'gp', 12, 16, x86.regs.r12l, undefined, true);
x86.regs.r13w   = new x86.Register('r13w', 'gp', 13, 16, x86.regs.r13l, undefined, true);
x86.regs.r14w   = new x86.Register('r14w', 'gp', 14, 16, x86.regs.r14l, undefined, true);
x86.regs.r15w   = new x86.Register('r15w', 'gp', 15, 16, x86.regs.r15l, undefined, true);
x86.regs.r8d    = new x86.Register('r8d', 'gp', 8, 32, x86.regs.r8w, undefined, true);
x86.regs.r9d    = new x86.Register('r9d', 'gp', 9, 32, x86.regs.r9w, undefined, true);
x86.regs.r10d   = new x86.Register('r10d', 'gp', 10, 32, x86.regs.r10w, undefined, true);
x86.regs.r11d   = new x86.Register('r11d', 'gp', 11, 32, x86.regs.r11w, undefined, true);
x86.regs.r12d   = new x86.Register('r12d', 'gp', 12, 32, x86.regs.r12w, undefined, true);
x86.regs.r13d   = new x86.Register('r13d', 'gp', 13, 32, x86.regs.r13w, undefined, true);
x86.regs.r14d   = new x86.Register('r14d', 'gp', 14, 32, x86.regs.r14w, undefined, true);
x86.regs.r15d   = new x86.Register('r15d', 'gp', 15, 32, x86.regs.r15w, undefined, true);
x86.regs.rax    = new x86.Register('rax', 'gp', 0, 64, x86.regs.eax);
x86.regs.rcx    = new x86.Register('rcx', 'gp', 1, 64, x86.regs.ecx);
x86.regs.rdx    = new x86.Register('rdx', 'gp', 2, 64, x86.regs.edx);
x86.regs.rbx    = new x86.Register('rbx', 'gp', 3, 64, x86.regs.ebx);
x86.regs.rsp    = new x86.Register('rsp', 'gp', 4, 64, x86.regs.esp);
x86.regs.rbp    = new x86.Register('rbp', 'gp', 5, 64, x86.regs.ebp);
x86.regs.rsi    = new x86.Register('rsi', 'gp', 6, 64, x86.regs.esi);
x86.regs.rdi    = new x86.Register('rdi', 'gp', 7, 64, x86.regs.edi);
x86.regs.r8     = new x86.Register('r8', 'gp', 8, 64, x86.regs.r8d, undefined, true);
x86.regs.r9     = new x86.Register('r9', 'gp', 9, 64, x86.regs.r9d, undefined, true);
x86.regs.r10    = new x86.Register('r10', 'gp', 10, 64, x86.regs.r10d, undefined, true);
x86.regs.r11    = new x86.Register('r11', 'gp', 11, 64, x86.regs.r11d, undefined, true);
x86.regs.r12    = new x86.Register('r12', 'gp', 12, 64, x86.regs.r12d, undefined, true);
x86.regs.r13    = new x86.Register('r13', 'gp', 13, 64, x86.regs.r13d, undefined, true);
x86.regs.r14    = new x86.Register('r14', 'gp', 14, 64, x86.regs.r14d, undefined, true);
x86.regs.r15    = new x86.Register('r15', 'gp', 15, 64, x86.regs.r15d, undefined, true);

// Instruction pointer, for RIP addressing
x86.regs.rip    = new x86.Register('rip', 'ip', 5, 64);

// Floating-point registers (x87)
x86.regs.st0    = new x86.Register('st0', 'fp', 0, 80);

// XMM SIMD registers
x86.regs.xmm0   = new x86.Register('xmm0', 'xmm', 0, 128);
x86.regs.xmm1   = new x86.Register('xmm1', 'xmm', 1, 128);
x86.regs.xmm2   = new x86.Register('xmm2', 'xmm', 2, 128);
x86.regs.xmm3   = new x86.Register('xmm3', 'xmm', 3, 128);
x86.regs.xmm4   = new x86.Register('xmm4', 'xmm', 4, 128);
x86.regs.xmm5   = new x86.Register('xmm5', 'xmm', 5, 128);
x86.regs.xmm6   = new x86.Register('xmm6', 'xmm', 6, 128);
x86.regs.xmm7   = new x86.Register('xmm7', 'xmm', 7, 128);
x86.regs.xmm8   = new x86.Register('xmm8', 'xmm', 8, 128, undefined, undefined, true);
x86.regs.xmm9   = new x86.Register('xmm9', 'xmm', 9, 128, undefined, undefined, true);
x86.regs.xmm10  = new x86.Register('xmm10', 'xmm', 10, 128, undefined, undefined, true);
x86.regs.xmm11  = new x86.Register('xmm11', 'xmm', 11, 128, undefined, undefined, true);
x86.regs.xmm12  = new x86.Register('xmm12', 'xmm', 12, 128, undefined, undefined, true);
x86.regs.xmm13  = new x86.Register('xmm13', 'xmm', 13, 128, undefined, undefined, true);
x86.regs.xmm14  = new x86.Register('xmm14', 'xmm', 14, 128, undefined, undefined, true);
x86.regs.xmm15  = new x86.Register('xmm15', 'xmm', 15, 128, undefined, undefined, true);

