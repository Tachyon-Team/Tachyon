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
Encoding of x86 instructions.

@author
Maxime Chevalier-Boisvert
*/

/**
x86 namespace
*/
var x86 = x86 || {};

/**
x86 instructions namespace
*/
x86.instrs = {};

/**
@class Base class for x86 instructions
*/
x86.Instruction = function ()
{
    /**
    @field Instruction operands
    */
    this.opnds = [];

    /**
    @field Previous instruction in the linked list
    */
    this.prev = null;

    /**
    @field Next instruction in the linked list
    */
    this.next = null;

    /**
    @field Instruction encoding descriptor
    */
    this.encDesc = null;

    /**
    @field Length of the instruction encoding
    */
    this.encLength = 0;
}

/**
Produce a string representation of this instruction
*/
x86.Instruction.prototype.toString = function ()
{
    var str = '';

    str += this.mnem;

    for (var i = 0; i < this.opnds.length; ++i)
    {
        if (i === 0)
            str += ' ';
        else
            str += ', ';

        str += this.opnds[i].toString();
    }

    str += ';';

    return str;
}

/**
Compute the length of an encoding of this instruction
*/
x86.Instruction.prototype.compEncLen = function (enc, x86_64)
{
    // x86 instruction format:
    // prefix(es)  [REX] opcode  [XRM  [SIB]]  disp  imm

    // Flag to indicate the REX prefix is needed
    var rexNeeded = (enc.REX_W === 1);

    // Flags to indicate if the ModRM and SIB bytes needed
    var rmNeeded = false;
    var sibNeeded = false;

    // RM operand, if present
    var rmOpnd = null;

    // Displacement size required
    var dispSize = 0;

    // Immediate size required
    var immSize = 0;

    // For each operand
    for (var i = 0; i < this.opnds.length; ++i)
    {
        var opnd = this.opnds[i];

        var opndType = x86.opndType(enc.opnds[i]);
        var opndSize = x86.opndSize(enc.opnds[i]);

        if (opnd.rexNeeded)
            rexNeeded = true;

        if (opndType === 'imm' ||
            opndType === 'moffs')
        {
            immSize = opndSize;
        }

        else if (opndType === 'rel')
        {
            immSize = opndSize;
        }

        else if (opndType === 'm' ||
                 opndType === 'r/m' ||
                 opndType === 'xmm/m')
        {
            rmNeeded = true;
            rmOpnd = opnd;

            if (opnd instanceof x86.MemLoc)
            {
                if (opnd.sibNeeded(x86_64))
                    sibNeeded = true;

                if (opnd.dispSize > 0)
                    dispSize = opnd.dispSize;
            }
        }
    }

    // Total encoding size
    var size = 0;

    // Add the address-size prefix, if needed
    if (rmOpnd && x86_64 &&
        ((rmOpnd.base && rmOpnd.base.size === 32) ||
         (rmOpnd.index && rmOpnd.index.size === 32)))
        size += 1;

    // Add the operand-size prefix, if needed
    if (enc.szPref === true)
        size += 1;

    // Add the prefix size
    size += enc.prefix.length;

    // Add the REX prefix, if needed
    if (rexNeeded === true)
        size += 1;

    // Add the opcode size
    size += enc.opCode.length;

    // Add the ModR/M byte, if needed
    if (rmNeeded)
        size += 1;

    // Add the SIB byte, if needed
    if (sibNeeded)
        size += 1;

    // Add the displacement size (in bytes)
    size += dispSize / 8;

    // Add the immediate size (in bytes)
    size += immSize / 8;

    // Return the encoding size
    return size;
}

/**
Find the shortest possible encoding for this instruction.
Returns null if no valid encoding is found
*/
x86.Instruction.prototype.findEncoding = function (x86_64)
{
    // Best encoding found
    var bestEnc = null;
    var bestLen = 0xFFFF;

    // For each possible encoding
    ENC_LOOP:
    for (var i = 0; i < this.encodings.length; ++i)
    {
        //print('encoding #' + (i+1));

        var enc = this.encodings[i];

        // If we are in x86-64 and this encoding is not valid in that mode
        if (x86_64 === true && enc.x86_64 === false)
            continue ENC_LOOP;

        // If the number of operands does not match, skip this encoding
        if (enc.opnds.length !== this.opnds.length)
            continue ENC_LOOP;

        // For each operand
        for (var j = 0; j < this.opnds.length; ++j)
        {
            var opnd = this.opnds[j];

            var encOpnd = enc.opnds[j];

            var opndType = x86.opndType(encOpnd);
            var opndSize = x86.opndSize(encOpnd);

            /*
            print('opnd: ' + opnd);
            print('enc : ' + encOpnd);
            */

            // If this encoding requires REX but the operand is not
            // available under REX, skip this encoding
            if (enc.REX_W && !opnd.rexAvail)
                continue ENC_LOOP;

            // Switch on the operand type
            switch (opndType)
            {
                case 'fixed_reg':
                if (opnd !== encOpnd)
                    continue ENC_LOOP;
                break;

                case 'cst':
                if (!(opnd instanceof x86.Immediate))
                    continue ENC_LOOP;
                if (opnd.value !== encOpnd)
                    continue ENC_LOOP;
                break;

                case 'imm':
                //print('imm opnd');
                //print('imm opnd size: ' + opnd.size);
                if (!(opnd instanceof x86.Immediate))
                    continue ENC_LOOP;
                if (opnd.type !== 'imm')
                    continue ENC_LOOP;
                if (opnd.size > opndSize)
                {
                    if (!opnd.unsgSize)
                        continue ENC_LOOP;

                    if (opnd.unsgSize !== opndSize || 
                        opndSize !== enc.opndSize)
                        continue ENC_LOOP;
                }
                break;

                case 'r':
                if (!(opnd instanceof x86.Register && opnd.type === 'gp'))
                    continue ENC_LOOP;
                if (opnd.size !== opndSize)
                    continue ENC_LOOP;
                break;

                case 'xmm':
                if (!(opnd instanceof x86.Register && opnd.type === 'xmm'))
                    continue ENC_LOOP;
                break;

                case 'm':
                if (!(opnd instanceof x86.MemLoc))
                    continue ENC_LOOP;
                if (opnd.size !== opndSize)
                    continue ENC_LOOP;
                break;

                case 'r/m':
                if (!(opnd instanceof x86.Register && opnd.type === 'gp') && 
                    !(opnd instanceof x86.MemLoc))
                    continue ENC_LOOP;
                if (opnd.size !== opndSize)
                    continue ENC_LOOP;
                break;

                case 'xmm/m':
                if (!(opnd instanceof x86.Register && opnd.type === 'xmm') && 
                    !(opnd instanceof x86.MemLoc && opnd.size === opndSize))
                    continue ENC_LOOP;
                break;

                case 'rel':
                if (!(opnd instanceof x86.LabelRef))
                    continue ENC_LOOP;
                if (opnd.size > opndSize)
                    continue ENC_LOOP;
                break;

                case 'moffs':
                if (!(opnd instanceof x86.Immediate))
                    continue ENC_LOOP;
                if (opnd.type !== 'moffs')
                    continue ENC_LOOP;
                if (opnd.unsgSize > opndSize)
                    continue ENC_LOOP;
                break;

                default:
                error('invalid operand type "' + opndType + '"');
            }
        }

        var len = this.compEncLen(enc, x86_64);

        //print('encoding length: ' + len);

        if (len < bestLen)
        {
            bestEnc = enc;
            bestLen = len;
        }
    }

    assert (
        bestEnc !== null,
        'no valid ' + (x86_64? 64:32) + '-bit encoding for "' + this + '"'
    );

    // Store the best encoding found
    this.encDesc = bestEnc;

    // Store the encoding length
    this.encLength = bestLen;
};

/**
Get the length of this instruction
*/
x86.Instruction.prototype.getLength = function (x86_64)
{
    // If no encoding is yet found, find one
    if (this.encDesc === null)
        this.findEncoding();

    // Return the encoding length
    return this.encLength;
}

/**
Encode the instruction into a byte array
*/
x86.Instruction.prototype.encode = function (codeBlock, x86_64)
{
    // If no encoding is yet found, find one
    if (this.encDesc === null)
        this.findEncoding();

    // Get a reference to the encoding descriptor found
    var enc = this.encDesc;

    // Flag to indicate the REX prefix is needed
    var rexNeeded = (enc.REX_W === 1);

    // Flags to indicate if the ModRM and SIB bytes needed
    var rmNeeded = false;
    var sibNeeded = false;

    // r and r/m operands
    var rOpnd = null;
    var rmOpnd = null;

    // Immediate operand size and value
    var immSize = 0;
    var immVal = 0;

    // Displacement size and value
    var dispSize = 0;
    var dispVal = 0;

    // For each operand
    for (var i = 0; i < this.opnds.length; ++i)
    {
        var opnd = this.opnds[i];

        var opndType = x86.opndType(enc.opnds[i]);
        var opndSize = x86.opndSize(enc.opnds[i]);

        if (opnd.rexNeeded)
            rexNeeded = true;

        if (opndType === 'imm' || 
            opndType === 'moffs')
        {
            immSize = opndSize;
            immVal = opnd.value;
        }

        else if (opndType === 'rel')
        {
            immSize = opndSize;
            immVal = opnd.relOffset;
        }

        else if (opndType === 'r' ||
                 opndType === 'xmm')
        {
            rOpnd = opnd;
        }

        else if (opndType === 'm' ||
                 opndType === 'r/m' ||
                 opndType === 'xmm/m')
        {
            rmNeeded = true;
            rmOpnd = opnd;

            if (opnd instanceof x86.MemLoc)
            {
                if (opnd.sibNeeded(x86_64))
                {
                    sibNeeded = true;
                }

                if (opnd.dispSize > 0)
                {
                    dispSize = opnd.dispSize;
                    dispVal = opnd.disp;
                }
            }
        }
    }

    // Get the index in the code block before the encoding
    var startIndex = codeBlock.writePos;

    // Add the address-size prefix, if needed
    if (rmOpnd && x86_64 &&
        ((rmOpnd.base && rmOpnd.base.size === 32) ||
         (rmOpnd.index && rmOpnd.index.size === 32)))
        codeBlock.writeByte(0x67);

    // Add the operand-size prefix, if needed
    if (enc.szPref === true)
        codeBlock.writeByte(0x66);

    // Write the prefix bytes to the code block
    for (var i = 0; i < enc.prefix.length; ++i)
        codeBlock.writeByte(enc.prefix[i]);

    // Add the REX prefix, if needed
    if (rexNeeded === true)
    {
        // 0 1 0 0 w r x b
        // w - 64-bit operand size flag
        // r - MODRM.reg extension
        // x - SIB.index extension
        // b - MODRM.rm or SIB.base extension

        var w = enc.REX_W;

        var r;
        if (rOpnd && rmOpnd)
            r = (rOpnd.regNo & 8)? 1:0;
        else
            r = 0;

        var x;
        if (sibNeeded && rmOpnd.index instanceof x86.Register)
            x = (rmOpnd.index.regNo & 8)? 1:0;
        else
            x = 0;

        var b;
        if (rmOpnd instanceof x86.Register)
            b = (rmOpnd.regNo & 8)? 1:0;
        else if (rOpnd && !rmOpnd)
            b = (rOpnd.regNo & 8)? 1:0;
        else if (rmOpnd && rmOpnd.base instanceof x86.Register)
            b = (rmOpnd.base.regNo & 8)? 1:0;
        else
            b = 0;

        // Encode and write the REX byte
        var rexByte = 0x40 + (w << 3) + (r << 2) + (x << 1) + (b);
        codeBlock.writeByte(rexByte);
    }

    // If an opcode reg field is to be used
    if (rOpnd && !rmOpnd)
    {
        // Write the reg field into the opcode byte
        var opByte = enc.opCode[0] | (rOpnd.regNo & 7);
        codeBlock.writeByte(opByte);
    }
    else
    {
        // Write the opcode bytes to the code block
        for (var i = 0; i < enc.opCode.length; ++i)
            codeBlock.writeByte(enc.opCode[i]);
    }

    // Add the ModR/M byte, if needed
    if (rmNeeded)
    {
        // MODRM.mod (2 bits)
        // MODRM.reg (3 bits)
        // MODRM.rm  (3 bits)

        assert (
            !(enc.opExt && rOpnd),
            'opcode extension and register operand present'
        );

        // Encode the mod field
        var mod;
        if (rmOpnd instanceof x86.Register)
        {
            mod = 3;
        }
        else
        {
            if (dispSize === 0 || !rmOpnd.base)
                mod = 0;
            else if (dispSize === 8)
                mod = 1
            else if (dispSize === 32)
                mod = 2;
        }

        // Encode the reg field
        var reg;
        if (enc.opExt)
            reg = enc.opExt;
        else if (rOpnd)
            reg = rOpnd.regNo & 7;
        else
            reg = 0;

        // Encode the rm field
        var rm;
        if (rmOpnd instanceof x86.Register)
        {
            rm = rmOpnd.regNo & 7;
        }
        else
        {
            if (sibNeeded)
                rm = 4;
            else if (!x86_64 && !rmOpnd.base && !rmOpnd.index)
                rm = 5;
            else if (rmOpnd.base === x86.regs.rip)
                rm = 5;
            else if (rmOpnd.base)
                rm = rmOpnd.base.regNo & 7;
            else
                rm = 0;
        }

        /*
        print('mod: ' + mod);
        print('reg: ' + reg);
        print('rm: ' + rm);
        */

        // Encode and write the ModR/M byte
        var rmByte = (mod << 6) + (reg << 3) + (rm);
        codeBlock.writeByte(rmByte);
    }

    // Add the SIB byte, if needed
    if (sibNeeded)
    {
        // SIB.scale (2 bits)
        // SIB.index (3 bits)
        // SIB.base  (3 bits)

        assert (
            rmOpnd instanceof x86.MemLoc,
            'expected r/m opnd to be mem loc'
        );

        // Encode the scale value
        var scale;
        switch (rmOpnd.scale)
        {
            case 1: scale = 0; break;
            case 2: scale = 1; break
            case 4: scale = 2; break
            case 8: scale = 3; break
            default: error('invalid SIB scale: ' + rmOpnd.scale);
        }

        // Encode the index value
        var index;
        if (!rmOpnd.index)
            index = 4;
        else
            index = rmOpnd.index.regNo & 7;

        // Encode the base register
        var base;
        if (!rmOpnd.base)
            base = 5;
        else
            base = rmOpnd.base.regNo & 7;

        // Encode and write the SIB byte
        var sibByte = (scale << 6) + (index << 3) + (base);
        codeBlock.writeByte(sibByte);
    }

    // Add the displacement size
    if (dispSize !== 0)
        codeBlock.writeInt(dispVal, dispSize);

    // If there is an immediate operand, write it
    if (immSize !== 0)
        codeBlock.writeInt(immVal, immSize);

    // Get the index in the code block after the encoding
    var endIndex = codeBlock.writePos;

    // Compute the length of the data written
    var wrtLength = endIndex - startIndex;

    assert (
        wrtLength === this.encLength,
        'encoding length:\n' +
        wrtLength + '\n' +
        'does not match expected length:\n' +
        this.encLength + '\n' +
        'for:\n' +
        this
    );
};

/**
@class Label operand
@extends x86.Operand
*/
x86.Label = function (name, extern)
{
    /**
    @field Label name
    */
    this.name = name;

    /**
    @field Offset at which this label is located
    */
    this.offset = 0;

    /**
    @field Reference count for this label
    */
    this.refCount = 0;

    /**
    @field Flag to indicate this label will be externally visible
    and usable for linking.
    */
    this.extern = false;
}
x86.Label.prototype = new x86.Instruction();

/**
Get the string representation of a label
*/
x86.Label.prototype.toString = function ()
{
    return this.name + ':';
}

/**
Find an encoding for a label, does nothing
*/
x86.Label.prototype.findEncoding = function ()
{
}

/**
Get the length of a label, always 0
*/
x86.Label.prototype.getLength = function ()
{
    return 0;
}

/**
Encode a label into a code block, does nothing
*/
x86.Label.prototype.encode = function ()
{
}

/**
Test if an operand is valid
*/
x86.opndValid = function (opnd)
{
    if (typeof opnd === 'number')
        return true;

    if (opnd instanceof x86.Register)
        return true;

    switch (opnd)
    {
        case 'r8':
        case 'r/m8':
        case 'imm8':
        case 'rel8':

        case 'r16':
        case 'r/m16':
        case 'imm16':
        case 'rel16':

        case 'r32':
        case 'r/m32':
        case 'imm32':
        case 'rel32':
        case 'moffs32':

        case 'r64':
        case 'm64':
        case 'r/m64':
        case 'xmm/m64':
        case 'xmm/m128':
        case 'imm64':
        case 'moffs64':

        case 'xmm':

        return true;

        default:
        return false;
    }
};

/**
Get an operand size from an operand description
*/
x86.opndSize = function (opnd)
{
    if (typeof opnd === 'number')
        return 0;

    if (opnd instanceof x86.Register)
        return opnd.size;

    switch (opnd)
    {
        case 'r8':
        case 'r/m8':
        case 'imm8':
        case 'rel8':
        return 8;

        case 'r16':
        case 'r/m16':
        case 'imm16':
        case 'rel16':
        return 16;

        case 'r32':
        case 'r/m32':
        case 'imm32':
        case 'rel32':
        case 'moffs32':
        return 32;

        case 'r64':
        case 'm64':
        case 'r/m64':
        case 'xmm/m64':
        case 'imm64':
        case 'moffs64':
        return 64;

        case 'xmm':
        case 'xmm/m128':
        return 128;

        default:
        error('invalid operand type in opndSize (' + opnd + ')');
    }
};

/**
Get the type of an operand
*/
x86.opndType = function (opnd)
{
    if (typeof opnd === 'number')
        return 'cst';

    if (opnd instanceof x86.Register)
        return 'fixed_reg';

    switch (opnd)
    {
        case 'r8':
        case 'r16':
        case 'r32':
        case 'r64':
        return 'r';

        case 'm64':
        return 'm';

        case 'xmm':
        return 'xmm';

        case 'r/m8':
        case 'r/m16':
        case 'r/m32':
        case 'r/m64':
        return 'r/m';

        case 'xmm/m64':
        case 'xmm/m128':
        return 'xmm/m';

        case 'imm8':
        case 'imm16':
        case 'imm32':
        case 'imm64':
        return 'imm';

        case 'rel8':
        case 'rel16':
        case 'rel32':
        return 'rel';

        case 'moffs32':
        case 'moffs64':
        return 'moffs';

        default:
        error('invalid operand type in opndSize (' + opnd + ')');
    }
};

/**
Anonymous function to create instruction classes from the instruction table.
*/
(function ()
{
    //print('instruction encodings: ' + x86.instrTable.length);

    // Table of mnemonics to possible instruction encodings
    var instrs = {};

    // Number of supported instructions
    var numInstrs = 0;

    // For each instruction encoding
    for (var i = 0; i < x86.instrTable.length; ++i)
    {
        var enc = x86.instrTable[i];

        var mnem = enc.mnem;

        assert (
            typeof mnem === 'string' && mnem !== '',
            'invalid mnemonic for encoding ' + (i+1)
        );

        // If no operands are specified, set the operands to the empty array
        if (enc.opnds === undefined)
            enc.opnds = [];

        assert (
            enc.opnds instanceof Array,
            'invalid operand array for ' + mnem
        );

        // For each operand
        for (var j = 0; j < enc.opnds.length; ++j)
        {
            var opnd = enc.opnds[j];

            // If this operand is a fixed register, replace it by the
            // corresponding register object
            for (var reg in x86.regs)
            {
                if (opnd === reg && opnd !== 'r8')
                {
                    opnd = x86.regs[reg];
                    enc.opnds[j] = opnd;
                }
            }

            assert (
                x86.opndValid(opnd),
                'operand ' + j + ' invalid for ' + mnem + ' (' + opnd + ')'
            );
        }

        // Update the operand size of the encoding
        if (!enc.opndSize && enc.opnds.length > 0)
            enc.opndSize = x86.opndSize(enc.opnds[0]);

        // If no mandatory prefix is defined, set it to the empty array
        if (enc.prefix === undefined)
            enc.prefix = [];

        assert (
            enc.opCode instanceof Array &&
            enc.opCode.length > 0 && 
            enc.opCode.length <= 3,
            'invalid opcode for ' + mnem
        );

        for (var j = 0; j < enc.opCode.length; ++j)
        {
            var opByte = enc.opCode[j];

            assert (
                isNonNegInt(opByte) && opByte <= 255,
                'invalid opcode byte for ' + mnem
            )
        }

        assert (
            enc.opExt === undefined ||
            (isNonNegInt(enc.opExt) && enc.opExt <= 7),
            'invalid opcode extension for ' + mnem
        );

        // If the operand-size prefix is not defined, set it to false
        if (enc.szPref === undefined)
            enc.szPref = false;

        assert (
            enc.szPref === true || enc.szPref === false,
            'invalid operand-size prefix flag for ' + mnem
        );

        // If the REX.W bit is not defined, set it to 0
        if (enc.REX_W === undefined)
            enc.REX_W = 0;

        assert (
            enc.REX_W === 1 || enc.REX_W === 0,
            'invalid REX.W flag for ' + mnem
        );

        // If the x86-64 supported flag is not set, set it to true
        if (enc.x86_64 === undefined)
            enc.x86_64 = true;

        assert (
            enc.x86_64 === true || enc.x86_64 === false,
            'invalid x86_64 flag for ' + mnem
        );

        assert (
            !(enc.szPref && 
              enc.opnds.length > 0 && 
              x86.opndSize(enc.opnds[0]) === 8),
            'cannot have operand-size override prefix with 8-bit operands (' +
            mnem + ')'
        );

        assert (
            !(enc.szPref && enc.REX_W),
            'cannot have both operand-size prefix and REX.W set (' + mnem + ')'
        );

        var numRM = 0;
        var numImm = 0;

        for (var j = 0; j < enc.opnds.length; ++j)
        {
            var opndType = x86.opndType(enc.opnds[j]);

            if (opndType === 'r/m')
                numRM++;
            else if (opndType === 'imm')
                numImm++;

            assert (
                !(opndType === 'r' && 
                  enc.opnds.length === 1 && 
                  enc.opCode.length > 1),
                'invalid opcode reg field use for ' + mnem
            );

            assert (
                !(opndType === 'r' &&
                  enc.opnds.length === 1 &&
                  (enc.opCode[0] & 7) !== 0),
               'nonzero opcode reg bits with reg field use for ' + mnem
            );
        }

        assert (
            numRM <= 1,
            'more than 1 r/m operand for ' + mnem
        );

        assert (
            numImm <= 1,
            'more than 1 imm operand for ' + mnem
        );

        // If there is no encoding list for this instruction
        if (instrs[mnem] === undefined)
        {
            // Create the list for this instruction
            instrs[mnem] = [];

            // Increment the supported instruction count
            numInstrs++;
        }

        // Add the encoding to the list for this instruction
        instrs[mnem].push(enc);
    }

    //print('supported instructions: ' + numInstrs);

    // Function to create an instruction class
    function makeInstr(mnem, encodings)
    {
        //print(mnem + ' (' + encList.length + ' encodings)');

        // Supported operand counts
        var opndCounts = [];

        // For each supported encoding
        for (var i = 0; i < encList.length; ++i)
        {
            var enc = encList[i];

            var opCount = enc.opnds.length;

            opndCounts[opCount] = true;
        }

        // Generate the instruction constructor
        var InstrCtor = function (opnds, x86_64)
        {
            assert (
                opnds.length <= 3 && opndCounts[opnds.length],
                'invalid operand count for ' + this.mnem + 
                ' (' + opnds.length + ')'
            );

            // Flag to indicate the REX prefix is needed
            var rexNeeded = false;

            // Flag to indicate all operands are available under a REX prefix
            var rexAvail = true;

            // For each operand
            for (var i = 0; i < opnds.length; ++i)
            {
                var opnd = opnds[i];

                if (opnd.rexNeeded)
                    rexNeeded = true;

                if (!opnd.rexAvail)
                    rexAvail = false;

                assert (
                    opnd instanceof x86.Operand,
                    'invalid operand for ' + this.mnem
                );
            }

            assert (
                !(x86_64 === false && rexNeeded === true),
                'cannot use REX operands in 32-bit mode'
            );

            assert (
                !(rexNeeded && !rexAvail),
                'some operands require REX but others are not available ' +
                'with REX'
            );

            // Copy and store the operand list
            this.opnds = opnds.slice(0);

            // If in debug mode, ensure that an initial encoding for the
            // instruction can be found
            if (DEBUG) this.findEncoding(x86_64);
        };

        // Set the prototype
        InstrCtor.prototype = new x86.Instruction();

        // Store the mnemonic name for the instruction
        InstrCtor.prototype.mnem = mnem;

        // Store the possible encodings for the instruction
        InstrCtor.prototype.encodings = encodings;

        // Store the constructor on the x86.instrs object
        x86.instrs[mnem] = InstrCtor;
    }

    // For each supported instruction
    for (var mnem in instrs)
    {
        var encList = instrs[mnem];

        // Create the instruction class
        makeInstr(mnem, encList);
    }

})();

