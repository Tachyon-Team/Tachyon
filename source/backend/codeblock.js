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
Executable machine code block implementation.

@author
Maxime Chevalier-Boisvert
*/

/**
@class Low-level machine code block implementation.
Stores generated machine code, external references and exposed labels.
*/
function CodeBlock(size)
{
    /**
    @field Executable memory block
    */
    this.memBlock = allocMemoryBlock(size, true);

    /**
    @field Memory block size
    */
    this.size = size;

    /**
    @field Current writing position
    */
    this.writePos = 0;
}

/**
Print the code block as a string
*/
CodeBlock.prototype.toString = function ()
{
    var str = '';

    for (var i = 0; i < this.writePos; ++i)
    {
        var byteVal = this.readByte(i);

        var byteStr = byteVal.toString(16);
        byteStr = byteStr.toUpperCase();
        if (byteStr.length === 1)
            byteStr = '0' + byteStr;

        if (i !== 0)
            str += ' ';

        str += byteStr;
    }

    return str;
}

/**
Get the address of an offset in the code block
*/
CodeBlock.prototype.getAddress = function (idx)
{
    return getBlockAddr(this.memBlock, idx);
}

/**
Clear the contents of the code block
*/
CodeBlock.prototype.clear = function ()
{
    this.writePos = 0;
}

/**
Read a byte at a given position
*/
CodeBlock.prototype.readByte = function (idx)
{
    assert (
        idx < this.size,
        'invalid code block index'
    );

    return readFromMemoryBlock(this.memBlock, idx);
}

/**
Write a byte at the current position
*/
CodeBlock.prototype.writeByte = function (val)
{
    assert (
        this.memBlock,
        'invalid memory block'
    );

    assert (
        this.writePos + 1 <= this.size,
        'no space to write byte in code block ' +
        '(pos ' + this.writePos + '/' + this.size + ')'
    );

    assert (
        isNonNegInt(val) && val <= 255,
        'invalid byte value: ' + val
    );

    writeToMemoryBlock(this.memBlock, this.writePos, val);

    this.writePos += 1;
}

/**
Write a signed integer at the current position
*/
CodeBlock.prototype.writeInt = function (val, numBits)
{
    assert (
        isPosInt(numBits) && numBits % 8 === 0,
        'the number of bits must be a positive multiple of 8'
    );

    assert (
        num_ge(val, getIntMin(numBits)) &&
        num_le(val, getIntMax(numBits, true)),
        'integer value does not fit within ' + numBits + ' bits: ' + val
    );

    // Compute the size in bytes
    var numBytes = numBits / 8;

    // Write out the bytes
    for (var i = 0; i < numBytes; ++i)
    {
        //print(num_to_string(val));

        var byteVal = num_and(val, 0xFF);

        this.writeByte(byteVal);

        val = num_shift(val, -8);
    }
}

