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
Runtime functions needed for FFI interfacing.

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010-2011 Maxime Chevalier-Boisvert, All Rights Reserved
*/

/**
Total amount of machine code bytes allocated.
*/
var codeBytesAllocated = 0;

// If we are running inside Tachyon
if (RUNNING_IN_TACHYON)
{
    /**
    Allocate a memory block.
    */
    var allocMemoryBlock = function (size, exec)
    {
        "tachyon:noglobal";

        assert (
            exec === true || exec === false,
            'executable flag not specified'
        );

        if (exec)
        {
            this.codeBytesAllocated += size;
        }

        var blockPtr = rawAllocMemoryBlock(unboxInt(size), boxToBool(exec));

        var blockObj = alloc_memblock();

        set_memblock_ptr(blockObj, blockPtr);
        set_memblock_size(blockObj, puint(size));

        return { mcb: blockObj };
    };

    /**
    Free a memory block.
    */
    var freeMemoryBlock = function (blockObj)
    {
        "tachyon:noglobal";

        var mcb = blockObj.mcb;

        assert (
            boolToBox(getRefTag(mcb) === TAG_OTHER),
            'invalid mcb reference'
        );

        var ptr = get_memblock_ptr(mcb);
        var size = iir.icast(IRType.pint, get_memblock_size(mcb));

        rawFreeMemoryBlock(ptr, size);
    };

    /**
    Get the address of an offset into the block, expressed
    as an array of bytes.
    */
    var getBlockAddr = function (blockObj, index)
    {
        "tachyon:noglobal";

        if (index === UNDEFINED)
            index = 0;

        var mcb = blockObj.mcb;

        assert (
            boolToBox(getRefTag(mcb) === TAG_OTHER),
            'invalid mcb reference'
        );

        var ptr = get_memblock_ptr(mcb);
        var size = iir.icast(IRType.pint, get_memblock_size(mcb));

        assert (
            boolToBox(unboxInt(index) < size),
            'invalid index in mcb'
        );

        var addr = ptr + unboxInt(index);

        // Convert the pointer to a byte array
        return ptrToByteArray(addr);
    };

    /**
    Write a byte to a memory block.
    */
    var writeToMemoryBlock = function (blockObj, index, byteVal)
    {
        "tachyon:noglobal";

        var mcb = blockObj.mcb;

        assert (
            boolToBox(getRefTag(mcb) === TAG_OTHER),
            'invalid mcb reference'
        );

        var ptr = get_memblock_ptr(mcb);
        var size = iir.icast(IRType.pint, get_memblock_size(mcb));

        assert (
            boolToBox(boxIsInt(index)),
            'index should be integer'
        );

        assert (
            boolToBox(unboxInt(index) < size),
            'invalid index in mcb'
        );

        assert (
            byteVal >= 0 && byteVal <= 255,
            "byte value '" + byteVal + "' out of range"
        );

        // Store the value in the block
        iir.store(IRType.u8, ptr, unboxInt(index), u8(byteVal));
    };

    /**
    Read a byte from a memory block.
    */
    var readFromMemoryBlock = function (blockObj, index)
    {
        "tachyon:noglobal";

        var mcb = blockObj.mcb;

        assert (
            boolToBox(getRefTag(mcb) === TAG_OTHER),
            'invalid mcb reference'
        );

        var ptr = get_memblock_ptr(mcb);
        var size = iir.icast(IRType.pint, get_memblock_size(mcb));

        assert (
            boolToBox(boxIsInt(index)),
            'index should be integer'
        );

        assert (
            boolToBox(unboxInt(index) < size),
            'invalid index in mcb'
        );

        // Load the value from the block
        var byteVal = iir.load(IRType.u8, ptr, unboxInt(index));

        // Box the byte value
        return boxInt(iir.icast(IRType.pint, byteVal));
    };

    /**
     Get memory block size
     */
    var getMemoryBlockSize = function (blockObj)
    {
        "tachyon:noglobal";

        var mcb = blockObj.mcb;

        assert (
            boolToBox(getRefTag(mcb) === TAG_OTHER),
            'invalid mcb reference'
        );

        return boxInt(iir.icast(IRType.pint, get_memblock_size(mcb)));
    };
}

// Otherwise, if we are running inside D8
else
{
    /**
    Allocate a memory block.
    */
    var v8AllocMemoryBlock = allocMemoryBlock;
    var allocMemoryBlock = function (size, exec)
    {
        if (exec)
        {
            codeBytesAllocated += size;
        }

        return v8AllocMemoryBlock(size, exec);
    };

    /**
    Write a byte to a memory block.
    */
    var writeToMemoryBlock = function (blockObj, index, byteVal)
    {
        assert (
            byteVal >= 0 && byteVal <= 255,
            "byte value '" + byteVal + "' out of range"
        );

        blockObj[index] = byteVal;
    };

    /**
    Read a byte from a memory block.
    */
    var readFromMemoryBlock = function (blockObj, index)
    {
        return blockObj[index];
    };

    /**
     Get memory block size
     */
    var getMemoryBlockSize = function (blockObj)
    {
        return blockObj.length;
    };
}

