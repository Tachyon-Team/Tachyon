/**
@fileOverview
Runtime functions needed for FFI interfacing.

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010-2011 Maxime Chevalier-Boisvert, All Rights Reserved
*/

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

        var blockPtr = rawAllocMemoryBlock(unboxInt(size), boxToBool(exec));

        var blockObj = alloc_memblock();

        set_memblock_ptr(blockObj, blockPtr);
        set_memblock_size(blockObj, u32(size));

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

