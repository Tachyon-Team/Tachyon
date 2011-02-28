/**
@fileOverview
Runtime functions needed for FFI interfacing.

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010-2011 Maxime Chevalier-Boisvert, All Rights Reserved
*/

// If we are running inside Tachyon
if (config.inTachyon)
{
    /**
    Machine code block wrapper object
    */
    function MCBWrapper(mcbObj)
    {
        assert (
            boolToBox(getRefTag(mcbObj) === TAG_OTHER),
            'invalid mcb reference'
        );

        this.mcb = mcbObj;
    }

    /**
    Allocate a machine code block.
    */
    var allocMachineCodeBlock = function (size)
    {
        "tachyon:noglobal";

        var blockPtr = rawAllocMachineCodeBlock(unboxInt(size));

        var blockObj = alloc_memblock();

        set_memblock_ptr(blockObj, blockPtr);
        set_memblock_size(blockObj, u32(size));

        return new MCBWrapper(blockObj);
    };

    /**
    Free a machine code block.
    */
    var freeMachineCodeBlock = function (blockObj)
    {
        "tachyon:noglobal";

        var mcb = blockObj.mcb;

        assert (
            boolToBox(getRefTag(mcb) === TAG_OTHER),
            'invalid mcb reference'
        );

        var ptr = get_memblock_ptr(mcb);
        var size = iir.icast(IRType.pint, get_memblock_size(mcb));

        rawFreeMachineCodeBlock(ptr, size);
    };

    /**
    Get the address of an offset into the block, expressed
    as an array of bytes.
    */
    var getBlockAddr = function (blockObj, index)
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
            boolToBox(unboxInt(index) < size),
            'invalid index in mcb'
        );

        var addr = ptr + unboxInt(index);

        // Convert the pointer to a byte array
        return ptrToByteArray(addr);
    };

    /**
    Write a byte to a machine code block.
    */
    var writeToMachineCodeBlock = function (blockObj, index, byteVal)
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
            boolToBox(unboxInt(index) < size),
            'invalid index in mcb'
        );

        assert (
            byteVal > 0 && byteVal <= 255,
            'byte value out of range'
        );

        // Store the value in the block
        iir.store(IRType.i8, ptr, unboxInt(index), i8(byteVal));
    };
}

// Otherwise, if we are running inside D8
else
{
    /**
    Write a byte to a machine code block.
    */
    var writeToMachineCodeBlock = function (blockObj, index, byteVal)
    {
        assert (
            byteVal >= 0 && byteVal <= 255,
            "byte value '" + byteVal + "' out of range"
        );

        blockObj[index] = byteVal;
    };
}

