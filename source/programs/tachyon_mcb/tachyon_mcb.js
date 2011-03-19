function foo(v)
{
    var u8v = u8(v);

    var pintv = iir.icast(IRType.pint, u8v);

    return boxInt(pintv);
}

function test()
{
    var testVals = [0, 15, 152, 223, 255, 243, 0, 13];

    var mcb = allocMemoryBlock(testVals.length, true);

    var addr = getBlockAddr(mcb, 0);

    if (addr.length !== boxInt(PTR_NUM_BYTES))
        return 1;

    for (var i = 0; i < testVals.length; ++i)
    {
        writeToMemoryBlock(mcb, i, testVals[i]);
    }

    for (var i = 0; i < testVals.length; ++i)
    {
        var byteVal = readFromMemoryBlock(mcb, i);

        if (byteVal !== testVals[i])
        {
            print(
                'Got value ' + byteVal + ' for byte #' + i + 
                ', expected ' + testVals[i]
            );

            return 2;
        }
    }

    freeMemoryBlock(mcb);

    return 0;
}

