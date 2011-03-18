// Dummy IRValue class
function IRValue()
{
}

function test()
{
    var ctxPtr = [];
    for (var i = 0; i < boxInt(PTR_NUM_BYTES); ++i)
        ctxPtr.push(0);
    print('ctx ptr: ' + ctxPtr);

    var funcPtr = getFuncAddr('testCallFFI');
    print('func ptr: ' + funcPtr);

    //callTachyonFFI = function (cArgTypes, cRetType, funcPtrBytes, ctxPtrBytes)

    var heapSize = 512;
    var heapBlock = allocMemoryBlock(256, false);
    var heapAddr = getBlockAddr(heapBlock, 0);

    print('heap size: ' + heapSize);
    print('heap ptr: ' + heapAddr);

    var ret = callTachyonFFI.apply(
        null,
        [
            ['void*', 'int'],
            'void*',
            funcPtr, ctxPtr
        ].concat([heapAddr, heapSize])
    );

    print('ret: ' + ret);

    freeMemoryBlock(heapBlock);

    return 0;
}

