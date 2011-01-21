function testStrs()
{
    "tachyon:noglobal";

    initStrTable();

    var ctxPtr = iir.get_ctx();

    var rawStr = ctxPtr + pint(8192);

    var strLen = pint(4);

    iir.store(IRType.u16, rawStr, pint(0), u16(104));    // h
    iir.store(IRType.u16, rawStr, pint(2), u16(105));    // i
    iir.store(IRType.u16, rawStr, pint(4), u16(33));     // !
    iir.store(IRType.u16, rawStr, pint(6), u16(0));      // \0

    var str = getStrObj(rawStr, strLen);

    printInt(1337);



}

function testObjs()
{
    "tachyon:noglobal";

    var obj = newObject(null);

    const NUM_PROPS = 13;

    for (var i = 0; i < NUM_PROPS; ++i)
    {
        __putPropVal(obj, i, i);
    }

    for (var i = 0; i < NUM_PROPS; ++i)
    {
        __putPropVal(obj, i, 2*i);
    }

    for (var i = 0; i < NUM_PROPS; ++i)
    {
        printInt(__getPropVal(obj, i));
    }

    var pv = __getPropVal(obj, 55);

    if (pv === UNDEFINED)
        printInt(1337);
    else
        printInt(boxInt(iir.icast(IRType.pint, pv)));
}

function proxy(ptr)
{
    "tachyon:arg ptr rptr";
    "tachyon:ret box";

    var ctx = iir.get_ctx();

    // Copy values from the old context object
    iir.store(IRType.pint, ptr, pint(0), iir.load(IRType.pint, ctx, pint(0)));
    iir.store(IRType.pint, ptr, pint(4), iir.load(IRType.pint, ctx, pint(4)));
    iir.store(IRType.pint, ptr, pint(8), iir.load(IRType.pint, ctx, pint(8)));
    iir.store(IRType.pint, ptr, pint(12), iir.load(IRType.pint, ctx, pint(12)));

    iir.set_ctx(ptr);

    set_ctx_allocptr(ptr, ptr + get_size_ctx());

    testStrs();

    //testObjs();

    return 31337;
}

//return proxy();

