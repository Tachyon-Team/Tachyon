function testObjs()
{
    "tachyon:noglobal";
    "tachyon:ret box";

    var obj = newObject(UNDEFINED);

    const NUM_PROPS = 5;

    for (var i = 0; i < NUM_PROPS; ++i)
    {
        __putPropVal(obj, i, i);
    }

    for (var i = 0; i < NUM_PROPS; ++i)
    {
        __getPropVal(obj, i);
    }
}

function proxy()
{
    "tachyon:ret box";

    var ptrVal = 
        (pint(b3) << pint(24)) + 
        (pint(b2) << pint(16)) + 
        (pint(b1) << pint(8)) + 
        pint(b0);

    var ptr = iir.icast(IRType.rptr, ptrVal);

    var ctx = iir.get_ctx();

    // Copy values from the old context object
    iir.store(IRType.pint, ptr, pint(0), iir.load(IRType.pint, ctx, pint(0)));
    iir.store(IRType.pint, ptr, pint(4), iir.load(IRType.pint, ctx, pint(4)));
    iir.store(IRType.pint, ptr, pint(8), iir.load(IRType.pint, ctx, pint(8)));
    iir.store(IRType.pint, ptr, pint(12), iir.load(IRType.pint, ctx, pint(12)));

    iir.set_ctx(ptr);

    set_ctx_allocptr(ptr, ptr + get_size_ctx());

    return testObjs();
}

return proxy();

