function putPropVal2(obj, propName, propVal)
{
    "tachyon:noglobal";

    // TODO: throw error if not object
    // - Maybe not, should never happen in practice... toObject
    // - What we actually want is a debug assertion

    // Get the hash code for the property
    // Boxed value, may be a string or an int
    var propHash = getHash(propName);

    printInt(propName);
    printInt(boxInt(propHash));

    // Set the property on the object
    putProp(obj, propName, propHash, propVal);
}

function getPropVal2(obj, propName)
{
    "tachyon:noglobal";

    // Get the hash code for the property
    // Boxed value, may be a string or an int
    var propHash = getHash(propName);

    printInt(propName);
    printInt(boxInt(propHash));

    // Attempt to find the property on the object
    var prop = getProp(obj, propName, propHash);

    printInt(prop);

    // If the property isn't defined
    if (iir.icast(IRType.pint, prop) == BIT_PATTERN_NOT_FOUND)
    {
        // Return the undefined value
        return UNDEFINED;
    }

    return prop;
}

function testObjs()
{
    //"tachyon:noglobal";
    //"tachyon:ret box";

    var obj = newObject(UNDEFINED);

    // One of these increments the numprops count, but sets no property,
    // the other sets the property in the wrong place but does not
    // increment the count...

    // TODO: look at assembler listing?

    putPropVal2(obj, 0, 35);

    return getPropVal2(obj, 0);


    // TODO: test putProp, getProp

    //obj[0] = 33;

    //var hashtbl = get_obj_tbl(obj);
    //return get_hashtbl_tbl_val(hashtbl, pint(2));

    /*Works properly:
    var hashtbl = get_obj_tbl(obj);
    set_hashtbl_tbl_key(hashtbl, pint(0), 0);
    set_hashtbl_tbl_val(hashtbl, pint(0), 33);
    */

    /*Doesn't work correctly:
    putPropVal(obj, 0, 33);
    */

    /*Works properly:
    putProp(obj, 0, pint(0), 33);
    */

    //var propHash = getHash(0);
    //putProp(obj, 0, propHash, 33);

    /*
    56: 25
    64: 25
    72: 25
    80: 25
    88: 25
    96: 25
    104: 25
    112: 25
    120: 25
    128: 25
    136: 25

    obj[0] = 33:
    73: 132

    obj[1] = 33:
    69: 4
    73: 132
    */






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

