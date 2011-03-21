(function ()
{
    "tachyon:noglobal";

    var ctx = iir.get_ctx();
    var globalObj = get_ctx_globalobj(ctx);
    var strTbl = get_ctx_strtbl(ctx);

    printPtr(iir.icast(IRType.rptr, ctx));
    printPtr(iir.icast(IRType.rptr, unboxRef(globalObj)));
    printPtr(iir.icast(IRType.rptr, unboxRef(strTbl)));

    var numGlobals = get_obj_numprops(globalObj);
    printInt(iir.icast(IRType.pint, numGlobals));

    var numStrings = get_strtbl_numstrs(strTbl);
    printInt(iir.icast(IRType.pint, numStrings));

    putPropVal(globalObj, 1, 1337);
    var propVal = getPropVal(globalObj, 1);
    printInt(unboxInt(propVal));

    var intStr = getIntStr(pint(777));
    puts(intStr);

    var str = 'Hello World!';
    var strPtr = iir.icast(IRType.rptr, unboxRef(str));
    printPtr(strPtr);
    var strLen = iir.icast(IRType.pint, get_str_size(str));
    printInt(strLen);
    puts(str);

    var str2 = 'Hello ' + 'World!';
    if (str === str2)
        printInt(pint(333));
    else
        printInt(pint(000));

    putPropVal(globalObj, str, 13372);
    var propVal = getPropVal(globalObj, str);
    printInt(unboxInt(propVal));

})();
