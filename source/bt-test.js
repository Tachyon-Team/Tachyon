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
    puts(str);

    putPropVal(globalObj, str, 13372);
    var propVal = getPropVal(globalObj, str);
    printInt(unboxInt(propVal));

})();
