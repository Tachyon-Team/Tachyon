function boxToCString(strVal)
{
    "tachyon:static";
    "tachyon:noglobal";
    "tachyon:ret rptr";

    if (strVal === null)
        return NULL_PTR;

    var strLen = iir.icast(IRType.pint, get_str_size(strVal));

    // Allocate memory for the C string
    var strPtr = malloc(strLen + pint(1));

    // Copy the characters
    for (var i = pint(0); i < strLen; i++)
    {
        var ch = get_str_data(strVal, i);
        var cCh = iir.icast(IRType.i8, ch);
        iir.store(IRType.i8, strPtr, i, cCh);
    }

    // Store the null terminator
    iir.store(IRType.i8, strPtr, i, i8(0));

    return strPtr;
}
