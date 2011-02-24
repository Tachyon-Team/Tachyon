/**
@fileOverview
Runtime functions needed for FFI interfacing.

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010-2011 Maxime Chevalier-Boisvert, All Rights Reserved
*/

/**
Create/allocate a C (UTF-8) string from a string object.
*/
function boxToCString(strVal)
{
    "tachyon:static";
    "tachyon:noglobal";
    "tachyon:ret rptr";

    // Get the string length
    var strLen = iir.icast(IRType.pint, get_str_len(strVal));

    // Allocate memory for the C string
    var strPtr = malloc(strLen + pint(1));

    // For each character
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

/**
Create a string object from a C (UTF-8) string pointer.
*/
function cStringToBox(strPtr)
{
    "tachyon:static";
    "tachyon:noglobal";
    "tachyon:arg strPtr rptr";

    // Compute the string length
    for (var strLen = pint(0); ; strLen++)
    {
        var ch = iir.load(IRType.i8, strPtr, strLen);

        if (ch === i8(0))
            break;
    }

    // Allocate a string object
    var strObj = alloc_str(strLen);
    
    // Set the string length in the string object
    set_str_len(strObj, iir.icast(IRType.u32, strLen));

    // For each character
    for (var i = pint(0); i < strLen; i++)
    {
        var cCh = iir.load(IRType.i8, strPtr, i);

        var ch = iir.icast(IRType.u16, cCh);

        set_str_data(strObj, i, ch);
    }

    // Compute the hash code for the new string
    compStrHash(strObj);

    // Attempt to find the string in the string table
    return getTableStr(strObj);
}

/**
Convert a raw pointer to a byte array
*/
function ptrToByteArray(ptr)
{
    "tachyon:static";
    "tachyon:noglobal";
    "tachyon:arg ptr rptr";

    var array = [];

    var ptrInt = iir.icast(IRType.pint, ptr);

    for (var i = 0; i < boxInt(PTR_NUM_BYTES); ++i)
    {
        var byteVal = ptrInt % pint(256);

        ptrInt /= pint(256);

        array[i] = boxInt(byteVal);
    }

    return array;
}

