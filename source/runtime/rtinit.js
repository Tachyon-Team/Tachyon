/**
@fileOverview
Low-level code to initialize the runtime.

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010 Maxime Chevalier-Boisvert, All Rights Reserved
*/

/**
Allocate and initialize a context object and a global object on the heap
@param heapPtr pointer to the start of the heap
*/
function initHeap(heapPtr)
{
    "tachyon:static";
    "tachyon:arg heapPtr rptr";
    "tachyon:ret rptr";

    // Treat first address as the address of context object and initialize
    // the allocation pointer
    set_ctx_allocptr(heapPtr, heapPtr);

    // Allocate the context object, incrementing the allocation pointer
    var ctxObj = alloc_ctx();

    // Allocate the global object
    var globalObj = newObject(null);

    // TODO: allocate object prototype object
    // set global object proto
    // set reference in context

    // Set the global object reference in the context object
    set_ctx_globalobj(ctxObj, globalObj);

    // Return a pointer to the context object
    return ctxObj;
}

/**
Function to allocate/get a reference to a string object containing the
given string data
@param strData pointer to raw UTF-16 string data
*/
function getStringObj(strData, strLen)
{
    "tachyon:static";
    "tachyon:arg strData rptr";
    "tachyon:arg strLen pint";

    // Allocate a string object
    var strObj = alloc_str(strLen);

    // Set the string length in the string object
    set_str_len(strObj, strLen);

    // Initialize the hash code to 0
    var hashCode = iir.constant(IRType.pint, 0);

    // For each character, update the hash code
    for (
        var index = iir.constant(IRType.pint, 0);; 
        index = index + iir.constant(IRType.pint, 1)
    )
    {
        // Get the current character
        var ch = iir.load(IRType.u16, strData, index);

        // Copy the character into the string object
        set_str_data(str, i, ch);

        // Convert the character value to the pint type
        var ch = iir.icast(IRType.pint, ch);

        // If this is the null terminator, break out of the loop
        if (ch == iir.constant(IRType.pint, 0))
            break;

        // Update 
        hashCode =
            (hashCode * iir.constant(IRType.pint, 256) + ch) %
            iir.constant(IRType.pint, 426870919);
    }

    // Set the hash code in the string object
    set_str_hash(strObj, iir.icast(IRType.i32, hashCode));

    // Return a reference to the string object
    return strObj;
}

