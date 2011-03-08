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
function initHeap(heapPtr, heapSize)
{
    "tachyon:static";
    "tachyon:noglobal";
    "tachyon:arg heapPtr rptr";
    "tachyon:arg heapSize pint";
    "tachyon:ret rptr";

    // Align the context object in memory
    var ctxPtr = alignPtr(heapPtr, CTX_ALIGN);

    // Treat first address as the address of context object and initialize
    // the allocation pointer
    iir.set_ctx(ctxPtr);
    set_ctx_allocptr(ctxPtr, ctxPtr);

    // Set the heap pointer and heap limit
    set_ctx_heapstart(ctxPtr, heapPtr);
    set_ctx_heaplimit(ctxPtr, heapPtr + heapSize);

    // Allocate the context object, incrementing the allocation pointer
    var ctxObj = alloc_ctx();

    // Allocate the global object
    var globalObj = newObject(null);

    // Set the global object reference in the context object
    set_ctx_globalobj(ctxObj, globalObj);

    // Initialize the string hash consing system
    initStrings();

    // Return a pointer to the context object
    return ctxObj;
}

/**
Allocate/get a reference to a float object containing a given value
@param fpVal 64 bit floating-point value
*/
function getFloatObj(fpVal)
{
    "tachyon:static";
    "tachyon:noglobal";
    "tachyon:arg fpVal f64";

    //
    // TODO: allocate a float object and return it
    //
}

