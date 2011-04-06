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
    "tachyon:arg heapSize puint";
    "tachyon:ret ref";

    // Align the context object in memory
    var ctxPtr = alignPtr(heapPtr, CTX_ALIGN);

    // Cast the context pointer to an unboxed reference type
    var ctx = iir.icast(IRType.ref, ctxPtr);

    // Treat first address as the address of context object and initialize
    // the allocation pointer
    iir.set_ctx(ctx);
    set_ctx_allocptr(ctx, ctxPtr);

    // Compute the heap limit pointer
    var heapLimit = heapPtr + heapSize;

    //printInt(heapSize);
    //printPtr(heapPtr);
    //printPtr(heapLimit);

    // Set the heap pointer and heap limit
    set_ctx_heapstart(ctx, heapPtr);
    set_ctx_heaplimit(ctx, heapLimit);

    // Allocate the context object, incrementing the allocation pointer
    var ctx = alloc_ctx();

    assert (
        boolToBox(heapLimit > heapPtr),
        1
    );

    assert (
        boolToBox(iir.icast(IRType.rptr, ctx) >= heapPtr),
        2
    );

    // Allocate the global object
    var globalObj = newObject(null);

    assert (
        boolToBox(boxIsObj(globalObj)),
        3
    );

    assert (
        boolToBox(iir.icast(IRType.rptr, unboxRef(globalObj)) > iir.icast(IRType.rptr, ctx)),
        4
    );

    // Set the global object reference in the context object
    set_ctx_globalobj(ctx, globalObj);

    // Initialize the string hash consing system
    initStrings();

    // printPtr(iir.icast(IRType.rptr, ctx));
    // printPtr(iir.icast(IRType.rptr, unboxRef(globalObj)));
    // printPtr(iir.icast(IRType.rptr, unboxRef(get_ctx_strtbl(ctx))));

    // Return a pointer to the context object
    return ctx;
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

