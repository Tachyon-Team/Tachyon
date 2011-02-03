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
    "tachyon:noglobal";
    "tachyon:arg heapPtr rptr";
    "tachyon:ret rptr";

    // Treat first address as the address of context object and initialize
    // the allocation pointer
    iir.set_ctx(heapPtr);
    set_ctx_allocptr(heapPtr, heapPtr);

    // Allocate the context object, incrementing the allocation pointer
    var ctxObj = alloc_ctx();

    // Allocate the global object
    var globalObj = newObject(null);

    // TODO: allocate object prototype object
    // set global object proto
    // set boxed reference in context
    // TODO: allocate other basic objects
    // Possibly, this should be done elsewhere, in a later initialization phase
    // However, it must be done before code that can throw JS exceptions is run

    // Set the global object reference in the context object
    set_ctx_globalobj(ctxObj, globalObj);

    // Initially, set the object prototype to null
    set_ctx_objproto(ctxObj, null);

    // Initialize the string table
    initStrTable();

    // Return a pointer to the context object
    return ctxObj;
}

/**
Initialize the standard library once the basic runtime components are ready.
*/
function initStdlib()
{
    "tachyon:static";

    var ctx = iir.get_ctx();

    // Set the string prototype object reference in the context
    set_ctx_strproto(ctx, String.prototype);

    // TODO: set array proto, object proto in ctx

    // TODO: set global object prototype
}

/**
Allocate/get a reference to a string object containing a given value
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

