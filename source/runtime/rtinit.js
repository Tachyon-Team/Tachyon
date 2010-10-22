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
Allocate/get a reference to a string object containing a given value
@param fpVal 64 bit floating-point value
*/
function getFloatObj(fpVal)
{
    "tachyon:static";
    "tachyon:arg fpVal f64";

    //
    // TODO
    //
}

