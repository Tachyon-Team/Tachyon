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
    var heapPtrInt = iir.icast(IRType.pint, heapPtr);
    var ctxPtrInt = (heapPtrInt % CTX_ALIGN) === pint(0)?  
                    heapPtrInt : 
                    (heapPtrInt - (heapPtrInt % CTX_ALIGN));
    var ctxPtr = iir.icast(IRType.rptr, ctxPtrInt);

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

    // TODO: allocate object prototype object
    // set global object proto
    // set boxed reference in context
    // TODO: allocate other basic objects
    // Possibly, this should be done elsewhere, in a later initialization phase
    // However, it must be done before code that can throw JS exceptions is run

    // Set the global object reference in the context object
    set_ctx_globalobj(ctxObj, globalObj);

    // Initially, set the prototype references to null
    set_ctx_objproto(ctxObj, null);
    set_ctx_arrproto(ctxObj, null);
    set_ctx_funcproto(ctxObj, null);
    set_ctx_strproto(ctxObj, null);

    // Initialize the string table
    initStrTable();

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

