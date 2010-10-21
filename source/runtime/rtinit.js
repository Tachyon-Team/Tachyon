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
function getStringObj(strData)
{
    "tachyon:static";
    "tachyon:arg strData rptr";

    // Allocate a string object
    var strObj = alloc_str();


    //
    // TODO
    //

    // Set:
    // len
    // hash
    // data

    // TODO: convert computeHash to use str data?
    // TODO: want getHash function for hash table use...

    //var hashCode = computeHash(strObj);


    // Return a reference to the string object
    return strObj;
}

