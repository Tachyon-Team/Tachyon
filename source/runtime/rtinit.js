/* _________________________________________________________________________
 *
 *             Tachyon : A Self-Hosted JavaScript Virtual Machine
 *
 *
 *  This file is part of the Tachyon JavaScript project. Tachyon is
 *  distributed at:
 *  http://github.com/Tachyon-Team/Tachyon
 *
 *
 *  Copyright (c) 2011, Universite de Montreal
 *  All rights reserved.
 *
 *  This software is licensed under the following license (Modified BSD
 *  License):
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions are
 *  met:
 *    * Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *    * Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions and the following disclaimer in the
 *      documentation and/or other materials provided with the distribution.
 *    * Neither the name of the Universite de Montreal nor the names of its
 *      contributors may be used to endorse or promote products derived
 *      from this software without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 *  IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 *  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 *  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL UNIVERSITE DE
 *  MONTREAL BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 *  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 *  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 *  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 *  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 *  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 *  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * _________________________________________________________________________
 */

/**
@fileOverview
Low-level code to initialize the runtime.

@author
Maxime Chevalier-Boisvert
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

    printInt(iir.icast(IRType.pint, heapSize));
    printPtr(heapPtr);
    printPtr(heapLimit);

    // Set the heap pointer and heap limit
    set_ctx_heapstart(ctx, heapPtr);
    set_ctx_heaplimit(ctx, heapLimit);

    // Allocate the context object, incrementing the allocation pointer
    var ctx = alloc_ctx();

    assert (
        heapLimit > heapPtr,
        1
    );

    assert (
        iir.icast(IRType.rptr, ctx) >= heapPtr,
        2
    );

    // Allocate the global object
    var globalObj = newObject(null);

    assert (
        boxIsObj(globalObj),
        3
    );

    assert (
        iir.icast(IRType.rptr, unboxRef(globalObj)) > iir.icast(IRType.rptr, ctx),
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

