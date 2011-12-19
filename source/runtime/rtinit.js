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
@param heapSize size of the heap block to be allocated
*/
function initHeap(heapSize)
{
    "tachyon:static";
    "tachyon:noglobal";
    "tachyon:arg heapSize puint";
    "tachyon:ret rptr";

    // Allocate a memory block for the heap
    var heapPtr = malloc(iir.icast(IRType.pint, heapSize));

    // Allocate a context object in the C heap
    var ctxPtr = malloc(comp_size_ctx() + CTX_ALIGN);

    // Align the context object in memory
    var ctx = alignPtr(ctxPtr, CTX_ALIGN);

    // Treat first address as the address of context object
    iir.set_ctx(ctx);

    // Initialize the context
    init_ctx(ctx);

    // Compute the heap limit pointer
    var heapLimit = heapPtr + heapSize;

    //printInt(iir.icast(IRType.pint, heapSize));
    //printPtr(heapPtr);
    //printPtr(heapLimit);

    // Set the heap parameters
    set_ctx_heapsize(ctx, heapSize);
    set_ctx_heapstart(ctx, heapPtr);
    set_ctx_heaplimit(ctx, heapLimit);
    set_ctx_freeptr(ctx, heapPtr);

    //printPtr(iir.icast(IRType.rptr, iir.get_ctx()));

    assert (
        heapLimit > heapPtr,
        1
    );

    // Allocate the global object
    var globalObj = newObject(null);

    assert (
        boxIsObj(globalObj),
        2
    );

    assert (
        iir.icast(IRType.rptr, unboxRef(globalObj)) >= heapPtr,
        3
    );

    // Set the global object reference in the context object
    set_ctx_globalobj(ctx, globalObj);

    // Initialize the string table
    initStrTbl();

    // Initialize the function table
    initFuncTbl();

    // printPtr(iir.icast(IRType.rptr, ctx));
    // printPtr(iir.icast(IRType.rptr, unboxRef(globalObj)));
    // printPtr(iir.icast(IRType.rptr, unboxRef(get_ctx_strtbl(ctx))));

    // Set the undefined global property manually,
    // string constants are not available at this point
    var undefStr = alloc_str(pint(9));
    set_str_data(undefStr, pint(0), u16(117));  // u
    set_str_data(undefStr, pint(1), u16(110));  // n
    set_str_data(undefStr, pint(2), u16(100));  // d
    set_str_data(undefStr, pint(3), u16(101));  // e
    set_str_data(undefStr, pint(4), u16(102));  // f
    set_str_data(undefStr, pint(5), u16(105));  // i
    set_str_data(undefStr, pint(6), u16(110));  // n
    set_str_data(undefStr, pint(7), u16(101));  // e
    set_str_data(undefStr, pint(8), u16(100));  // d
    compStrHash(undefStr);
    undefStr = getTableStr(undefStr);
    globalObj[undefStr] = UNDEFINED;

    // Return a pointer to the context object
    return ctx;
}

/**
Initialize the function table
*/
function initFuncTbl()
{
    "tachyon:static";
    "tachyon:noglobal";

    // Allocate the function table object
    var functbl = alloc_functbl(FUNC_TBL_INIT_SIZE);

    // Initialize the number of functions
    set_functbl_numfuncs(functbl, u32(0));

    // Get a pointer to the context
    var ctx = iir.get_ctx();

    // Set the function table reference in the context
    set_ctx_functbl(ctx, functbl);
}

/**
Register a function in the function table
*/
function regFunction(funcPtr)
{
    "tachyon:static";
    "tachyon:noglobal";
    "tachyon:arg funcPtr rptr";

    var ctx = iir.get_ctx();
    var funcTbl = get_ctx_functbl(ctx);

    var tblSize = iir.icast(IRType.pint, get_functbl_size(funcTbl));
    var numFuncs = iir.icast(IRType.pint, get_functbl_numfuncs(funcTbl));

    // If the table needs to be extended
    if (numFuncs + pint(1) === tblSize)
    {
        var newSize = tblSize * pint(2);
        var newTbl = alloc_functbl(newSize);

        for (var i = pint(0); i < tblSize; ++i)
        {
            var f = get_functbl_tbl(funcTbl, i);
            set_functbl_tbl(newTbl, i, f);
        }

        set_ctx_functbl(ctx, newTbl);

        funcTbl = newTbl;
    }

    // Add the new function to the table
    set_functbl_tbl(funcTbl, iir.icast(IRType.pint, numFuncs), funcPtr);

    // Increment the number of functions in the table
    set_functbl_numfuncs(funcTbl, iir.icast(IRType.u32, numFuncs) + u32(1));

    return 0;
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

