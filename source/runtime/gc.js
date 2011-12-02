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
Allocator and garbage collector implementation.

@author
Maxime Chevalier-Boisvert
*/

/**
Align a pointer to a given number of bytes.
*/
function alignPtr(ptr, alignBytes)
{
    "tachyon:inline";
    "tachyon:noglobal";
    "tachyon:arg ptr rptr";
    "tachyon:arg alignBytes puint";
    "tachyon:ret rptr";

    // Compute the pointer modulo the given alignment boundary
    var rem = iir.icast(IRType.puint, ptr) % alignBytes;

    // If the pointer is already aligned, return it
    if (rem === puint(0))
        return ptr;

    // Pad the pointer by the necessary amount to align it
    var pad = alignBytes - rem;
    ptr += pad;

    // Return the aligned pointer
    return ptr;
}

/**
Allocate a memory block of a given size on the heap
*/
function heapAlloc(size)
{
    "tachyon:static";
    "tachyon:noglobal";
    "tachyon:arg size pint";
    "tachyon:ret rptr";

    // Get a pointer to the context
    var ctx = iir.get_ctx();

    //printPtr(iir.icast(IRType.rptr, ctx));

    // Get the current allocation pointer
    var allocPtr = get_ctx_allocptr(ctx);

    // Compute the next allocation pointer
    var nextPtr = allocPtr + size;

    // Get the heap limit pointer
    var heapLimit = get_ctx_heaplimit(ctx);

    //printInt(iir.icast(IRType.pint, size));
    //printPtr(nextPtr);
    //printPtr(heapLimit);

    // If this allocation exceeds the heap limit
    if (nextPtr >= heapLimit)
    {
        // Log that we are going to perform GC
        puts('Performing garbage collection');

        // Call the garbage collector
        gcCollect();

        // Get the new allocation pointer
        allocPtr = get_ctx_allocptr(ctx);

        // Compute the next allocation pointer
        nextPtr = allocPtr + size;

        // Get the new limit pointer
        heapLimit = get_ctx_heaplimit(ctx);

        // If this allocation still exceeds the heap limit
        if (nextPtr >= heapLimit)
        {
            // Report an error and abort
            error('allocation exceeds heap limit');
        }
    }

    // Align the next allocation pointer
    nextPtr = alignPtr(nextPtr, HEAP_ALIGN);
        
    // Update the allocation pointer in the context object
    set_ctx_allocptr(ctx, nextPtr);

    // Allocate the object at the current position
    return allocPtr;
}

/**
Perform a garbage collection
*/
function gcCollect()
{
    /*

    Cheney's Algorithm:

    flip() =
        Fromspace, Tospace = Tospace, Fromspace
        top_of_space = Tospace + space_size
        scan = free = Tospace

        for R in roots
            R = copy(R)

        while scan < free
            for P in Children(scan)
                *P = copy(*P)
            scan = scan + size (scan)

    copy(P) =
        if forwarded(P)
            return forwarding_address(P)
        else
            addr = free
            move(P,free)
            free = free + size(P)
            forwarding_address(P) = addr
            return addr

    Context variables:
    heapstart
    heaplimit
    allocptr


    TODO: try browsing objects from context, no stack roots for now.
    - Ensure no bugs
    - Collect stats

    TODO: allocate to-space heap
    - do this after root traversal works

    TODO: traverse from roots


    */

    "tachyon:static";
    "tachyon:noglobal";

    printBox('entering gcCollect');

    // Get a reference to the context object
    var ctx = iir.get_ctx();










    printBox('leaving gcCollect');
}

