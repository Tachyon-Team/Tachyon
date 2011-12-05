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
    var freePtr = get_ctx_freeptr(ctx);

    // Compute the next allocation pointer
    var nextPtr = freePtr + size;

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
        freePtr = get_ctx_freeptr(ctx);

        // Compute the next allocation pointer
        nextPtr = freePtr + size;

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
    set_ctx_freeptr(ctx, nextPtr);

    // Allocate the object at the current position
    return freePtr;
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
    freeptr
    scanptr
    */

    "tachyon:static";
    "tachyon:noglobal";

    printBox('entering gcCollect');

    // Get a reference to the context object (from-space)
    var fromCtx = iir.get_ctx();

    // Get the current heap parameters (from-space)
    var fromStart = get_ctx_heapstart(fromCtx);
    var fromLimit = get_ctx_heaplimit(fromCtx);

    // Compute the current heap size (from-space)
    var fromSize = fromLimit - fromStart;

    // Allocate a memory block for the to-space
    var toStart = malloc(fromSize);

    // Compute the to-space heap limit
    var toLimit = toStart + fromSize;

    // FIXME: gcCopy will modify the current context object
    // Copy the context to the to-space
    //var toCtx = gcCopy(fromCtx, comp_size_ctx());

    // Set the heap start and limit in the to-space context
    //set_ctx_heapstart(toCtx, toStart);
    //set_ctx_heaplimit(toCtx, toLimit);

    // TODO: Update the context pointer
    // iir.set_ctx(toCtx);







    // TODO: breadth-first traversal, starting from the roots
    // When an object is copied, a forwarding pointer is kept in place of the
    // old copy.






    // TODO: scan to-space, update references
    // Scan Pointer: All objects behind it (i.e. to its left) have been fully
    // processed; objects in front of it have been copied but not processed.
    // Free Pointer: All copied objects are behind it; Space to its right is free

    // Set the scan pointer at the to-space heap start
    //set_ctx_scanptr(toCtx, toStart);






    // TODO: flip from-space, to-space
    // TODO: free from-space block


    printBox('leaving gcCollect');
}

/**
Copy a live object into the to-space.
*/
function gcCopy(ref, size)
{
    "tachyon:static";
    "tachyon:noglobal";
    "tachyon:arg ref ref";
    "tachyon:arg size pint";
    "tachyon:ret ref";

    /*
        addr = free
        move(P,free)
        free = free + size(P)
        forwarding_address(P) = addr
        return addr
    */

    //var freePtr = 

    // Use the free pointer as the new object address
    // TODO: alignment considerations
    //var newAddr = iir.icast(IRType.ref, freePtr);

    // Increment the free pointer
    //freePtr += size;

    // TODO: copy num bytes


    // TODO: update free pointer in ctx


    // TODO: write forwarding pointer in the object



    // TODO: return the to-space pointer
    return ref;
}

/**
Function to forward a memory object. The argument is an unboxed reference.
*/
function gcForward(ref)
{
    "tachyon:static";
    "tachyon:noglobal";
    "tachyon:arg ref ref";
    "tachyon:ret ref";


    /*
    if forwarded(P)
        return forwarding_address(P)
    else
        addr = free
        move(P,free)
        free = free + size(P)
        forwarding_address(P) = addr
        return addr
    */

    // TODO








    return ref;
}

/**
Function to test if a pointer points inside the heap
*/
function ptrInHeap(ptr)
{
    "tachyon:arg ptr rptr";

    // TODO


}

