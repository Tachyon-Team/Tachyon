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
    "tachyon:arg alignBytes pint";
    "tachyon:ret rptr";

    alignBytes = iir.icast(IRType.puint, alignBytes);

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
Copy a block of memory
*/
function memCopy(dst, src, size)
{
    "tachyon:static";
    "tachyon:noglobal";
    "tachyon:arg src rptr";
    "tachyon:arg dst rptr";
    "tachyon:arg size pint";

    // Copy the object byte by byte
    for (var i = pint(0); i < size; ++i)
    {
        var b = iir.load(IRType.u8, src, i);
        iir.store(IRType.u8, dst, i, b);
    }
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

    // Get the current allocation pointer
    var freePtr = get_ctx_freeptr(ctx);

    // Align the allocation pointer
    freePtr = alignPtr(freePtr, HEAP_ALIGN);

    // Compute the next allocation pointer
    var nextPtr = freePtr + size;

    // Get the heap limit pointer
    var heapLimit = get_ctx_heaplimit(ctx);

    //printInt(iir.icast(IRType.pint, size));
    //printPtr(nextPtr);
    //printPtr(heapLimit);

    // TODO: align before allocation?

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

    iir.trace_print('entering gcCollect');

    // Get a reference to the context object (from-space)
    var ctx = iir.get_ctx();

    // Update the garbage collection count
    set_ctx_gccount(ctx, get_ctx_gccount(ctx) + u32(1));

    // Get the current heap parameters (from-space)
    var fromStart = get_ctx_heapstart(ctx);
    var fromLimit = get_ctx_heaplimit(ctx);

    // Compute the current heap size (from-space)
    var fromSize = fromLimit - fromStart;

    // Allocate a memory block for the to-space
    var toStart = malloc(fromSize);

    // Compute the to-space heap limit
    var toLimit = toStart + fromSize;

    // Set the to-space heap parameters in the context
    set_ctx_tostart(ctx, toStart);
    set_ctx_tolimit(ctx, toLimit);
    set_ctx_tofree(ctx, toStart);

    iir.trace_print('visiting context roots');

    // Visit the stack roots
    gc_visit_ctx(ctx);

    iir.trace_print('visiting stack roots');

    // Get the current return address and stack base pointer
    var ra = iir.get_ra();
    var bp = iir.get_bp();

    // Visit the stack roots
    visitStackRoots(ra, bp);

    iir.trace_print('scanning to-space');

    // Scan Pointer: All objects behind it (i.e. to its left) have been fully
    // processed; objects in front of it have been copied but not processed.
    // Free Pointer: All copied objects are behind it; Space to its right is free

    // Initialize the scan pointer at the to-space heap start
    var scanPtr = toStart;

    // Until the to-space scan is complete
    for (var numObjs = pint(0);; ++numObjs)
    {
        iir.trace_print('scanning object');

        // Get the current free pointer
        var freePtr = get_ctx_tofree(ctx);

        //printPtr(scanPtr);
        //printPtr(freePtr);

        // If we are past the free pointer, scanning done
        if (scanPtr >= freePtr)
            break;

        // Get the current object reference
        var objPtr = alignPtr(scanPtr, HEAP_ALIGN);
        var objRef = iir.icast(IRType.ref, objPtr);        

        iir.trace_print('begin visit');

        // Visit the object layout, forward its references
        gc_visit_layout(objRef);

        iir.trace_print('end visit');
      
        // If the object is a function/closure
        if (get_layout_header(objRef) === TYPEID_CLOS)
        {
            iir.trace_print('closure object');

            // Get the function pointer
            var funcPtr = get_clos_funcptr(boxRef(objRef, TAG_FUNCTION));

            // Visit the machine code block
            gcVisitMCB(funcPtr);
        }

        // Get the object size
        var objSize = sizeof_layout(objRef);

        // Move to the next object
        scanPtr = objPtr + objSize;
    }

    iir.trace_print('objects copied/scanned:');
    printInt(numObjs);

    iir.trace_print('flipping from-space and to-space');

    // Flip the from-space and to-space
    // Set the heap start, limit and free pointers in the context
    set_ctx_heapstart(ctx, toStart);
    set_ctx_heaplimit(ctx, toLimit);
    set_ctx_freeptr(ctx, get_ctx_tofree(ctx));

    // Clear the to-space information
    set_ctx_tostart(ctx, NULL_PTR);
    set_ctx_tolimit(ctx, NULL_PTR);
    set_ctx_tofree(ctx, NULL_PTR);

    iir.trace_print('freeing original to-space block');

    // FIXME: must fix the context issue for this to work
    // TODO: Free the from-space heap block
    //free(fromStart);

    iir.trace_print('leaving gcCollect');
}

/**
Copy a live object into the to-space.
*/
function gcCopy(ref, size, align)
{
    "tachyon:static";
    "tachyon:noglobal";
    "tachyon:arg ref ref";
    "tachyon:arg size pint";
    "tachyon:arg align pint";    
    "tachyon:ret ref";

    var objPtr = iir.icast(IRType.rptr, ref);

    iir.trace_print('copying object:');
    printPtr(objPtr);

    assert (
        ptrInHeap(objPtr) === true,
        'gcCopy: object not in heap'
    );

    // Get the context pointer
    var ctx = iir.get_ctx();

    // Get the allocation pointer and the heap limit
    var freePtr = get_ctx_tofree(ctx);
    var heapLimit = get_ctx_tolimit(ctx);

    // Align the free pointer
    freePtr = alignPtr(freePtr, align);

    // Use the free pointer as the new object address
    var newAddr = freePtr;

    // Increment the free pointer
    freePtr += size;

    assert (
        freePtr < heapLimit,
        'cannot copy in to-space, heap limit exceeded'
    );

    // Copy the object to the to-space
    memCopy(newAddr, iir.icast(IRType.rptr, ref), size);

    // Update the free pointer in the context
    set_ctx_tofree(ctx, freePtr);

    //iir.trace_print('object copied, free ptr:');
    //printPtr(freePtr);

    // Write the forwarding pointer in the object
    set_layout_next(ref, newAddr);

    //iir.trace_print('copying done');

    // Return the to-space pointer
    return iir.icast(IRType.ref, newAddr);
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

    var ctx = iir.get_ctx();

    // Get the to-space heap extents
    var toStart = get_ctx_tostart(ctx);
    var toLimit = get_ctx_tolimit(ctx);

    // Get the forwarding pointer in the object
    var nextPtr = get_layout_next(ref);

    // If the object is already forwarded
    if (nextPtr >= toStart && nextPtr < toLimit)
    {
        // Return the forwarding pointer
        return iir.icast(IRType.ref, nextPtr);
    }
    else
    {
        // Copy the object into the to-space
        return gcCopy(ref, sizeof_layout(ref), HEAP_ALIGN);
    }
}

/**
Function to test if a pointer points inside the heap
*/
function ptrInHeap(ptr)
{
    "tachyon:arg ptr rptr";

    var ctx = iir.get_ctx();
    
    // Get the from and to-space heap extents
    var fromStart = get_ctx_heapstart(ctx);
    var fromLimit = get_ctx_heaplimit(ctx);
    var toStart = get_ctx_tostart(ctx);
    var toLimit = get_ctx_tolimit(ctx);

    /*
    printPtr(ptr);
    printPtr(fromStart);
    printPtr(fromLimit);
    printPtr(toStart);
    printPtr(toLimit);
    */

    return (
        (ptr >= fromStart && ptr < fromLimit) ||
        (ptr >= toStart && ptr < toLimit)
    );
}

/**
Walk the stack and forward references to the to-space
*/
function visitStackRoots(ra, bp)
{
    "tachyon:static";
    "tachyon:noglobal";
    "tachyon:arg ra rptr";
    "tachyon:arg bp rptr";

    // Size of a stack slot
    const slotSize = PTR_NUM_BYTES;

    iir.trace_print('** starting stack walk');

    assert (
        ra !== NULL_PTR,
        'invalid return address'
    );

    assert (
        bp !== NULL_PTR,
        'invalid base pointer'
    );

    // Loop variable declarations
    var sp = NULL_PTR;
    var frameSize = pint(0);

    // For each stack level
    for (var numLevels = 0;; ++numLevels)
    {
        iir.trace_print('* stack frame');

        // Note: 8 byte offset to data

        // Read the magic code
        var magic = iir.icast(IRType.pint, iir.load(IRType.u16, ra, pint(8)));

        // If the magic code doesn't match, stop the traversal
        if (magic !== pint(1337))
        {
            iir.trace_print("magic does't match");
            break;
        }

        // Read the padding space for this call
        var padSpace = iir.icast(IRType.pint, iir.load(IRType.u16, ra, pint(10)));

        // Read the number of stack slots
        var numSlots = iir.icast(IRType.pint, iir.load(IRType.u16, ra, pint(12)));

        // Read the return address slot index
        var raSlot = iir.icast(IRType.pint, iir.load(IRType.u16, ra, pint(14)));

        // Compute the size of this frame
        var frameSize = numSlots * slotSize;

        //print('pad space : ' + boxInt(padSpace));
        printBox('num slots : ' + boxInt(numSlots));
        printBox('ra slot   : ' + boxInt(raSlot));
        //print('frame size: ' + boxInt(frameSize));

        // If this frame uses dynamic alignment
        if (padSpace === pint(0xFFFF))
        {
            iir.trace_print('dynamic alignment');

            // Load the sp at the base pointer
            var sp = iir.load(IRType.rptr, bp, pint(0));
        }
        else
        {
            // Compute the sp at the moment of the call
            var sp = bp + padSpace;
        }

        var kindByte = pint(0);

        // For each stack slot, from top to bottom
        for (var i = pint(0); i < numSlots; ++i)
        {
            if (i % pint(4) === pint(0))
            {
                var offset = pint(16) + (i / pint(4));
                kindByte = iir.load(IRType.u8, ra, offset);
                kindByte = iir.icast(IRType.pint, kindByte);
            }

            var kind = kindByte & pint(3);
            kindByte >>>= pint(2);

            //print('slot idx : ' + boxInt(i));
            //print('slot kind: ' + boxInt(kind));

            // Compute the displacement for this slot
            var disp = i * slotSize;

            // Function pointer
            if (kind === pint(1))
            {
                iir.trace_print('fptr');

                // Read the function pointer
                var funcPtr = iir.load(IRType.rptr, sp, disp);

                // Visit the function's machine code block
                gcVisitMCB(funcPtr);
            }

            // Ref
            if (kind === pint(2))
            {
                iir.trace_print('ref');

                // Read the reference from the stack
                var refVal = iir.load(IRType.ref, sp, disp);

                assert (
                    ptrInHeap(iir.icast(IRType.rptr, refVal)) === true,
                    'ref val points out of heap'
                );

                // Get a forwarded reference in the to-space
                var newRef = gcForward(refVal);

                // Update the reference on the stack
                iir.store(IRType.ref, sp, disp, newRef);
            }

            // Box
            else if (kind === pint(3))
            {
                //iir.trace_print('box');

                // Read the value from the stack
                var boxVal = iir.load(IRType.box, sp, disp);

                //print('box val: ' + val);

                // If the boxed value is a reference
                if (boxIsRef(boxVal) === true)
                {
                    iir.trace_print('boxed ref');

                    // Unbox the reference
                    var refVal = unboxRef(boxVal);
                    var refTag = getRefTag(boxVal);

                    assert (
                        ptrInHeap(iir.icast(IRType.rptr, refVal)) === true,
                        'ref val points out of heap'
                    );

                    // Get a forwarded reference in the to-space
                    var newRef = gcForward(refVal);

                    // Rebox the reference value
                    var newBox = boxRef(newRef, refTag);

                    // Update the boxed value on the stack
                    iir.store(IRType.box, sp, disp, newBox);
                }
            }
        }

        // Load the return address for the next frame down
        ra = iir.load(IRType.rptr, sp, raSlot * slotSize);

        // Compute the base pointer for this frame
        bp = sp + frameSize;
    }
}

/**
Visit a machine code block and its references
*/
function gcVisitMCB(funcPtr)
{
    "tachyon:static";
    "tachyon:noglobal";
    "tachyon:arg funcPtr rptr";

    iir.trace_print('visiting mcb');
    printPtr(funcPtr);

    // Get the address of the machine code block start
    var mcbPtr = funcPtr - MCB_HEADER_SIZE;

    // Get the garbage collection count
    var ctx = iir.get_ctx();
    var gcCount = get_ctx_gccount(ctx);

    var lastCol = iir.load(IRType.u32, mcbPtr, pint(0));

    // If this block has been visited in this collection, stop
    if (lastCol === gcCount)
    {
        iir.trace_print('already visited');
        return; 
    }

    // Mark the block as visited
    iir.store(IRType.u32, mcbPtr, pint(0), gcCount);

    // Load the offset to the ref entries
    var dataOffset = iir.icast(IRType.pint, iir.load(IRType.u32, mcbPtr, pint(4)));

    // Load the number of ref entries
    var numEntries = iir.icast(IRType.pint, iir.load(IRType.u32, mcbPtr, pint(8)));

    // For each reference entry
    for (var i = pint(0); i < numEntries; ++i)
    {
        // Load the reference offset and kind
        var offset = iir.icast(IRType.pint, iir.load(IRType.u32, mcbPtr, dataOffset + i * MCB_REF_ENTRY_SIZE));
        var kind = iir.load(IRType.u32, mcbPtr, dataOffset + i * MCB_REF_ENTRY_SIZE + pint(4));

        // Function pointer
        if (kind === u32(1))
        {
            iir.trace_print('fptr');

            // Read the function pointer
            var funcPtr = iir.load(IRType.rptr, mcbPtr, offset);

            // Visit the function's machine code block
            gcVisitMCB(funcPtr);
        }

        // Ref
        if (kind === u32(2))
        {
            iir.trace_print('ref');

            // Read the reference from the stack
            var refVal = iir.load(IRType.ref, mcbPtr, offset);

            assert (
                ptrInHeap(iir.icast(IRType.rptr, refVal)) === true,
                'ref val points out of heap'
            );

            // Get a forwarded reference in the to-space
            var newRef = gcForward(refVal);

            // Update the reference on the stack
            iir.store(IRType.ref, mcbPtr, offset, newRef);
        }

        // Box
        else if (kind === u32(3))
        {
            //iir.trace_print('box');

            // Read the value from the stack
            var boxVal = iir.load(IRType.box, mcbPtr, offset);

            //print('box val: ' + val);

            // If the boxed value is a reference
            if (boxIsRef(boxVal) === true)
            {
                iir.trace_print('boxed ref');

                // Unbox the reference
                var refVal = unboxRef(boxVal);
                var refTag = getRefTag(boxVal);

                assert (
                    ptrInHeap(iir.icast(IRType.rptr, refVal)) === true,
                    'ref val points out of heap'
                );

                // Get a forwarded reference in the to-space
                var newRef = gcForward(refVal);

                // Rebox the reference value
                var newBox = boxRef(newRef, refTag);

                // Update the boxed value on the stack
                iir.store(IRType.box, mcbPtr, offset, newBox);
            }
        }
    }
}

