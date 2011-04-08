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
Implementation of a heap data structure.

@author
Erick Lavoie

@copyright
Copyright (c) 2010 Erick Lavoie, All Rights Reserved
*/

/**
@class Heap implementation
*/
function Heap(compFunc)
{
    /**
    Comparison function used to maintain the heap property
    @field
    */
    this.compFunc = compFunc;

    /**
    Array to hold the data 
    @field
    */
    this.innerArray = [];

    /**
    Number of elements in the heap
    @field
    */
    this.length = 0;
}
Heap.prototype = {};

/**
Returns the parent index
*/
Heap.prototype.parent = function (i)
{
    return (i-1) >> 1;
};

/**
Returns the left child index
*/
Heap.prototype.left = function (i)
{
    return ((i+1) << 1) - 1;
};

/**
Returns the right child index
*/
Heap.prototype.right = function (i)
{
    return ((i+1) << 1);
};

/**
Get a string representation of the heap
*/
Heap.prototype.toString = function ()
{
    return "(" + this.innerArray.slice(0, this.length).join(",") + ")";
};

/**
Test if the heap is empty
*/
Heap.prototype.isEmpty = function ()
{
    return this.length === 0;
};

/**
Clear all the heap contents
*/
Heap.prototype.clear = function ()
{
    this.innerArray = [];
    this.length = 0;
};

/**
Maintains the heap property
*/
Heap.prototype.heapify = function (i)
{
    // Done for a max heap but
    // inverting the compFunc makes it work
    // for a min heap 
    const size = this.length;
    const a = this.innerArray;

    function exchange(i1, i2)
    {
        var temp = a[i1];
        a[i1] = a[i2];
        a[i2] = temp;
    }

    var root = i;
    var largest = -1;

    while (true)
    {
        var l = this.left(root);
        var r = this.right(root);

        if (l < size && this.compFunc(a[l], a[root]) > 0)
        {
            largest = l; 
        } else
        {
            largest = root;
        }

        if (r < size && this.compFunc(a[r], a[largest]) > 0)
        {
            largest = r;
        }

        if (largest === root)
        {
            break;
        } else
        {
            exchange(root, largest);
            root = largest;
        }
    }
};

/**
Initialize heap from an unordered array
*/
Heap.prototype.fromArray = function (arr)
{
    this.length = arr.length;
    this.innerArray = arr;

    for (var i = (this.length >> 1) - 1; i >= 0; --i)
    {
        this.heapify(i);
    }
};

/**
Insert an element into the heap
*/
Heap.prototype.insert = function (x)
{
    const a = this.innerArray;
    function exchange(i1, i2)
    {
        var temp = a[i1];
        a[i1] = a[i2];
        a[i2] = temp;
    }

    var i = this.length++;
    var p = this.parent(i);
    a[i] = x;

    while (i > 0 && this.compFunc(a[p], a[i]) < 0)
    {
        exchange(p, i);    
        i = this.parent(i);
        p = this.parent(i);
    }
};

/**
Extract the element at the root of the heap
*/
Heap.prototype.extract = function ()
{
    assert(this.length > 0, "No element to extract from heap");

    var max = this.root();
    this.innerArray[0] = this.innerArray[--this.length];
    this.heapify(0);
    return max;
};

/**
Return the element at the root of the heap
*/
Heap.prototype.root = function ()
{
    return this.innerArray[0];
};

/**
Obtain the content of the heap as an array
*/
Heap.prototype.toArray = function ()
{
    return this.innerArray.slice(0);
};
