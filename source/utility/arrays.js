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
Useful array manipulation functions.

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010 Maxime Chevalier-Boisvert, All Rights Reserved
*/

/**
Test if an array set contains a value
*/
function arraySetHas(array, val)
{
    for (var i = 0; i < array.length; ++i)
        if (array[i] === val)
            return true;

    return false;
}

/**
Add a value to an array set
*/
function arraySetAdd(array, val)
{
    if (arraySetHas(array, val))
        return;

    array.push(val);
}

/**
Remove a value from an array set
*/
function arraySetRem(array, val)
{
    assert (
        array instanceof Array,
        'expected array in arraySetRem'
    );

    for (var i = 0; i < array.length; ++i)
    {
        if (array[i] === val)
        {
            array[i] = array[array.length - 1];
            array.pop();

            return val;
        }
    }

    return null;
}

/**
Remove all values from an array set in the second array
*/
function arraySetRemAll(arr1, arr2)
{
    for (var i = arr2.length - 1; i >= 0; --i)
    {
        arraySetRem(arr1, arr2[i]);
    }

    return null;
};

/**
Perform the union operation on an array set
*/
function arraySetUnion(arr1, arr2)
{
    out = arr1.slice(0);

    for (var i = 0; i < arr2.length; ++i)
        arraySetAdd(out, arr2[i]);

    return out;
}

/**
Perform the intersection operation on an array set
*/
function arraySetIntr(arr1, arr2)
{
    var out = [];

    for (var i = 0; i < arr1.length; ++i)
        if (arraySetHas(arr2, arr1[i]))
            out.push(arr1[i]);

    return out;
}

/**
Test two array sets for equality
*/
function arraySetEqual(arr1, arr2)
{
    if (arr1.length !== arr2.length)
        return false;

    for (var i = 0; i < arr1.length; ++i)
        if (!arraySetHas(arr2, arr1[i]))
            return false;

    return true;
}

/**
Generate an array with integer values between
from and to
*/
function arrayRange(from, to, step)
{
    var a = [];
    var i, j;

    if (to === undefined && step === undefined)
    {
        to = from;
        from = 0;
    }

    if (step === undefined)
    {
        step = 1;
    }

    assert(from >= 0, 'from must be >= 0');
    assert(step > 0, 'step must be > 0');

    j=0;
    for (i=from; i < to; i+=step)
    {
        a[j] = i;
        j += 1;
    }

    return a;
}

function arrayDirectConcat(array1, array2)
{
    for (var i = 0; i < array2.length; ++i)
        array1.push(array2[i]);
}

/**
    @class
    Iterates over an array.

    @param {Array} array array to iterate over

    @augments Iterator
*/
function ArrayIterator(array)
{
    assertNew(this);

    /**
    @private
    */
    this.array = array;

    /**
    @private
    */
    this.index = 0;

    return this;
};

ArrayIterator.prototype = new Iterator();

/** 
Move iterator to the next item
*/
ArrayIterator.prototype.next = function ()
{
    this.index++;
};

/**
Ensure iterator is still on a valid item.  Ex: Not at the end
*/
ArrayIterator.prototype.valid = function ()
{
    return this.index < this.array.length;
};

/**
Returns the current item
*/
ArrayIterator.prototype.get = function ()
{
    return this.array[this.index];
};

/**
Get the index of the current item. This is needed to implement
some operations on array iterators efficiently.
*/
ArrayIterator.prototype.getIndex = function ()
{
    return this.index;
};
