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
    out = [];

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
    if (arr1.length != arr2.length)
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

    
    assert(from >= 0);
    assert(to > from);
    assert(step > 0);

    j=0;
    for (i=from; i < to; i+=step)
    {
        a[j] = i;
        j += 1;
    }

    return a;
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

