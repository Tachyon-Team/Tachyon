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
    @class
    Iterates over an array.

    @param {Array} a array to iterate over

    @augments Iterator
*/
function ArrayIterator(a)
{
    assertNew(this);

    /** @private */
    this.a = a;

    /** @private */
    this.index = 0;

    return this;
};

ArrayIterator.prototype = new Iterator();

/** Move iterator to the next item */
ArrayIterator.prototype.next = function ()
{
    this.index++;
};

/** Ensure iterator is still on a valid item.  Ex: Not at the end */
ArrayIterator.prototype.valid = function ()
{
    return this.index < this.a.length;
};

/** Returns the current item */
ArrayIterator.prototype.get = function ()
{
    return this.a[this.index];
};






