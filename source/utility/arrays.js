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

            return;
        }
    }
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
Returns an iterator for this array. An optional filter may be used
taking an element of the array and returning true if the value satisfies
the condition.
*/
function arrayIterator(a, filter)
{
    if (filter === undefined)
    {
        filter = function (item) { return true; };
    }

    var it = Object.create(arrayIterator.prototype);
    it.index = -1;
    it.arr = a;
    it.filter = filter;
    it.next();
    return it;
}

/** Tells whether all items have been visited */
arrayIterator.prototype.end = function ()
{
    return this.index >= this.arr.length;
};

/** Move iterator to the next item */
arrayIterator.prototype.next = function ()
{
    this.index++;
    while (!this.end() &&
           !this.filter(this.arr[this.index]))
    {
        this.index++;
    }
};

/** Returns the current item being visited */
arrayIterator.prototype.get = function ()
{
    return this.arr[this.index];
};

/** Returns a new array with the result of func applied to every item */
arrayMap = function (arr, func)
{
    var i = 0;
    var arr2 = [];
    arr2.length = arr.length;
    
    for (i=0; i<arr.length; ++i)
    {
        arr2[i] = func(arr[i]);
    }

    return arr2;
}
