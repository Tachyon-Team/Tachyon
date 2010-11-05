/**
@fileOverview
Miscellaneous utility functions.

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010 Maxime Chevalier-Boisvert, All Rights Reserved
*/

/**
Test if an integer value is a power of 2
*/
function isPowerOf2(x)
{
    assert (
        Math.floor(x) == x,
        'value must be integer'
    );

    if (x <= 0)
        return false;

    return (x & (x-1)) == 0;
}

/**
Find the highest bit set to 1 in an integer
*/
function highestBit(x)
{
    assert (
        Math.floor(x) == x,
        'value must be integer'
    );

    var ret = -1;

    while (x != 0)
    {
        x >>= 1;
        ret++;
    }

    return ret;
}

/**
Calculate the minimum value an integer variable can store. Signed
values are assumed to use the 2s complement representation.
*/
function getIntMin(numBits, unsigned)
{
    // If this is an unsigned integer type
    if (unsigned)
    {
        return 0;
    }

    // If this is a signed integer type
    else
    {
        return -Math.pow(2, numBits - 1);
    }
}

/**
Calculate the maximum value an integer variable can store. Signed
values are assumed to use the 2s complement representation.
*/
function getIntMax(numBits, unsigned)
{
    // If this is an unsigned integer type
    if (unsigned)
    {
        return Math.pow(2, numBits) - 1;
    }

    // If this is a signed integer type
    else
    {
        return Math.pow(2, numBits - 1) - 1;
    }
}

