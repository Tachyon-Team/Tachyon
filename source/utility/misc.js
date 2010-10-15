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

