/**
@fileOverview
Miscellaneous utility functions.

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010 Maxime Chevalier-Boisvert, All Rights Reserved
*/

/**
Get a reference to the global object
*/
function getGlobalObj()
{
    return (function() { return this; })();
}

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

/**
Test if a value is a positive integer
*/
function isPosInt(val)
{
    return (
        typeof val === 'number' &&
        val >= 1 &&
        Math.floor(val) === val
    );
}

/**
Test if a value is a non-negative integer
*/
function isNonNegInt(val)
{
    return (
        typeof val === 'number' &&
        val >= 0 &&
        Math.floor(val) === val
    );
}

/**
Test if a value is an integer
*/
function isInt()
{
    return (
        typeof val === 'number' &&
        Math.floor(val) === val
    );
}

/**
Format a given number string to limit the number of decimals
*/
function fmtNumDecimals(numVal, numDecs)
{
    assert (
        typeof numVal === 'number' && isNonNegInt(numDecs),
        'invalid input arguments'
    );

    var numStr = numVal.toString();
    var charIndex = 0;

    var state = 'SIGN';

    var numSig = null;
    var intDigs = [];
    var fracDigs = [];
    var expNum = 0;

    for (;;)
    {
        var ch = numStr.charAt(charIndex);

        if (ch === '')
            break;

        switch (state)
        {
            case 'SIGN':
            {
                if (ch == '+')
                {
                    numSig = 1;
                    charIndex++;
                }
                else if (ch == '-')
                {
                    numSig = -1;
                    charIndex++;
                }
                else if (ch >= '0' && ch <= '9')
                {
                    numSig = 1;
                }

                state = 'INTG';
                continue;
            }
            break;
    
            case 'INTG':
            {
                if (ch == '.')
                {
                    state = 'FRAC';
                    charIndex++;
                    continue;
                }

                else if (ch == 'e' || ch == 'E')
                {
                    state = 'EXPN';
                    charIndex++;
                    continue;
                }

                assert (
                    ch >= '0' && ch <= '9',
                    'invalid integer digit'
                );

                intDigs.push(numStr.charCodeAt(charIndex) - '0'.charCodeAt(0));
                charIndex++;
            }
            break;

            case 'FRAC':
            {
                if (ch == 'e' || ch == 'E')
                {
                    state = 'EXPN';
                    charIndex++;
                    continue;
                }

                assert (
                    ch >= '0' && ch <= '9',
                    'invalid fraction digit'
                );

                fracDigs.push(numStr.charCodeAt(charIndex) - '0'.charCodeAt(0));
                charIndex++;
            }
            break;

            case 'EXPN':
            {
                expNum = Number(numStr.slice(charIndex));

                assert (
                    isNaN(expNum) || numStr.length - charIndex < 4,
                    'invalid exponent'
                );

                charIndex = numStr.length;
            }
            break;

            default:
            assert (
                false,
                'invalid state'
            );
        }
    }

    // If rounding is required
    if (fracDigs.length > numDecs)
    {
        var allDigs = intDigs.concat(fracDigs);

        // Get the digit based on which we will round
        var rndDig = allDigs[intDigs.length + numDecs];

        // Remove the digits starting with the rounding digit
        allDigs = allDigs.slice(0, intDigs.length + numDecs);

        // Get the last digit that will be kept
        var lastDig = allDigs[allDigs.length-1];

        var rndDir;

        if (rndDig > 5)
        {
            rndDir = 1;
        }
        else if (rndDig < 5)
        {
            rndDir = 0;
        }
        else
        {
            // Round half to even: If the fraction of y is 0.5, then q is the 
            // even integer nearest to y.
            if (lastDig % 2 == 0)
                rndDir = 0;
            else
                rndDir = 1;
        }

        for (var i = allDigs.length - 1; i >= 0 && rndDir == 1; --i)
        {
            allDigs[i] = (allDigs[i] + 1) % 10;

            if (allDigs[i] != 0)
                rndDir = 0;
        }

        if (rndDir == 1)
        {
            allDigs.unshift(1);

            // If this number is in scientific notation, remove
            // the last fraction digit so as to keep only one 
            // integer digit
            if (expNum)
            {
                allDigs.pop();
                ++expNum;
            }
        }

        fracDigs = allDigs.slice(allDigs.length - numDecs, allDigs.length);
        intDigs = allDigs.slice(0, allDigs.length - numDecs);
    }

    var outStr = (numSig == -1)? '-':'';

    for (var i = 0; i < intDigs.length; ++i)
        outStr += String(intDigs[i]);

    // If there is a fractional part
    if (fracDigs.length)
    {
        outStr += '.';

        for (var i = 0; i < fracDigs.length; ++i)
            outStr += String(fracDigs[i]);
    }    

    // If there is a nonzero exponent
    if (expNum != 0)
    {
        outStr += 'e' + ((expNum > 0)? '+':'') + expNum;
    }

    //print(numVal);
    //print(outStr);

    return outStr;
}

