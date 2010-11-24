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
Format a given number string to limit the significant digits
*/
/*
function fmtNumSigDigits(numVal, sigDigits)
{
    assert (
        typeof numVal === 'number' && isPosInt(sigDigits),
        'invalid input arguments'
    );

    var numStr = numVal.toString();
    var charIndex = 0;

    var state = 'PREF';

    var prefStr = '';
    var postStr = '';
    var intgDigs = [];
    var fracDigs = [];

    for (;;)
    {
        var ch = numStr.charAt(charIndex);

        if (ch === '')
            break;

        switch (state)
        {
            case 'PREF':
            {
                if (ch >= 0 || ch <= 9)
                {
                    state = 'INTG';
                    continue;
                }

                prefStr += ch;
                charIndex++;
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

                if (ch == 'e' || ch == 'E')
                {
                    state = 'POST';
                    continue;
                }

                intgDigs.push(numStr.charCodeAt(charIndex) - '0'.charCodeAt(0));
                charIndex++;
            }
            break;

            case 'FRAC':
            {
                if (ch == 'e' || ch == 'E')
                {
                    state = 'POST';
                    continue;
                }

                fracDigs.push(numStr.charCodeAt(charIndex) - '0'.charCodeAt(0));
                charIndex++;
            }
            break;

            case 'POST':
            {
                postStr += ch;
                charIndex++;
            }
            break;
        }
    }

    print('"' + prefStr + '"');
    print('"' + intgDigs + '"');
    print('"' + fracDigs + '"');
    print('"' + postStr + '"');

    // If some digits must be removed
    if (intgDigs.length + fracDigs.length > sigDigits)
    {
        



    }    



    // TODO: rounding







}
*/

