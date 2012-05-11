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
Miscellaneous utility functions.

@author
Maxime Chevalier-Boisvert
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
        num_instance(x) || Math.floor(x) === x,
        'value must be integer'
    );

    if (num_le(x, 0) === true)
        return false;

    // (x & (x-1)) === 0
    return num_eq(num_and(x, num_sub(x, 1)), 0);
}

/**
Find the index of the highest bit set to 1 in an integer
*/
function highestBitIdx(x)
{
    assert (
        num_instance(x) || Math.floor(x) === x,
        'value must be integer'
    );

    var ret = -1;

    while (num_ne(x, 0) === true)
    {
        x = num_shift(x, -1);
        ret++;
    }

    return ret;
}

/**
Get the value of the highest bit set to 1 in an integer
*/
function highestBit(x)
{
    return num_shift(1, highestBitIdx(x));
}

/**
Get the value of the lowest bit set to 1 in an integer
*/
function lowestBit(x)
{
    return num_and(x, num_neg(x));
}

/**
Compute the next power of 2 greater than x
*/
function nextPowerOf2(x)
{
    assert (
        num_instance(x) || Math.floor(x) === x,
        'value must be integer'
    );

    var bits = num_add(x, 0);

    for (var sft = 1;; sft *= 2)
    {
        bits = num_or(bits, num_shift(bits, -sft));

        if (num_eq(num_shift(x, -sft), 0))
            break;
    }

    return num_add(bits, 1);
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
        return num_shift(-1, numBits-1);
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
        return num_sub(num_shift(1, numBits), 1);
    }

    // If this is a signed integer type
    else
    {
        return num_sub(num_shift(1, numBits-1), 1);
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
function isInt(val)
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
                if (ch === '+')
                {
                    numSig = 1;
                    charIndex++;
                }
                else if (ch === '-')
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
                if (ch === '.')
                {
                    state = 'FRAC';
                    charIndex++;
                    continue;
                }

                else if (ch === 'e' || ch === 'E')
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
                if (ch === 'e' || ch === 'E')
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
            if (lastDig % 2 === 0)
                rndDir = 0;
            else
                rndDir = 1;
        }

        for (var i = allDigs.length - 1; i >= 0 && rndDir === 1; --i)
        {
            allDigs[i] = (allDigs[i] + 1) % 10;

            if (allDigs[i] !== 0)
                rndDir = 0;
        }

        if (rndDir === 1)
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

    var outStr = (numSig === -1)? '-':'';

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
    if (expNum !== 0)
    {
        outStr += 'e' + ((expNum > 0)? '+':'') + expNum;
    }

    //print(numVal);
    //print(outStr);

    return outStr;
}

/**
Find an available name in a set by prepending the smallest number
postfix possible. This function uses a binary search.
*/
function findFreeName(nameTaken, startName)
{
    if (nameTaken(startName) === false)
    {
        return startName;
    }
    else
    {
        function idToName(idx) { return startName + '_' + idx; }
        function idTaken(idx) { return nameTaken(idToName(idx)); }

        var minIdx = 1;
        var maxIdx = 1;

        while (idTaken(maxIdx) === true)
        {
            minIdx = maxIdx;
            maxIdx *= 2;
        }

        while (minIdx < maxIdx)
        {
            var midIdx = minIdx + ((maxIdx - minIdx) >> 1);

            if (idTaken(midIdx))
                minIdx = midIdx + 1;
            else
                maxIdx = midIdx;
        }

        return idToName(maxIdx);
    }
}

function PerfInfo()
{
    this.time = 0;
    this.kbs_alloc = 0;
    this.buckets = {};
}

var perfBuckets = {};

function measurePerformance(bucket, thunk)
{
    var perfInfo;

    if (!perfBuckets.hasOwnProperty(bucket))
    {
        perfInfo = new PerfInfo();
        perfBuckets[bucket] = perfInfo;
    }
    else
    {
        perfInfo = perfBuckets[bucket];
    }

    var perfBucketsOld = perfBuckets;

    perfBuckets = perfInfo.buckets;

    var start_time = currentTimeMillis();
    var start_kbs_alloc = memAllocatedKBs();

    var result = thunk();

    var kbs_alloc = memAllocatedKBs() - start_kbs_alloc;
    var time = currentTimeMillis() - start_time;

    perfBuckets = perfBucketsOld;

    perfInfo.time += time;
    perfInfo.kbs_alloc += kbs_alloc;

    return result;
}

function reportPerformance()
{
    print("******************** Performance report");

    for (var bucket in perfBuckets)
    {
        var perfInfo = perfBuckets[bucket];

        print("");
        print(
            bucket + ": " +
            (perfInfo.time/1000) + " s, " +
            (perfInfo.kbs_alloc/1024) + " MB allocated"
        );

        reportPerformanceSubBuckets(perfInfo, perfInfo.buckets, "");
    }

    print("");
    print("********************");
}

function reportPerformanceSubBuckets(overall, buckets, indent)
{
    indent += "    ";

    var sortedBuckets = [];

    for (var bucket in buckets)
        sortedBuckets.push(bucket);

    sortedBuckets = sortedBuckets.sort(
        function (x, y)
        {
            return buckets[x].time < buckets[y].time; 
        }
    );

    for (var i = 0; i<sortedBuckets.length; i++)
    {
        var bucket = sortedBuckets[i];
        var perfInfo = buckets[bucket];

        print(
            indent +
            "time=" + Math.floor(100*perfInfo.time/overall.time) + "% " +
            "alloc=" + Math.floor(100*perfInfo.kbs_alloc/overall.kbs_alloc) + "%" +
            " -- " + bucket
        );

        reportPerformanceSubBuckets(overall, perfInfo.buckets, indent);
    }
}
