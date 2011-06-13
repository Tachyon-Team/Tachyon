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
Implementation of ECMAScript 5 math library routines.

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010-2011 Tachyon Javascript Engine, All Rights Reserved
*/

/**
Math object (see ECMAScript 5 18.8)
*/
var Math = {};

// TODO: set [[Class]] internal property of the Math object to "Math"

/**
15.8.2.1 abs (x)
Returns the absolute value of x; the result has the same magnitude as x
but has positive sign.

• If x is NaN, the result is NaN.
• If x is −0, the result is +0.
• If x is −∞, the result is +∞.
*/
Math.abs = function (x)
{
    if (x < 0)
        return -x;
    else
        return x;
};

/**
15.8.2.6 ceil (x)
Returns the smallest (closest to −∞) Number value that is not less than x
and is equal to a mathematical integer. If x is already an integer, the
result is x.

• If x is NaN, the result is NaN.
• If x is +0, the result is +0.
• If x is −0, the result is −0.
• If x is +∞, the result is +∞.
• If x is −∞, the result is −∞.
• If x is less than 0 but greater than -1, the result is −0.

The value of Math.ceil(x) is the same as the value of -Math.floor(-x).
*/
Math.ceil = function (x)
{
    // For integers, the value is unchanged
    return x;
};

/**
15.8.2.9 floor (x)
Returns the greatest (closest to +∞) Number value that is not greater than x 
and is equal to a mathematical integer. If x is already an integer, the result
is x.

• If x is NaN, the result is NaN.
• If x is +0, the result is +0.
• If x is −0, the result is −0.
• If x is +∞, the result is +∞.
• If x is −∞, the result is −∞.

• If x is greater than 0 but less than 1, the result is +0.

NOTE The value of Math.floor(x) is the same as the value of -Math.ceil(-x).
*/
Math.floor = function (x)
{
    // For integers, the value is unchanged
    return x;
};

/**
15.8.2.11 max ([value1 [, value2 [, … ]]])
Given zero or more arguments, calls ToNumber on each of the arguments and 
returns the largest of the resulting values.

• If no arguments are given, the result is −∞.
• If any value is NaN, the result is NaN.
• The comparison of values to determine the largest value is done as in
  11.8.5 except that +0 is considered to be larger than −0.

The length property of the max method is 2.
*/
Math.max = function ()
{
    var m = MIN_FIXNUM;

    for (var i = 0; i < arguments.length; ++i)
        if (arguments[i] > m)
            m = arguments[i];

    return m;
};

/**
15.8.2.12 min ([ value1 [, value2 [, … ]]])
Given zero or more arguments, calls ToNumber on each of the arguments and
returns the smallest of the resulting values.

• If no arguments are given, the result is +∞.
• If any value is NaN, the result is NaN.
• The comparison of values to determine the smallest value is done as in
  11.8.5 except that +0 is considered to be larger than −0.

The length property of the min method is 2.
*/
Math.min = function ()
{
    var m = MAX_FIXNUM;

    for (var i = 0; i < arguments.length; ++i)
        if (arguments[i] < m)
            m = arguments[i];

    return m;
};

/**
15.8.2.13 pow (x, y)
Returns an implementation-dependent approximation to the result of raising 
x to the power y.

• If y is NaN, the result is NaN.
• If y is +0, the result is 1, even if x is NaN.
• If y is −0, the result is 1, even if x is NaN.
• If x is NaN and y is nonzero, the result is NaN.
• If abs(x)>1 and y is +∞, the result is +∞.
• If abs(x)>1 and y is −∞, the result is +0.
• If abs(x)==1 and y is +∞, the result is NaN.
• If abs(x)==1 and y is −∞, the result is NaN.
• If abs(x)<1 and y is +∞, the result is +0.
• If abs(x)<1 and y is −∞, the result is +∞.
• If x is +∞ and y>0, the result is +∞.
• If x is +∞ and y<0, the result is +0.
• If x is −∞ and y>0 and y is an odd integer, the result is −∞.
• If x is −∞ and y>0 and y is not an odd integer, the result is +∞.
• If x is −∞ and y<0 and y is an odd integer, the result is −0. 162 © Ecma International 2009
• If x is −∞ and y<0 and y is not an odd integer, the result is +0.
• If x is +0 and y>0, the result is +0.
• If x is +0 and y<0, the result is +∞.
• If x is −0 and y>0 and y is an odd integer, the result is −0.
• If x is −0 and y>0 and y is not an odd integer, the result is +0.
• If x is −0 and y<0 and y is an odd integer, the result is −∞.
• If x is −0 and y<0 and y is not an odd integer, the result is +∞.
• If x<0 and x is finite and y is finite and y is not an integer, the result is NaN.
*/
Math.pow = function (base, power)
{
    var acc = 1;
    var current = base;

    for (; power != 0; power >>= 1, current *= current) {
        if (power & 1) acc *= current;
    }

    return acc;
};
