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

function foo1()
{
    return 0;
}

function foo2(x)
{
    return x;
}

function foo3(x,y)
{
    return x - y;
}

function foo4(a1, a2, a3, a4, a5)
{
    return a1 + a2 + a3 + a4 + a5;
}

function foo5()
{
    return this;
}

function foo6()
{
    var sum = 0;

    for (var i = 0; i < arguments.length; ++i)
        sum += arguments[i];

    return sum;
}

function llapply(func, thisArg, argArray)
{
    // Get the function pointer for the function
    var funcPtr = get_clos_funcptr(func);

    // Get the arguments table from the array
    var argTable = unboxRef(get_arr_arr(argArray));

    // Get the number of arguments
    var numArgs = iir.icast(IRType.pint, get_arr_len(argArray));

    // Perform the call using the apply instruction
    var retVal = iir.call_apply(funcPtr, func, thisArg, argTable, numArgs);

    return retVal;
}

function test()
{
    if (llapply(foo1, null, []) !== 0)
        return 100;
    if (llapply(foo1, null, [7]) !== 0)
        return 101;
    if (llapply(foo1, null, [7,7]) !== 0)
        return 102;
    if (llapply(foo1, null, [7,7,7]) !== 0)
        return 103;
    if (llapply(foo1, null, [7,7,7,7]) !== 0)
        return 104;
    if (llapply(foo1, null, [7,7,7,7,7]) !== 0)
        return 105;

    if (llapply(foo2, null, []) !== UNDEFINED)
        return 200;
    if (llapply(foo2, null, [3]) !== 3)
        return 201;
    if (llapply(foo2, null, [3,7]) !== 3)
        return 202;
    if (llapply(foo2, null, [3,7,7]) !== 3)
        return 203;
    if (llapply(foo2, null, [3,7,7,7]) !== 3)
        return 204;
    if (llapply(foo2, null, [3,7,7,7,7]) !== 3)
        return 205;

    if (llapply(foo3, null, [9,4]) !== 5)
        return 300;

    if (llapply(foo4, null, [1,1,1,1,1,7]) !== 5)
        return 400;

    var obj = {};
    if (llapply(foo5, obj, [1,1,1,1,1,7]) !== obj)
        return 500;

    // Create a vector with many arguments
    var numArgs = 777;
    var argArray = [7,3];
    argArray.length = numArgs;
    for (var i = 2; i < numArgs; ++i)
        argArray[i] = 1;
    var argSum = 0;
    for (var i = 0; i < argArray.length; ++i)
        argSum += argArray[i];

    if (llapply(foo3, null, argArray) !== 4)
        return 600;

    if (llapply(foo6, null, argArray) !== argSum)
        return 700;

    return 0;
}

