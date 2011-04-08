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

function test_putProp()
{
    var op1 = { toString: function () { return 'b'; } };

    var a = {};
    a.b = 1;

    if (a.b !== 1)
        return 1;

    var b = 1;
    var c = (b.a = 1);

    if (c !== 1)
        return 2;

    a[0] = 1;

    if (a[0] !== 1)
        return 3;

    if (a['0'] !== 1)
        return 4;

    a['1'] = 2;

    if (a['1'] !== 2)
        return 3;

    if (a[1] !== 2)
        return 5;

    a[op1] = 7;

    if (a.b !== 7)
        return 6;

    return 0;
}

function test_getProp()
{
    var op1 = { toString: function () { return 'b'; } };

    var a = { b:1 };

    if (a.b !== 1)
        return 1;

    if (a.c !== undefined)
        return 2;

    if (a[op1] !== 1)
        return 3;

    var b = 1;

    if (b.a !== undefined)
        return 4;

    return 0;
}

function test_delProp()
{
    var op1 = { toString: function () { return 'c'; } };

    var a = { b:1, c:2 };

    if (a.b !== 1)
        return 1;

    var c = (delete a.b);

    if (c !== true)
        return 2;

    if (a.b !== undefined)
        return 3;    

    if (a.c !== 2)
        return 4;

    delete a[op1];

    if (a.c !== undefined)
        return 5;

    var b = 1;
    
    var c = (delete b.a);

    if (c !== true)
        return 6;

    return 0;
}

function test_enum()
{
    var n = 0;

    for (k in 1)
        n++;

    if (n !== 0)
        return 1;

    return 0;
}

function test_in()
{
    var op1 = { toString: function () { return 'a'; } };
    var op2 = { toString: function () { return 'd'; } };

    var o = { a:1, b:1, c:3 };

    if (!('a' in o))
        return 1;

    if ('d' in o)
        return 2;

    if (!(op1 in o))
        return 3;

    if (op2 in o)
        return 4;

    o[0] = 1337;

    if (!(0 in o))
        return 5;

    if (!('0' in o))
        return 6;

    return 0;
}

function test_instanceOf()
{
    if (!({} instanceof Object))
        return 1;

    if (1 instanceof Object)
        return 2;

    return 0;
}

function test()
{
    var r = test_putProp();
    if (r !== 0)
        return 100 + r;

    var r = test_getProp();
    if (r !== 0)
        return 200 + r;

    var r = test_delProp();
    if (r !== 0)
        return 300 + r;

    var r = test_enum();
    if (r !== 0)
        return 400 + r;

    var r = test_in();
    if (r !== 0)
        return 500 + r;

    var r = test_instanceOf();
    if (r !== 0)
        return 600 + r;

    return 0;
}

