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

function test_ctor()
{
    var o = {};

    if (typeof Object !== 'function')
        return 1;

    if (typeof o !== 'object')
        return 2;

    if (!(o instanceof Object))
        return 3;

    return 0;
}

function test_getPrototypeOf()
{
    //Object.getPrototypeOf = function (obj)

    var o = {}

    if (Object.getPrototypeOf(o) !== Object.prototype)
        return 1;

    return 0;
}

function test_create()
{
    //Object.create = function (obj, props)

    var a = {};

    var o = Object.create(a);

    if (Object.getPrototypeOf(o) !== a)
        return 1;

    return 0;
}

function test_defineProperty()
{
    //Object.defineProperty = function (obj, prop, attribs)

    var o = {};

    Object.defineProperty(o, 'p', { value: 7 });

    if (o.p !== 7)
        return 1;

    return 0;
}

function test_toString()
{
    //Object.prototype.toString = function ()

    var o = {};

    if (o.toString() !== 'object')
        return 1;

    return 0;
}

function test_valueOf()
{
    //Object.prototype.valueOf = function ()

    var o = {};

    if (o.valueOf() !== o)
        return 1;

    return 0;
}

function test_hasOwnProperty()
{
    //Object.prototype.hasOwnProperty = function (prop)

    var a = { va: 9 };

    var b = Object.create(a);

    b.vb = 10;

    if (b.hasOwnProperty('vb') !== true)
        return 1;

    if (b.hasOwnProperty('va') !== false)
        return 2;

    if (a.hasOwnProperty('va') !== true)
        return 3;

    return 0;
}

function test_isPrototypeOf()
{
    //Object.prototype.isPrototypeOf = function (obj)

    var a = {};

    var o = Object.create(a);

    if (a.isPrototypeOf(o) !== true)
        return 1;

    if (Object.prototype.isPrototypeOf(a) !== true)
        return 2;

    return 0;
}

function test()
{
    var r = test_ctor();
    if (r != 0)
        return 100 + r;

    var r = test_getPrototypeOf();
    if (r != 0)
        return 200 + r;

    var r = test_create();
    if (r != 0)
        return 300 + r;

    var r = test_defineProperty();
    if (r != 0)
        return 400 + r;

    var r = test_toString();
    if (r != 0)
        return 500 + r;

    var r = test_valueOf();
    if (r != 0)
        return 600 + r;

    var r = test_hasOwnProperty();
    if (r != 0)
        return 700 + r;

    var r = test_isPrototypeOf();
    if (r != 0)
        return 800 + r;

    return 0;
}

