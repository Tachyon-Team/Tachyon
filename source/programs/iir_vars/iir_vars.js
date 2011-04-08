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

function t1()
{
    var a = pint(1);
    var b = pint(2);

    return boxInt(a + b);
}

function t2(v)
{
    var a = pint(1);
    var b = pint(2);

    if (v)
    {
    }
    else
    {
    }

    return boxInt(a + b);
}

function t3(arg)
{
    if (true)
    {
        var a = pint(1);
        arg = unboxInt(arg);

        return boxInt(a + arg);
    }
    else
    {
        return 0;
    }
}

function t4(arg, v)
{
    if (v)
    {
        var a = pint(1);
        arg = unboxInt(arg);

        var c = boxInt(a + arg);
    }
    else
    {
        var d = 4;
    }

    return 0;
}

function t5()
{
    var sum = u16(0);

    for (var i = 0; i < 10; ++i)
    {
        var a = u16(1);
        sum += a;
    }

    return boxInt(iir.icast(IRType.pint, sum));
}

function t6()
{
    if (true)
    {
        for (var i = 0; i < 5; ++i)
        {
            var ch = u16(0);
        }
    }

    return 0;
}

function test()
{
    if (t1() !== 3)
        return 1;

    if (t2() !== 3)
        return 2;

    if (t3(4) !== 5)
        return 3;

    if (t4(4) !== 0)
        return 4;

    if (t5() !== 10)
        return 5;

    if (t6() !== 0)
        return 6;

    return 0;
}

