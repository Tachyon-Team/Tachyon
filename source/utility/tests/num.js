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

Unit tests for infinite precision integers.

@copyright
Copyright (c) 2011 Tachyon Javascript Engine, All Rights Reserved
*/

tests.utility = tests.utility || tests.testSuite();

tests.utility.num = tests.testSuite();

tests.utility.num.arithmetic = function ()
{
    // These tests cover more cases when bignum_radix_log2 is set to 5.

    const n = 50;

    for (var a=-n; a<=n; a++)
    {
        if (bignum_to_js(bignum_from_js(a)) !== a)
        {
            throw ("bignum_to_js(bignum_from_js("+a+")) !== "+a);
        }

        if (num_abs(a) !== Math.abs(a))
        {
            throw ("num_abs("+a+") !== "+Math.abs(a));
        }

        if (num_neg(a) !== -a)
        {
            throw ("num_neg("+a+") !== "+(-a));
        }

        if (num_not(a) !== ~a)
        {
            throw ("num_not("+a+") !== "+(~a));
        }
    }

    for (var a=-n; a<=n; a++)
    {
        for (var b=-n; b<=n; b++)
        {
            if (num_lt(a, b) !== (a<b))
            {
                throw ("num_lt("+a+", "+b+") !== "+(a<b));
            }

            if (num_eq(a, b) !== (a===b))
            {
                throw ("num_eq("+a+", "+b+") !== "+(a===b));
            }

            if (num_gt(a, b) !== (a>b))
            {
                throw ("num_gt("+a+", "+b+") !== "+(a>b));
            }

            if (num_add(a, b) !== (a+b))
            {
                throw ("num_add("+a+", "+b+") !== "+(a+b));
            }

            if (num_sub(a, b) !== (a-b))
            {
                throw ("num_sub("+a+", "+b+") !== "+(a-b));
            }

            if (num_mul(a, b) !== (a*b))
            {
                throw ("num_mul("+a+", "+b+") !== "+(a*b));
            }

            if (num_and(a, b) !== (a&b))
            {
                throw ("num_and("+a+", "+b+") !== "+(a&b));
            }

            if (num_or(a, b) !== (a|b))
            {
                throw ("num_or("+a+", "+b+") !== "+(a|b));
            }

            if (num_xor(a, b) !== (a^b))
            {
                throw ("num_xor("+a+", "+b+") !== "+(a^b));
            }
        }
    }

    for (var a=0; a<=n; a++)
    {
        for (var b=1; b<=10; b++)
        {
            if (num_div(a, b) !== Math.floor(a/b))
            {
                throw ("num_div("+a+", "+b+") !== "+Math.floor(a/b));
            }

            if (num_mod(a, b) !== (a%b))
            {
                throw ("num_mod("+a+", "+b+") !== "+(a%b));
            }
        }
    }

    for (var a=-711; a<=711; a++)
    {
        for (var shift=-19; shift<=19; shift++)
        {
            var res;

            if (shift < 0)
                res = a >> -shift;
            else
                res = a << shift;

            if (num_shift(a, shift) !== res)
            {
                throw ("num_shift("+a+", "+shift+") !== "+res);
            }
        }
    }

    if (num_to_string(num_shift(1,1000)) !==
        "10715086071862673209484250490600018105614048117055336074437503883703510511249361224931983788156958581275946729175531468251871452856923140435984577574698574803934567774824230985421074605062371141877954182153046474983581941267398767559165543946077062914571196477686542167660429831652624386837205668069376")
    {
        throw ("num_to_string(num_shift(1,1000)) is incorrect");
    }
};
