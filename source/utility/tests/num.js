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
        for (var shift=-20; shift<=20; shift++)
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
