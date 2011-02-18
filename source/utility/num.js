/**
@fileOverview
Infinite precision integer arithmetic functions.

@author
Marc Feeley

@copyright
Copyright (c) 2011 Marc Feeley, All Rights Reserved
*/

/*
This library implements infinite precision integers using bignums.
For convenience it also provides an abstraction, the "num" type, which
is a combination of the JavaScript number representation and the
bignum representation.  The JavaScript number representation is used
when the integer fits in a JavaScript number.  Otherwise the bignum
representation is used.
*/

//-----------------------------------------------------------------------------

/*
Bignums are represented using an array of "big digits".  Each big
digit is a binary number of a certain width (bignum_radix_log2).  A
2's complement little-endian representation is used, in other words
the big digit at index 0 is the least significant and the sign of the
number is the most significant bit of the most significant big digit.
For efficiency, the representation is normalized so that the array
with the fewest big digits is used.

For example, if bignum_radix_log2 = 2, here is how the numbers from
-10 to 10 are represented:

-10 -> [2,1,3]
-9  -> [3,1,3]
-8  -> [0,2]
-7  -> [1,2]
-6  -> [2,2]
-5  -> [3,2]
-4  -> [0,3]
-3  -> [1,3]
-2  -> [2]
-1  -> [3]
0   -> [0]
1   -> [1]
2   -> [2,0]
3   -> [3,0]
4   -> [0,1]
5   -> [1,1]
6   -> [2,1]
7   -> [3,1]
8   -> [0,2,0]
9   -> [1,2,0]
10  -> [2,2,0]

*/

const bignum_radix_log2 = 14; // must be between 5 and 14 (could be as low as 1 except for division by 10 required by bignum_to_string)
const bignum_radix = 1 << bignum_radix_log2;
const bignum_radix_div2 = 1 << (bignum_radix_log2-1);

function bignum_from_js(n)
{
    // Constructs a normalized bignum from a plain JavaScript integer.

    var bignum = [];
    var i = 0;

    if (n < 0)
    {
        while (n < -bignum_radix_div2)
        {
            bignum[i++] = n & (bignum_radix-1);
            n = n >> bignum_radix_log2;
        } 
    }
    else
    {
        while (n >= bignum_radix_div2)
        {
            bignum[i++] = n & (bignum_radix-1);
            n = n >> bignum_radix_log2;
        } 
    }

    bignum[i++] = n & (bignum_radix-1);

    return bignum;
}

function bignum_to_js(bignum)
{
    // Converts a bignum to a plain JavaScript integer.  If the number
    // does not fit in a JavaScript integer, false is returned.

    var len = bignum.length;
    var i = len-1;
    var n = (bignum[i] < bignum_radix_div2) ? 0 : -1;

    while (i >= 0)
    {
        var d = bignum[i--];
        var x = (n << bignum_radix_log2) + d;
        if ((x >> bignum_radix_log2) !== n || // check for overflow
            (x & (bignum_radix-1)) !== d)
            return false;
        n = x;
    }

    return n;
}

/**
Test that a value is a bignum instance
*/
function bignum_instance(val)
{
    return (val instanceof Array);
}

function bignum_nonneg(bignum)
{
    // Tests if a normalized bignum is nonnegative.

    return bignum[bignum.length-1] < bignum_radix_div2;
}

function bignum_zero(bignum)
{
    // Tests if a normalized bignum is zero.

    return (bignum.length === 1) && (bignum[0] === 0);
}

function bignum_cmp(bignum_a, bignum_b)
{
    // Compares two normalized bignums.  Returns -1, 0, or 1 if
    // bignum_a is respectively less than, equal, or greater than
    // bignum_b.

    if (bignum_nonneg(bignum_a))
    {
        if (!bignum_nonneg(bignum_b))
            return 1;
    }
    else
    {
        if (bignum_nonneg(bignum_b))
            return -1;
    }

    // bignums have same sign

    var len_a = bignum_a.length;
    var len_b = bignum_b.length;
    var result;

    if (len_a === len_b)
    {
        // bignums have same number of big digits

        result = 0;
        while (--len_a >= 0)
        {
            var dig_a = bignum_a[len_a];
            var dig_b = bignum_b[len_a];
            if (dig_a < dig_b)
            {
                result = -1;
                break;
            }
            else if (dig_a > dig_b)
            {
                result = 1;
                break;
            }
        }
    }
    else
    {
        if (len_a < len_b)
            result = -1;
        else
            result = 1;

        if (!bignum_nonneg(bignum_a))
            result = -result;
    }

    return result;
}

function bignum_lt(bignum_a, bignum_b)
{
    // Compares two normalized bignums and returns true iff first
    // is less than second.

    return bignum_cmp(bignum_a, bignum_b) < 0;
}

function bignum_eq(bignum_a, bignum_b)
{
    // Compares two normalized bignums and returns true iff first
    // is less than second.

    // return bignum_cmp(bignum_a, bignum_b) === 0;

    var i = bignum_a.length;

    if (i !== bignum_b.length)
        return false;

    while (--i >= 0)
    {
        if (bignum_a[i] !== bignum_b[i])
            return false;
    }

    return true;
}

function bignum_gt(bignum_a, bignum_b)
{
    // Compares two normalized bignums and returns true iff first
    // is greater than second.

    return bignum_cmp(bignum_a, bignum_b) > 0;
}

function bignum_abs(bignum_a)
{
    if (bignum_nonneg(bignum_a))
        return bignum_a;
    else
        return bignum_neg(bignum_a);
}

function bignum_neg(bignum_a)
{
    // Negates a normalized bignum.

    var len_a = bignum_a.length;
    var bignum = [];
    var carry = 1;
    var i = 0;

    do
    {
        var a = (bignum_radix-1) - bignum_a[i] + carry;
        bignum[i++] = a & (bignum_radix-1);
        carry = a >> bignum_radix_log2;
    } while (i < len_a);

    var ext_a = (bignum_a[i-1] < bignum_radix_div2) ? 0 : bignum_radix-1;

    bignum[i++] = ((bignum_radix-1) - ext_a + carry) & (bignum_radix-1); 

    return bignum_normalize(bignum);
}

function bignum_add(bignum_a, bignum_b)
{
    // Adds two normalized bignums.

    var len_a = bignum_a.length;
    var len_b = bignum_b.length;

    if (len_a < len_b)
    {
        var bignum_tmp = bignum_a;
        var len_tmp = len_a;
        bignum_a = bignum_b;
        len_a = len_b;
        bignum_b = bignum_tmp;
        len_b = len_tmp;
    }

    var bignum = [];
    var carry = 0;
    var i = 0;

    do
    {
        var ab = bignum_a[i] + bignum_b[i] + carry;
        bignum[i++] = ab & (bignum_radix-1);
        carry = ab >> bignum_radix_log2;
    } while (i < len_b);

    var ext_b = (bignum_b[i-1] < bignum_radix_div2) ? 0 : bignum_radix-1;

    while (i < len_a)
    {
        var ab = bignum_a[i] + ext_b + carry;
        bignum[i++] = ab & (bignum_radix-1);
        carry = ab >> bignum_radix_log2;
    }

    var ext_a = (bignum_a[i-1] < bignum_radix_div2) ? 0 : bignum_radix-1;

    bignum[i++] = (ext_a + ext_b + carry) & (bignum_radix-1); 

    return bignum_normalize(bignum);
}

function bignum_sub(bignum_a, bignum_b)
{
    // Substracts two normalized bignums.

    return bignum_add(bignum_a, bignum_neg(bignum_b));
}

function bignum_mul(bignum_a, bignum_b)
{
    // Multiplies two normalized bignums.

    var neg = false;

    if (!bignum_nonneg(bignum_a))
    {
        neg = !neg;
        bignum_a = bignum_neg(bignum_a);
    }

    if (!bignum_nonneg(bignum_b))
    {
        neg = !neg;
        bignum_b = bignum_neg(bignum_b);
    }

    var len_a = bignum_a.length;
    var len_b = bignum_b.length;
    var bignum = [];

    for (var i=len_a+len_b; i>=0; i--)
        bignum[i] = 0;

    for (var i=0; i<len_a; i++)
    {
        var mult = bignum_a[i];
        var carry = 0;
        var k = i;
        for (var j=0; j<len_b; j++)
        {
            var ab = mult * bignum_b[j] + bignum[k] + carry;
            bignum[k++] = ab & (bignum_radix-1);
            carry = ab >> bignum_radix_log2;
        }
        bignum[k++] = carry;
    }

    bignum = bignum_normalize(bignum);

    if (neg)
        return bignum_neg(bignum);
    else
        return bignum;
}

function bignum_div(bignum_a, bignum_b)
{
    // Quotient of two normalized bignums.

    var qr = bignum_nonneg_quorem(bignum_abs(bignum_a), bignum_abs(bignum_b));

    if (bignum_nonneg(bignum_a) === bignum_nonneg(bignum_b))
        return qr.quo;
    else
        return bignum_neg(qr.quo);
}

function bignum_mod(bignum_a, bignum_b)
{
    // Modulo of two normalized bignums.

    var qr = bignum_nonneg_quorem(bignum_abs(bignum_a), bignum_abs(bignum_b));

    if (bignum_nonneg(bignum_a) === bignum_nonneg(bignum_b))
        return qr.rem;
    else
        return bignum_neg(qr.rem);
}

function bignum_nonneg_quorem(bignum_a, bignum_b)
{
    // Computes quotient and remainder of two nonnegative normalized
    // bignums.

    var len_a = bignum_a.length;
    var len_b = bignum_b.length;

    if (len_b > 1)
    {
        throw "we don't yet support multi-digit divisors";
    }
    else
    {
        // simple algo for single digit divisor

        var bignum = new Array(len_a);
        var d = bignum_b[0]; // divisor
        var n = 0;

        for (var i=len_a-1; i>=0; i--)
        {
            n = (n << bignum_radix_log2) + bignum_a[i];
            var q = Math.floor(n/d); // integer division
            bignum[i] = q;
            n = n - q*d;
        }

        return { quo: bignum_normalize(bignum),
                 rem: bignum_from_js(n)
               };
    }
}

function bignum_not(bignum_a)
{
    // Bitwise not of a normalized bignum.

    var len_a = bignum_a.length;
    var bignum = [];
    var i = 0;

    do
    {
        var a = (bignum_radix-1) ^ bignum_a[i];
        bignum[i++] = a;
    } while (i < len_a);

    var ext_a = (bignum_a[i-1] < bignum_radix_div2) ? 0 : bignum_radix-1;

    bignum[i++] = (bignum_radix-1) ^ ext_a;

    return bignum_normalize(bignum);
}

function bignum_and(bignum_a, bignum_b)
{
    // Bitwise and of two normalized bignums.

    var len_a = bignum_a.length;
    var len_b = bignum_b.length;

    if (len_a < len_b)
    {
        var bignum_tmp = bignum_a;
        var len_tmp = len_a;
        bignum_a = bignum_b;
        len_a = len_b;
        bignum_b = bignum_tmp;
        len_b = len_tmp;
    }

    var bignum = [];
    var i = 0;

    do
    {
        var ab = bignum_a[i] & bignum_b[i];
        bignum[i++] = ab;
    } while (i < len_b);

    var ext_b = (bignum_b[i-1] < bignum_radix_div2) ? 0 : bignum_radix-1;

    while (i < len_a)
    {
        var ab = bignum_a[i] & ext_b;
        bignum[i++] = ab;
    }

    var ext_a = (bignum_a[i-1] < bignum_radix_div2) ? 0 : bignum_radix-1;

    bignum[i++] = ext_a & ext_b;

    return bignum_normalize(bignum);
}

function bignum_or(bignum_a, bignum_b)
{
    // Bitwise or of two normalized bignums.

    var len_a = bignum_a.length;
    var len_b = bignum_b.length;

    if (len_a < len_b)
    {
        var bignum_tmp = bignum_a;
        var len_tmp = len_a;
        bignum_a = bignum_b;
        len_a = len_b;
        bignum_b = bignum_tmp;
        len_b = len_tmp;
    }

    var bignum = [];
    var i = 0;

    do
    {
        var ab = bignum_a[i] | bignum_b[i];
        bignum[i++] = ab;
    } while (i < len_b);

    var ext_b = (bignum_b[i-1] < bignum_radix_div2) ? 0 : bignum_radix-1;

    while (i < len_a)
    {
        var ab = bignum_a[i] | ext_b;
        bignum[i++] = ab;
    }

    var ext_a = (bignum_a[i-1] < bignum_radix_div2) ? 0 : bignum_radix-1;

    bignum[i++] = ext_a | ext_b;

    return bignum_normalize(bignum);
}

function bignum_xor(bignum_a, bignum_b)
{
    // Bitwise xor of two normalized bignums.

    var len_a = bignum_a.length;
    var len_b = bignum_b.length;

    if (len_a < len_b)
    {
        var bignum_tmp = bignum_a;
        var len_tmp = len_a;
        bignum_a = bignum_b;
        len_a = len_b;
        bignum_b = bignum_tmp;
        len_b = len_tmp;
    }

    var bignum = [];
    var i = 0;

    do
    {
        var ab = bignum_a[i] ^ bignum_b[i];
        bignum[i++] = ab;
    } while (i < len_b);

    var ext_b = (bignum_b[i-1] < bignum_radix_div2) ? 0 : bignum_radix-1;

    while (i < len_a)
    {
        var ab = bignum_a[i] ^ ext_b;
        bignum[i++] = ab;
    }

    var ext_a = (bignum_a[i-1] < bignum_radix_div2) ? 0 : bignum_radix-1;

    bignum[i++] = ext_a ^ ext_b;

    return bignum_normalize(bignum);
}

function bignum_shift(bignum_a, shift)
{
    // Shifting a normalized bignum.

    var bit_shift;

    if (shift < 0)
        bit_shift = (bignum_radix_log2-1) + (shift+1) % bignum_radix_log2;
    else
        bit_shift = shift % bignum_radix_log2;
        
    var dig_shift = (shift - bit_shift) / bignum_radix_log2;

    var len_a = bignum_a.length;
    var len = len_a + dig_shift + (bit_shift===0 ? 0 : 1);

    if (len <= 0)
        return bignum_from_js((bignum_a[len_a-1] < bignum_radix_div2) ? 0 : -1);
    else
    {
        var bignum = new Array(len);

        if (bit_shift === 0)
        {
            /* optimize when only shifting big digits */

            var i = 0;
            var j = -dig_shift;

            while (j < 0)
            {
                bignum[i++] = 0;
                j++;
            }

            while (j < len_a)
            {
                bignum[i++] = bignum_a[j++];
            }
        }
        else
        {
            var i = 0;
            var j = -dig_shift;
            var reg;

            if (j > 0)
                reg = bignum_a[j-1] << bit_shift;
            else
            {
                while (j < 0)
                {
                    bignum[i++] = 0;
                    j++;
                }
                reg = 0;
            }

            while (j < len_a)
            {
                reg = (reg >> bignum_radix_log2) + (bignum_a[j++] << bit_shift);
                bignum[i++] = reg & (bignum_radix-1);
            }

            var ext_a = (bignum_a[len_a-1] < bignum_radix_div2) ? 0 : bignum_radix-1;

            reg = (reg >> bignum_radix_log2) + (ext_a << bit_shift);
            bignum[i++] = reg & (bignum_radix-1);
        }

        return bignum_normalize(bignum);
    }
}

var bignum_digits = new String("0123456789abcdefghijklmnopqrstuvwxyz");

function bignum_to_string(bignum_a, radix)
{
    // Convert a normalized bignum to a string of digits.

    var str = "";

    if (radix === undefined)
        radix = 10;

    if (bignum_zero(bignum_a))
        return "0";

    var sign;

    if (bignum_nonneg(bignum_a))
        sign = "";
    else
    {
        sign = "-";
        bignum_a = bignum_neg(bignum_a);
    }

    var bignum_radix = bignum_from_js(radix);

    while (!bignum_zero(bignum_a))
    {
        var qr = bignum_nonneg_quorem(bignum_a, bignum_radix);
        var d = bignum_to_js(qr.rem);
        str = bignum_digits.slice(d, d+1) + str;
        bignum_a = qr.quo;
    }

    return sign + str;
}

function bignum_normalize(bignum)
{
    // Normalizes a bignum by removing redundant top big digits.

    var len = bignum.length;
    var last = bignum[len-1];

    if (last < bignum_radix_div2)
    {
        while (len >= 2 && last === 0 && (last=bignum[len-2]) < bignum_radix_div2)
            len--;
    }
    else
    {
        while (len >= 2 && last === bignum_radix-1 && (last=bignum[len-2]) >= bignum_radix_div2)
            len--;
    }

    bignum.length = len;

    return bignum;
}

//-----------------------------------------------------------------------------

// The following functions are generic.  Their parameters can be
// JavaScript integers or bignums.  The parameters will be converted
// automatically, and if the result can fit in a JavaScript integer
// then it will be returned, otherwise a bignum is returned.

function num_from_js(n) // n is a JS integer or bignum
{
    if (bignum_instance(n) === true) // bignum?
        return n;
    else
        return bignum_from_js(n);
}

function num_to_js(bignum)
{
    var result = bignum_to_js(bignum);

    if (result === false)
        result = bignum;

    return result;
}

/**
Test that a value is a num instance
*/
function num_instance(val)
{
    return (bignum_instance(val) === true || typeof val === 'number');
}

/**
Test that a num value is integer
*/
function num_integer(val)
{
    return (bignum_instance(val) === true || Math.floor(val) === val);
}

function num_nonneg(n) // n is a JS integer or bignum
{
    if (bignum_instance(n) === true) // bignum?
        return bignum_nonneg(n);
    else
        return n >= 0;
}

function num_zero(n) // n is a JS integer or bignum
{
    if (bignum_instance(n) === true) // bignum?
        return bignum_zero(n);
    else
        return n === 0;
}

function num_lt(a, b) // a and b are JS integers or bignums
{
    if (bignum_instance(a) === true || bignum_instance(b) === true) // bignum case?
        return bignum_lt(num_from_js(a), num_from_js(b));
    else
        return a < b;
}

function num_gt(a, b) // a and b are JS integers or bignums
{
    if (bignum_instance(a) === true || bignum_instance(b) === true) // bignum case?
        return bignum_gt(num_from_js(a), num_from_js(b));
    else
        return a > b;
}

function num_eq(a, b) // a and b are JS integers or bignums
{
    if (bignum_instance(a) === true || bignum_instance(b) === true) // bignum case?
        return bignum_eq(num_from_js(a), num_from_js(b));
    else
        return a === b;
}

function num_ne(a, b)
{
    return num_eq(a, b) === false;
}

function num_le(a, b)
{
    return (num_lt(a,b) || num_eq(a,b));
}

function num_ge(a, b)
{
    return (num_gt(a,b) || num_eq(a,b));
}

function num_abs(a) // a is a JS integer or bignum
{
    // beware of case of the most negative JavaScript integer whose
    // absolute value is not representable as a JavaScript integer.

    return num_to_js(bignum_abs(num_from_js(a)));
}

function num_neg(a) // a is a JS integer or bignum
{
    // beware of case of the most negative JavaScript integer whose
    // absolute value is not representable as a JavaScript integer.

    return num_to_js(bignum_neg(num_from_js(a)));
}

function num_add(a, b) // a and b are JS integers or bignums
{
    return num_to_js(bignum_add(num_from_js(a), num_from_js(b)));
}

function num_sub(a, b) // a and b are JS integers or bignums
{
    return num_to_js(bignum_sub(num_from_js(a), num_from_js(b)));
}

function num_mul(a, b) // a and b are JS integers or bignums
{
    return num_to_js(bignum_mul(num_from_js(a), num_from_js(b)));
}

function num_div(a, b) // a and b are JS integers or bignums
{
    return num_to_js(bignum_div(num_from_js(a), num_from_js(b)));
}

function num_mod(a, b) // a and b are JS integers or bignums
{
    return num_to_js(bignum_mod(num_from_js(a), num_from_js(b)));
}

function num_not(a) // a is a JS integer or bignum
{
    return num_to_js(bignum_not(num_from_js(a)));
}

function num_and(a, b) // a and b are JS integers or bignums
{
    return num_to_js(bignum_and(num_from_js(a), num_from_js(b)));
}

function num_or(a, b) // a and b are JS integers or bignums
{
    return num_to_js(bignum_or(num_from_js(a), num_from_js(b)));
}

function num_xor(a, b) // a and b are JS integers or bignums
{
    return num_to_js(bignum_xor(num_from_js(a), num_from_js(b)));
}

function num_shift(a, shift) // a is a JS integer or bignum, shift is a JS integer
{
    return num_to_js(bignum_shift(num_from_js(a), shift));
}

/**
Unsigned right shift.
*/
function num_urshift(n, shift, width)
{
    assert (
        shift >= 0,
        'shift amount must be positive'
    );

    assert (
        typeof width === 'number',
        'width must be specified'
    );

    return num_shift(num_and(n, num_not(num_shift(-1,width))), -shift);
}

function num_to_string(a, radix) // a is a JS integer or bignum, radix is a JS integer
{
    if (bignum_instance(a) === true) // bignum?
        return bignum_to_string(a, radix);
    else
        return a.toString();
}

//-----------------------------------------------------------------------------
