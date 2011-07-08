const neg0 = 1 / -Infinity;
const pos0 = 0;
const pos1 = 1;
const pos2 = 2;
const posHalf = 1 / 2;
const eBits = 11;
const mBits = 52;
const eBias = 1023;
const num_2exp52   = num_mul(67108864, 67108864);                     // bignum((2^26)^2)
const num_2exp64m1 = num_sub(num_mul(num_2exp52, 4096), 1);           // bignum(2^(52+12) - 1)
const pow_2_26     = 67108864;                                        // Math.pow(2, 26)

/* */
var isNeg0 = function (x)
{
    return (x===0 && (1/x)===-Infinity);
}

/* */
var floatCopysign = function (x, y)
{
    if (isNeg0(y))
        return -(Math.abs(x));
    else
        return ((y < 0) ? -(Math.abs(x)) : Math.abs(x));
}


/* */
var floatToInexactExpFormat = function (x)
{
    // Positive exponent
    var expFormPos = function (x, y, i)
    {
        var i2 = i + i;
        var z = !(eBias < i2) && !(x < y) ? expFormPos(x, y*y, i2)
                                          : [x, 0, 1];
        var a = z[0];
        var b = z[1];
        var i_b = i + b;

        if (!(eBias < i_b) && !(a < y))
        {
            z[0] = a / y;
            z[1] = i_b;
        }
        
        return z;
    }

    // Negative exponent
    var expFormNeg = function (x, y, i)
    {
        var i2 = i + i;
        var z = ((i2 < (eBias - 1) && (x < y)) ? (expFormNeg(x, y*y, i2))
                                               : [x, 0, 1]);
        var a = z[0];
        var b = z[1];
        var i_b = i + b;

        if ((i_b < (eBias - 1)) && (a < y))
        {
            z[0] = a / y;
            z[1] = i_b;
        }
        return z;
    }

    var expForm = function (x)
    {
        if (x < pos1)
        {
            var z = expFormNeg(x, posHalf, 1);
            z[0] = 2 * z[0];
            z[1] = -1 - z[1];
            return z;
        }
        else
        {
            var res = expFormPos(x, pos2, 1);
            return res;
        }
        
    }

    if (floatCopysign(pos1, x) < 0)
    {
        var z = expForm(floatCopysign(x, pos1));
        z[2] = -1;
        return z;
    }
    else
        return (expForm(x));
}

/* */
var floatToExactExpFormat = function (x)
{
    var z = floatToInexactExpFormat(x);
    var y = z[0];

    if (!(y < pos2))              // deal with +Infinity and +NaN
    {
        if (pos0 < y)
            z[0] = num_2exp52;                            // +Infinity
        else
            z[0] = num_sub(num_mul(num_2exp52, 2), 1);    // +NaN
        z[1] = Math.pow(2, (eBits - 1));
    }
    else
    {
        temp = z[0] * pow_2_26;
        temp_floor = Math.floor(temp);
        high = num_mul(pow_2_26, temp_floor);
        low = num_from_js(Math.floor(pow_2_26 * (temp - temp_floor)));
        z[0] = num_add(high, low);
    }

    z[1] = z[1] - mBits;
    return z;
}

/* */
var floatToBits = function (x)
{
    var bits = function (a, b)
    {
        var res;

        if (num_lt(a, num_2exp52))
            res = a;
        else
            res = num_add(num_sub(a, num_2exp52), num_mul(num_2exp52, (b + mBits + eBias)));
        return res;
    }
    
    var z = floatToExactExpFormat(x);
    var y = bits(z[0], z[1]);
    var res = ((z[2] < 0) ? num_add(num_mul(Math.pow(2, eBits), num_2exp52), y)
                          : y);
    
    return num_and(num_2exp64m1, res);
}

/* */
var binary = function(x)
{
    return (x && num_to_string(x, 16));    
}

print(binary(floatToBits(1.7976931348623157e308)));
/*
// Tests //
print(binary(floatToBits(0.0)));
print(binary(floatToBits(-0.0)));
print(binary(floatToBits(1.0)));
print(binary(floatToBits(-1.0)));
print(binary(floatToBits(0.1)));
print(binary(floatToBits(+Infinity)));
print(binary(floatToBits(1.7976931348623157e308)));
print(binary(floatToBits(-1.7976931348623157e308)));
print(binary(floatToBits(2.2250738585072020e-308)));
print(binary(floatToBits(-2.2250738585072020e-308)));
*/
/*
# expected output:
#
# "0"
# "1000000000000000000000000000000000000000000000000000000000000000"
# "11111111110000000000000000000000000000000000000000000000000000"
# "1011111111110000000000000000000000000000000000000000000000000000"
# "11111110111001100110011001100110011001100110011001100110011010"
# "111111111110000000000000000000000000000000000000000000000000000"
# "111111111111111111111111111111111111111111111111111111111111111"
# "10000000000000000000000000000000000000000000000000001"
# "1000000000010000000000000000000000000000000000000000000000000001"
#  0         1         2         3         4         5         6   6
#  01234567890         0         0         0         0         0   4
*/
