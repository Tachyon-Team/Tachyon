function test()
{
    /*
    var min = getIntMin(30, false);
    var max = getIntMax(30, false);

    print('min: ' + num_to_string(min));
    print('max: ' + num_to_string(max));

    // num_sub(num_shift(1, numBits-1), 1);

    var a = num_shift(1, 29)
    print('(1<<29): ' + num_to_string(a));
    var b = num_sub(a, 1);
    print('(1<<29)-1: ' + num_to_string(b));
    */

    /*
    print('radix    : ' + bignum_radix);
    print('radix - 1: ' + (bignum_radix-1))
    print('n&(r-1)  : ' + (1 & (bignum_radix-1)));
    */

    /*
    var n = bignum_from_js(1);
    print('bignum 1: ' + num_to_string(n));
    */

    var a = num_shift(1, 29)
    print('(1<<29): ' + num_to_string(a));
    var b = num_sub(a, 1);
    print('(1<<29)-1: ' + num_to_string(b));






    return 0;
}

