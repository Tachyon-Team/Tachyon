function test_and(x, y)
{
    return x & y;
}

function test_and3(x)
{
    return x & 3;
}

function test_3and(y)
{
    return 3 & y;
}

function test_or(x, y)
{
    return x | y;
}

function test_or75(x)
{
    return x | 75;
}

function test_75or(x)
{
    return 75 | x;
}

function test_xor(x, y)
{
    return x ^ y;
}

function test_xor101(x)
{
    return x ^ 101;
}

function test_101xor(x)
{
    return 101 ^ x;
}

function test()
{
    // TODO: bitwise NOT tests cannot work with only fixnum 
    // support. Implement once FP support is in.

    if (test_and(7, 2) !== 2)
        return 1;
    if (test_and3(9) !== 1)
        return 2;
    if (test_3and(9) !== 1)
        return 3;

    if (test_or(11, 33) !== 43)
        return 4;
    if (test_or75(42) !== 107)
        return 5;
    if (test_75or(43) !== 107)
        return 6;

    if (test_xor(93, 107) !== 54)
        return 7;
    if (test_xor101(69) !== 32)
        return 8;
    if (test_101xor(69) !== 32)
        return 9;

    return 0;
}

