/*
For now, since we only have integer support, only the integer behavior of the
library is tested.
*/

function test_abs()
{
    if (Math.abs(0) !== 0)
        return 1;
    if (Math.abs(1) !== 1)
        return 2;
    if (Math.abs(-1) !== 1)
        return 3;
    if (Math.abs(5) !== 5)
        return 3;
    if (Math.abs(-5) !== 5)
        return 3;

    return 0;
}

function test_ceil()
{
    if (Math.ceil(0) !== 0)
        return 1;
    if (Math.ceil(3) !== 3)
        return 2;
    if (Math.ceil(-3) !== -3)
        return 3;

    return 0;
}

function test_floor()
{
    if (Math.floor(0) !== 0)
        return 1;
    if (Math.floor(3) !== 3)
        return 2;
    if (Math.floor(-3) !== -3)
        return 3;

    return 0;
}

function test_max()
{
    if (Math.max(0, 1) !== 1)
        return 1;
    if (Math.max(-5, 2) !== 2)
        return 2;
    if (Math.max(1, 2, 9, 3, 4) !== 9)
        return 3;
    if (Math.max(-8, -9, -3, -5, -7) !== -3)
        return 4;

    return 0;
}

function test_min()
{
    if (Math.min(0, 1) !== 0)
        return 1;
    if (Math.min(-5, 2) !== -5)
        return 2;
    if (Math.min(1, 2, 9, -3, 4) !== -3)
        return 3;
    if (Math.min(-8, -9, -3, -5, -11, -7) !== -11)
        return 4;

    return 0;
}

function test_pow()
{
    if (Math.pow(0, 0) !== 1)
        return 1;
    if (Math.pow(1, 0) !== 1)
        return 2;
    if (Math.pow(1, 2) !== 1)
        return 3;
    if (Math.pow(2, 2) !== 4)
        return 4;
    if (Math.pow(3, 9) !== 19683)
        return 5;

    return 0;
}

function test()
{
    var r = test_abs();
    if (r !== 0)
        return 100 + r;

    var r = test_ceil();
    if (r !== 0)
        return 200 + r;

    var r = test_floor();
    if (r !== 0)
        return 300 + r;

    var r = test_max();
    if (r !== 0)
        return 400 + r;

    var r = test_min();
    if (r !== 0)
        return 500 + r;

    var r = test_pow();
    if (r !== 0)
        return 600 + r;

    return 0;
}

