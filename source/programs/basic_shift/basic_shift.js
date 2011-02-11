function lshift_test(x,y)
{
    return x << y;
}

function lshift_test2(x)
{
    return x << 3;
}

function rshift_test(x,y)
{
    return x >> y;
}

function rshift_test2(x)
{
    return x >> 2;
}

function rshift_test3(x)
{
    return x >> 1;
}

function urshift_test(x,y)
{
    return x >>> y;
}

function urshift_test2(x)
{
    return x >>> 30;
}

function foo()
{
    if (lshift_test(2,3) !== 16)
    {
        return 1;
    } 

    if (lshift_test2(2) !== 16)
    {
        return 2;
    } 

    if (rshift_test(8,2) !== 2)
    {
        return 3;
    }

    if (rshift_test2(8) !== 2)
    {
        return 4;
    }

    if (rshift_test(-2, 1) !== -1)
    {
        return 5;
    }

    if (rshift_test3(-2) !== -1)
    {
        return 6;
    }

    if (urshift_test(-2, 30) !== 3)
    {
        return 7;
    }

    if (urshift_test2(-2) !== 3)
    {
        return 8;
    }

    return 0;
}

