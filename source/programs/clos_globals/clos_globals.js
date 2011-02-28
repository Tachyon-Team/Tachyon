var g = 3;

function callee(n)
{
    if (g != 3)
        return 1;

    function myClos()
    {
        return ++n;
    }

    if (g != 3)
        return 2;

    myClos();

    if (g != 3)
        return 3;

    return 0;
}

function test()
{
    if (callee(3) != 0)
        return 0;

    return 0;
}

