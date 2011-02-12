function foo()
{
    return 1;
}


function foo1(x)
{
    return x;
}

function foo2(x,y)
{
    return x+y;
}

function foo3(x,y,z)
{
    return x+y+z;
}

function foo4(x,y)
{
    return y;
}

function foo5(x,y,z)
{
    return z;
}

function foo6(x1,x2,x3,x4,x5,x6,x7,x8,x9)
{
    return x9;
}

function foo_proxy()
{
    // Pass more arguments than expected
    if (foo(0,1,2,3,4,5,6) !== 1)
    {
        return 1;
    }

    if (foo1(0,1,2,3,4,5,6) !== 0)
    {
        return 2;
    }

    if (foo2(0,1,2,3,4,5,6) !== 1)
    {
        return 3;
    }

    if (foo3(0,1,2,3,4,5,6) !== 3)
    {
        return 4;
    }

    // Pass less arguments than expected
    if (foo1() !== UNDEFINED)
    {
        return 5;
    }

    if (foo4(1) !== UNDEFINED)
    {
        return 6;
    }

    if (foo5(1,2) !== UNDEFINED)
    {
        return 7;
    }

    if (foo6() !== UNDEFINED)
    {
        return 8;
    }

    return 0;
}

