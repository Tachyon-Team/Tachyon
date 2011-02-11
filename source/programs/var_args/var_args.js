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

    return 0;
}

