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

function foo4(x)
{
    if (x !== undefined)
    {
        return false;
    } else
    {
        return true;
    }
}

function foo5(x,y)
{
    if (x !== 1 ||
        y !== undefined)
    {
        return false;
    } else
    {
        return true;
    }
}

function foo6(x,y,z)
{
    if (x !== 1 || 
        y !== 2 || 
        z !== undefined)
    {
        return false;
    }

    return true; 
}

function foo7(x1,x2,x3,x4,x5,x6,x7,x8,x9)
{
    if (x1 !== undefined ||
        x2 !== undefined ||
        x3 !== undefined ||
        x4 !== undefined ||
        x5 !== undefined ||
        x6 !== undefined ||
        x7 !== undefined ||
        x8 !== undefined ||
        x9 !== undefined)
    {
        return false;
    }

    return true;
}

function foo_proxy()
{
    // Pass more arguments than expected
    if (foo(1) !== 1)
    {
        return 1;
    }

    if (foo(1,2) !== 1)
    {
        return 2;
    }

    if (foo(1,2,3) !== 1)
    {
        return 3;
    }

    if (foo(0,1,2,3,4,5,6) !== 1)
    {
        return 4;
    }


    if (foo1(0,1,2,3,4,5,6) !== 0)
    {
        return 5;
    }

    if (foo2(0,1,2,3,4,5,6) !== 1)
    {
        return 6;
    }

    if (foo3(0,1,2,3,4,5,6) !== 3)
    {
        return 7;
    }

    // Pass less arguments than expected
    if (!foo4())
    {
        return 8;
    }

    if (!foo5(1))
    {
        return 9;
    }

    if (!foo6(1,2))
    {
        return 10;
    }

    if (!foo7())
    {
        return 11;
    }

    return 0;
}

