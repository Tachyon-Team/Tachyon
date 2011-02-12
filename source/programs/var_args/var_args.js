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
    if (x !== UNDEFINED)
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
        y !== UNDEFINED)
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
        z !== UNDEFINED)
    {
        return false;
    }

    return true; 
}

function foo7(x1,x2,x3,x4,x5,x6,x7,x8,x9)
{
    if (x1 !== UNDEFINED ||
        x2 !== UNDEFINED ||
        x3 !== UNDEFINED ||
        x4 !== UNDEFINED ||
        x5 !== UNDEFINED ||
        x6 !== UNDEFINED ||
        x7 !== UNDEFINED ||
        x8 !== UNDEFINED ||
        x9 !== UNDEFINED)
    {
        return false;
    }

    return true;
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
    if (!foo4())
    {
        return 5;
    }

    if (!foo5(1))
    {
        return 6;
    }

    if (!foo6(1,2))
    {
        return 7;
    }

    if (!foo7())
    {
        return 8;
    }

    return 0;
}

