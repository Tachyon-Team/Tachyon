function lshift(x,y)
{
    return x << y;
}

function rshift(x,y)
{
    return x >> y;
}

function urshift(x,y)
{
    return x >>> y;
}

function foo()
{
    if (lshift(2,3) !== 16)
    {
        return lshift(2,3);
    } 
    
    if (rshift(8,2) !== 2)
    {
        return 2;
    }

    if (rshift(-2, 1) !== -1)
    {
        return 3;
    }

    if (urshift(-2, 30) !== 3)
    {
        return 4;
    }

    return 1 << 2;
}


