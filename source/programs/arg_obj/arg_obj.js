function foo()
{
    return arguments.length + 
           arguments[0] + 
           arguments[1] +
           arguments[2] + 
           arguments[3] + 
           arguments[4] + 
           arguments[5] + 
           arguments[6] + 
           arguments[7] +
           arguments[8] + 
           arguments[9];
}

function foo2(x)
{
    return arguments.length;
}

function foo_proxy()
{
    if (foo(0,1,2,3,4,5,6,7,8,9) !== 55)
    {
        return 1;
    }

    if (foo2(1) !== 1)
    {
        return 2;
    }

    if (foo2() !== 0)
    {
        return 3;
    }

    return 0;
}

