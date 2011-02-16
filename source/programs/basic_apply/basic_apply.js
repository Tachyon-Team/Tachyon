function foo()
{
    return;
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

function foo4(a,b,c,d)
{
    return a+b+c+d;
}

function foo5(a,b,c,d,e)
{
    return a+b+c+d+e;
}

function foo6(x)
{
    this.push(x);
    return this.length;
}

function foo7(i,x)
{
    this[i] = x;
}

function foo_proxy()
{
    if(foo.apply(undefined, []) !== undefined)
    {
        return 1;
    }

    if(foo1.apply(undefined, [1]) !== 1)
    {
        return 2;
    }

    if(foo2.apply(undefined, [1,2]) !== 3)
    {
        return 3;
    }

    if(foo3.apply(undefined, [1,2,3]) !== 6)
    {
        return 4;
    }

    if(foo4.apply(undefined, [1,2,3,4]) !== 10)
    {
        return 5;
    }

    if(foo5.apply(undefined, [1,2,3,4,5]) !== 15)
    {
        return 6;
    }

    if(foo6.apply([], [1]) !== 1)
    {
        return 7;
    }

    var a = [];
    foo7.apply(a, [42,1337]);
    if (a[42] !== 1337)
    {
        return 8;
    }

    return 0;
}
