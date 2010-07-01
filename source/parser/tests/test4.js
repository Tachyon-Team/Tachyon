function f(x)
{
    /*
    var y;

    if (true)
        y = x + 1;
    else
        y = x + 2;

    f(y);
    */

    //y[0] = x + 1;

    if (x < 2)
        return 1;
    else
        return f(x-1) + f(x-2);

    /*
    if (x && y)
    {
    }
    */

    //x.foo(5);

    /*
    function foo()
    {
    }

    print('hi');
    */
}

print(f(20));
