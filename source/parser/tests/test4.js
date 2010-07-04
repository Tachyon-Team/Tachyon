function f(x)
{
    var y = 0;

    do
    {
        y = y + 1;

    } while (true);



    /*
    var y;

    if (true)
        y = x + 1;
    else
        y = x + 2;

    f(y);
    */

    //y[0] = x + 1;

    /*
    var y = 0;

    if (true)
        y = 0;

    f(y);
    */

    /*    
    foo:
    while (true)
    {
        y = y + 1;

        f(y);

        if (true)
            continue;
        else
            y = 3;
    }
    */

    /*
    if (x < 2)
        return 1;
    else
        return f(x-1) + f(x-2);
    */

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
