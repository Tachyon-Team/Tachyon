function f(x)
{
    /*
    if (true)
        f(0);
    else
        f(1);
    */

    if (x < 2)
        return 1;
    else
        return f(x-1) + f(x-2);

    //return true;

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
