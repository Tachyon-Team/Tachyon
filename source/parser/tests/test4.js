function f(c1, c2)
{
    var v = 6;

    k = c1;

    var y = c1++;
    var z = c1++;

    c1 = z + 2;

    print(y);

    /*
    arguments[1] = 5;

    c1 = 3;

    print(c1);
    */

    /*
    var i1 = iir.unbox(IRType.INT32, c1);
    var i2 = iir.unbox(IRType.INT32, c1);
    
    var v = i2;
    while (true)
    {
        var v = i1;
        print(v);
    }

    print(v);
    */


    /*    
    var i1 = iir.unbox(IRType.INT32, c1);
    var i2 = iir.unbox(IRType.INT32, c2);
    
    var a;
    if (a = iir.add_ovf(i1, i2))
    {
        //var v = iir.box(IRType.INT32, a);
        //return v;

        var v = i1;
    }
    else
    {
        var v = i2;
    }
    
    print(v);
    */




    /*
    var v = -1;

    switch (c1)
    {
        case 0:
        v = 0;
        
        if (c2)
            break;

        v = 0.5;

        case 1:
        print(v);
        v = 1;
        break;

        case 2:
        v = 2;
        break;
    }

    print(v);
    */

    /*
    if (x < 2)
        return x;
    else
        return f(x-1) + f(x-2);
    */

    /*
    for (var i = 0; i < 10; ++i)
    {
        while (true)
        {
            print(i);
        }

        if (true)
        {
        }
        else
        {
        }
    }

    print(i);
    */

    /*
    for (var v in lst)
    {
        print(v);
    }
    */

    /*
    try
    {
        print(f);

        throw 'foo';
    }
    catch (e)
    {
    }
    */

    /*
    while (true)
    {
        try
        {
            break foo;
        }
        catch (e)
        {
        }
    }
    */

    /*
    var y;

    switch (a)
    {
        case 'foo':
        y = a + 1;
        break;

        default:
        y = a + 2;

        case 'bar':
        y = a + 3;
    }

    print(y);
    */

    /*
    x = 3;
    x++;

    if (true)
        x += y;
    else
        x = x + 1;

    print(x);
    */

    /*
    with (obj)
    {
        x[0] = x[1];
    }
    
    print(x);
    */

    /*    
    var x;

    try
    {
        if (true)
        {
            x = 1;
            
            //new foo();
            print(x);
            //throw a;
            //throw new foo();
        }
        else
        {
            x = 2;
            throw a;
        }
    }

    catch (e)
    {
        x = 3;
    }

    finally
    {
        print(x);
        print(e);
    }
    */
    

    /*
    var x;

    try
    {
        if (true)
        {
            x = 1;
            throw new foo();
        }
        else
        {
            x = 2;
            throw a;
        }
    }
    catch (e)
    {
        print(e);
    }
    */

    /*
    function g(z)
    {
        function h()
        {
            u = u + 1;
        }

        a;

        y;
        z;
        w;
    }

    var y = 3;
    var z;
    var w;
    var u;

    g(2);
    */
    
    //a = function () { y = 2; };
    
    //a = { x: 1, y:'foo', z: (x+1) };
    //a = [1, 'foo', (x+1)];
    //y[0] = x + 1;
    //this.foo = a;
    //a = x? y:z;
    //a = x && y;
    //a = x || y;
    //a = new foo(1, 2, 3);
    //a += 1;
    //++x;
    //typeof x;
    //x instanceof y
     
    /*
    var y = 0;

    do
    {
        if (true)
        {
            y = 1;
            break;
        }

        y = y + 1;

    } while (true);

    f(y);
    */

    /*
    var y = 0;

    while (true)
    {
        if (true)
        {
            y = 3;
            break;
        }

        y = y + 1;
    }    

    f(y);
    */

    /*
    for (var i = 0; i < 10; ++i)
    {
        if (true)
            break;
    }

    print(i);
    */

    /*    
    var y = 0;

    if (true)
        y = 1;

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

//print(f(20));

