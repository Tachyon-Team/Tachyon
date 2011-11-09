function foo(n)
{
    var o = { a:1, b:true, c:'foo'};

    if (n == 3)
        o.d = true;
    else
        o.d = 3;

    return o;
}

function test()
{
    if (foo(3).d !== true)
        return 1;

    return 0;
}

test();

