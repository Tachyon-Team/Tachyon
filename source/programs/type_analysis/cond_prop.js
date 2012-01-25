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
    var o = foo(3);

    typeAssert(o.d, '["and", "true", ["not", "int"]]');

    if (o.d !== true)
        return 1;

    return 0;
}

test();

