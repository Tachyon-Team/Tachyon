function foo(n)
{
    var o = { a:1, b:true, c:'foo', d:n };

    return bar(o);
}

function bar(o)
{
    o.d += 7;

    return o;
}

function test()
{
    if (foo(3).d !== 10)
        return 1;

    return 0;
}

test();

