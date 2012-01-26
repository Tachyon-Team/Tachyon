function foo(n)
{
    var a1 = {};
    var a2 = {};

    if (n)
        var b = a1;
    else
        var b = a2;

    b.foo = 3;

    typeAssert(b, '["and", "object", ["not", "undef"]]');
    typeAssert(a1.foo, '["and", "int", "undef"]');
    typeAssert(a2.foo, '["and", "int", "undef"]');

    return b;
}

function test()
{
    var o1 = foo(0);

    if (o1.foo !== 3)
        return 1;

    var o2 = foo(1);

    if (o2.foo !== 3)
        return 2;

    return 0;
}

