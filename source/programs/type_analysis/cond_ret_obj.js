function foo(b)
{
    return { o:b };
}

function bar(n)
{
    var a1 = {};

    if (n)
        var r = foo(a1);

    typeAssert(r, '["and", "object", "undef"]');

    if (n && r)
        r.o.f = 3;

    typeAssert(a1.f, '["and", "int", "undef", ["val", 3]]');
}

bar(0);
bar(1);

