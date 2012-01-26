function bar(n)
{
    var list = [];

    for (var i = 0; i < 10; ++i)
    {
        var a1 = {};

        if (n)
            var r = { a:a1 };

        list[i] = a1;
    }

    if (n)
        r.a.f = 3;

    typeAssert(a1, '["and", "object", "undef"]');
    typeAssert(r, '["and", "object", "undef"]');
}

bar(0);
bar(1);

