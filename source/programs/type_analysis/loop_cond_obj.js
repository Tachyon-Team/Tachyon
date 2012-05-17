function bar(n)
{
    var list = [];

    for (var i = 0; i < 10; ++i)
    {
        var a1 = {};

        if (n)
        {
            typeAssert(a1, '["and", "object", ["not", "undef"]]');

            var r = { a:a1 };

            typeAssert(r.a, '["and", "object", ["not", "undef"]]');
        }

        typeAssert(r.a, '["and", "object", ["not", "undef"]]');

        list[i] = a1;
    }

    if (n)
        r.a.f = 3;

    list[0].g = 3;

    typeAssert(a1, '["and", "object", "undef"]');
    typeAssert(r, '["and", "object", "undef"]');
    typeAssert(list[1].g, '"undef"');
}

bar(0);
bar(1);

