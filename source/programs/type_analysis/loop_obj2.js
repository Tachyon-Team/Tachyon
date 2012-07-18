function bar(n)
{
    for (var i = 0; i < 10; ++i)
    {
        var r = { a:1 };

        typeAssert(r.a, '["and", "int", ["not", "undef"]]');
    }
}

bar(0);

