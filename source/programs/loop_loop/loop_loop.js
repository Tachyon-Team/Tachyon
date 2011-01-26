function bar(v1, v2, v3)
{
    return v1 + v2 + v3;
}

function foo(n)
{
    var a = 0;

    for (var i = 0; i < n; ++i)
    {
        a += bar(i, i, i);
    }

    for (var i = 0; i < n; ++i)
    {
        a += bar(i, i, i);
    }

    return a;
}

