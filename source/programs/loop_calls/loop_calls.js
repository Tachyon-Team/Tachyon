function bar(v)
{
    return v + 1;
}

function bif(v)
{
    return v + 2;
}

function foo(a)
{
    a += bar(a);

    a += 1;

    for (var i = 0; i < 5; ++i)
    {
        a += bif(a);

        a += 2;

        a += bar(a);
    }

    a += 3;

    a += bif(a);

    return a;
}

