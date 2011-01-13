function bar(n)
{
    var a = 0;

    for (var i = 0; i < n; ++i)
    {
        a += 2;
    }

    return a;
}

function bif(v1, v2, v3)
{
    return (v1 + v2) * v3;
}

function foo()
{
    var v = 3;

    v += bar(5);        // v += 10

    v += 3;

    v += bif(2,3,4);    // v += 20

    v += 3;

    // v === 156
    return v;
}

return foo();
