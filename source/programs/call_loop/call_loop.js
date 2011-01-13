function sum(v1, v2)
{
    return v1 + v2;
}

function foo()
{
    var a = sum(7, 3);

    for (var i = 0; i < 5; ++i)
    {
        a += 1;
    }

    return a;
}

return foo();
