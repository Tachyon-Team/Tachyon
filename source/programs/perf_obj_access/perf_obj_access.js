function foo(n)
{
    var list = null;

    for (var i = 0; i < 10000; ++i)
        list = { next: list, value:i };

    for (var i = 0; i < n; ++i)
    {
        var sum = 0;
        for (var p = list; p !== null; p = p.next)
            sum = sum + p.value;
    }

    //print(sum);

    return sum;
}

function test()
{
    var r = foo(1);

    if (r !== 49995000)
        return 1;

    return 0;
}

