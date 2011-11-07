function foo(n)
{
    var sum = 0;

    for (var i = 0; i < n; ++i)
    {
        sum += i;
    }

    return sum;
}

function test()
{
    if (foo(5) !== 10)
        return 1;

    return 0;
}

test();
