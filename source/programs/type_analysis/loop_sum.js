function foo(n)
{
    var sum = 0;

    for (var i = 0; i < n; ++i)
    {
        sum += i;
    }

    typeAssert(sum, '["and", "int", [">=", 0], ["not", "float"], ["not", "string"]]');

    return sum;
}

function test()
{
    if (foo(5) !== 10)
        return 1;

    return 0;
}

test();
