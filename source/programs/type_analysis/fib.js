function fib(n)
{
    if (n < 2)
        return n;

    return fib(n-1) + fib(n-2);
}

function test()
{
    var r = fib(6);

    typeAssert(r, '["and", "int", ["not", "float"], ["not", "string"]]');

    if (r !== 8)
        return 1;

    return 0;
}

test();

