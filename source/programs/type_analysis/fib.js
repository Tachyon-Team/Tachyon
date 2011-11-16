function fib(n)
{
    if (n < 2)
        return n;

    return fib(n-1) + fib(n-2);
}

function test()
{
    if (fib(6) !== 8)
        return 1;

    return 0;
}

test();

