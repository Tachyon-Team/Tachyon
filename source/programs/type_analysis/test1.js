function foo(a, b)
{
    return a + b;
}

function bar()
{
    return foo(5, 3);
}

function bif()
{
    return foo('lestring', 7);
}

function test()
{
    if (bar() !== 8)
        return 1;

    if (bif() !== 'lestring7')
        return 2;

    return 0;
}

test();

