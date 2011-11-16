function foo(a)
{
    if (a === true)
        return 1;
    else
        return 2;
}

function test()
{
    if (foo(false) !== 2)
        return 1;

    return 0;
}

test();

