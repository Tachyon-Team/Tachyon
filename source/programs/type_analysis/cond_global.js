function foo(n)
{
    g1 = 3;

    if (n === 0)
        g2 = 'foo';
}

function test()
{
    foo(0);
    foo(1);

    typeAssert(g1, '["and", "int", ["val", 3], ["not", "undef"]]');
    typeAssert(g2, '["and", "string", "undef"]');

    if (g1 !== 3)
        return 1;

    if (g2 !== 'foo')
        return 2;

    return 0;
}

test();

