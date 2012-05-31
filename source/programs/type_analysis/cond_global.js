function foo(n)
{
    g1 = 3;

    if (n === 0)
        g2 = 'foo';
}

foo(0);
foo(1);

typeAssert(g1, '["and", "int", ["val", 3], ["not", "undef"]]');
typeAssert(g2, '["and", "string", "undef"]');

