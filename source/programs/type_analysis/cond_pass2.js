function foo(b, c)
{
    typeAssert(b, '["and", "object", ["not", "undef"]]');
    typeAssert(c, '["and", "object", ["not", "undef"]]');

    b.f = 3;

    typeAssert(b.f, '["and", "int", ["val", 3]]');
    typeAssert(c.f, '["and", "int", ["val", 3]]');
}

function bar(n)
{
    var a1 = {};

    if (n)
        foo(a1, a1);

    typeAssert(a1.f, '["and", "int", "undef", ["val", 3]]');
}

bar(0);
bar(1);

