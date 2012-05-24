function foo(n)
{
    if (n)
        return 'foo';
    else
        return 'bar';
}

foo(0);
foo(1);

var r = foo(3);

typeAssert(r, '["and", "string", ["not", ["val", "foo"]], ["not", ["val", "bar"]]]');

