function foo(a, b)
{
    return a + b;
}

typeAssert(foo, '["and", "function", ["not", "undef"]]');

var c = foo(1, 2);

typeAssert(c, '["and", "int", ["val", 3]]');

