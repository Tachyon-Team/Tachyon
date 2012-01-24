function foo(a, b)
{
    return a + b;
}

var c = foo(1, 2);

typeAssert(c, 'int');

