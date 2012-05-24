function foo(n)
{
    if (n == 0)
        return 0;
    else
        return 1;
}

var r = foo(0);

typeAssert(r, '["and", "int", ["val", 0], ["not", "undef"]]');

