function foo()
{
    g = 5;
}

foo();

typeAssert(g, '["and", "int", ["val", 5], ["not", "undef"]]');

