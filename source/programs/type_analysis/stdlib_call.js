function foo(x, y)
{
    typeAssert(this, '["and", "object", ["not", "null"], ["not", "undef"]]');

    typeAssert(x, '["and", "int", ["not", ["val", 1]]]');
    typeAssert(y, '["and", "int", "string"]');
}

foo.call(null, 1, 2);
foo.call(null, 3, "bar");

