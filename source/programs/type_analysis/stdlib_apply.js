function foo(x, y)
{
    typeAssert(this, '["and", "object", ["not", "null"], ["not", "undef"]]');

    typeAssert(x, '["and", "int", "string"]')
    typeAssert(y, '["and", "int", "string"]')
}

foo.apply(null, [1, 2, 3]);

foo.apply(undefined, ["foo", "bar"]);
