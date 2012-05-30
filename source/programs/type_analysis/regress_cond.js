function foo(n)
{
    var o = {};
    o.d = true;

    return o;
}

var o = foo(3);

typeAssert(o.d, '["and", "true", ["not", "int"]]');

