function foo()
{
    var o = {};
    o.d = true;

    return o;
}

var o = foo();

typeAssert(o.d, '["and", "true", ["not", "int"], ["not", "undef"]]');

