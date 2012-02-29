function foo(n)
{
    var o = { a:1, b:true, c:'foo', d:n };

    return bar(o);
}

function bar(o)
{
    o.d += 7;

    return o;
}

var o = foo(3);

typeAssert(o.a, '["and", "int", ["val", 1], ["not", "undef"]]');
typeAssert(o.b, '["and", "true", ["not", "undef"]]');
typeAssert(o.c, '["and", "string", ["not", "undef"]]');
typeAssert(o.d, '["and", "int", ["not", "undef"]]');

