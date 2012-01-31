function foo(n)
{
    var a1 = {};

    if (n)
        bar(a1);

    return a1;
}

function bar(o)
{
    typeAssert(o, '["and", "object", ["not", "undef"]]');

    o.f = 3;
}

var o1 = foo(0);
var o2 = foo(1);

typeAssert(o1, '"object"');
typeAssert(o2, '"object"');
typeAssert(o1.f, '"undef"');
typeAssert(o2.f, '"int"');

