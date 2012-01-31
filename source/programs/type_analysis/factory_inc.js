function foo(n)
{
    var a1 = {};

    a1.n = 1;

    bar(a1);

    typeAssert(a1.n, '["and", "int", [">=", 0], ["not", "undef"]]');

    return a1;
}

function bar(o)
{
    typeAssert(o, '"object"');

    o.n += 1;
}

var o1 = foo(0);
var o2 = foo(1);

bar(o1);

typeAssert(o1, '"object"');
typeAssert(o2, '"object"');
typeAssert(o1.n, '["and", "int", [">=", 0], ["not", "undef"]]');

