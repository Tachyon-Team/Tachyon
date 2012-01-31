var g;

function foo()
{
    var a = {};

    g = {};

    bif();

    g = a;

    return a;
}

function bif()
{
    typeAssert(g, '["and", "object", ["not", "undef"]]');

    g.f = 3;

    typeAssert(g.f, '"int"');
}

var o1 = foo();
var o2 = foo();

bif();

typeAssert(o1, '"object"');
typeAssert(o2, '"object"');
typeAssert(o2.f, '"undef"');
typeAssert(o2.f, '"int"');

