function bar(o)
{
    typeAssert(o, '["and", "object", "int"]');

    o.f = 3;
}

function foo()
{
    var a = {};

    bar(3);

    return a;
}

o1 = foo();
o2 = foo();

bar(o1);

typeAssert(o1, '"object"');
typeAssert(o1.f, '"int"');

