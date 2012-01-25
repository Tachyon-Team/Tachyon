function init(o)
{
    o.a = 1;
    o.b = 'foo';
    o.m = function () { return 1; };
}

function ctor()
{
    var o = {};

    init(o);

    return o;
}

var o = ctor();

typeAssert(o, '"object"');
typeAssert(o.a, '["and", "int", ["not", "float"]]');
typeAssert(o.b, '"string"');
typeAssert(o.m, '"function"');

