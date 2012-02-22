function init()
{
    var o = {};

    var s = 0;
    for (var i = 0; i < 10; ++i)
    {
        s += i
    }

    o.f = 3;

    return o;
}

var o = init();

typeAssert(o.f, '["and", "int", ["val", 3], ["not", "undef"], ["not", "missing"]]');

