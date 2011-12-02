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

