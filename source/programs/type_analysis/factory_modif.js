function Factory()
{
    var o = {};

    o.x = 1;

    return o;
}

var o1 = Factory();

o1.x = 'foo';

var o2 = Factory();

typeAssert(o1.x, '"string"');

