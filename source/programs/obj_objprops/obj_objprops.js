function test()
{
    var o = {};

    var k1 = { toString: function() { return 'foo'; } };

    o[k1] = 3;

    if (o[k1] !== 3)
        return 1;

    if (o['foo'] !== 3)
        return 2;

    if (o.foo !== 3)
        return 3;

    delete o[k1];

    if (o[k1] !== undefined)
        return 4;

    return 0;
}

