function ctor()
{
    typeAssert(this, '["and", "object", ["not", "undef"]]');

    this.x = 3;
    this.y = 'foo';
}

function test()
{
    var o = new ctor();

    typeAssert(o, '["and", "object", ["not", "undef"]]');
    typeAssert(o.x, '"int"');
    typeAssert(o.y, '"string"');

    if (o.x !== 3)
        return 1;

    if (o.y !== 'foo')
        return 2;

    return 0;
}

