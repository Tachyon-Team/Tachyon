function ctor(z)
{
    typeAssert(this, '["and", "object", ["not", "undef"]]');
    typeAssert(z, '["and", "false", ["not", "undef"]]');

    this.x = 3;
    this.y = 'foo';
    this.z = false;
}

function test()
{
    var o = new ctor();

    typeAssert(o, '["and", "object", ["not", "undef"]]');
    typeAssert(o.x, '["and", "int", ["not", "undef"]]');
    typeAssert(o.y, '["and", "string", ["not", "undef"]]');
    typeAssert(o.z, '["and", "false", ["not", "undef"]]');

    if (o.x !== 3)
        return 1;

    if (o.y !== 'foo')
        return 2;

    if (o.z !== false)
        return 3;

    return 0;
}

