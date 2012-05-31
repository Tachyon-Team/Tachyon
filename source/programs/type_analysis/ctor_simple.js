function ctor(z)
{
    typeAssert(this, '["and", "object", ["not", "undef"]]');
    typeAssert(z, '["and", "false", ["not", "undef"]]');

    this.x = 3;
    this.y = 'foo';
    this.z = z;
}

var o = new ctor(false);

typeAssert(o, '["and", "object", ["not", "undef"]]');
typeAssert(o.x, '["and", "int", ["not", "undef"]]');
typeAssert(o.y, '["and", "string", ["not", "undef"]]');
typeAssert(o.z, '["and", "false", ["not", "undef"]]');

