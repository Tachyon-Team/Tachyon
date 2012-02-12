function Ctor()
{
    return [];
}

var o = new Ctor();

typeAssert(o, '["and", "array", ["not", "object"]]');

