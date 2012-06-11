function Ctor(val)
{
    this.val = val;
}

function maker(val, i)
{
    if (i === 0)
        return new Ctor(val);
    else
        return new Ctor(val);
}

var o1 = maker(1, 0);
var o2 = maker(1, 1);

typeAssert(o1.val, '["and", "int", ["not", "undef"]]');
typeAssert(o2.val, '["and", "int", ["not", "undef"]]');

