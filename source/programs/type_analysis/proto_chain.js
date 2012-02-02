function TheCtor(n)
{
    this.n = n;
}

TheCtor.prototype.n = 'foo';

TheCtor.prototype.p = 3;

var o = new TheCtor(7);



// TODO: add type asserts for TheCtor.prototype



// The missing property type should never appear
typeAssert(o.n, '["and", "int", ["not", "missing"]]');



// FIXME!
//typeAssert(o.p, '["and", "int", ["not", "missing"]]');

