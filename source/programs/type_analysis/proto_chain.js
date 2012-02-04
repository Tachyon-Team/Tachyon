function TheCtor(n)
{
    this.n = n;
}

TheCtor.prototype.n = 'foo';

TheCtor.prototype.p = 'bar';

var o = new TheCtor(7);

// Ensure that the constructor is recognized as a function
typeAssert(TheCtor, '"function"');

// Ensure the prototype object exists
typeAssert(TheCtor.prototype, '["and", "object", ["not", "undef"], ["not", "missing"]]');

// The missing property type should never appear
typeAssert(o.n, '["and", "int", ["not", "missing"]]');

// Check the prototype property type
typeAssert(o.p, '["and", "string", ["not", "int"], ["not", "missing"]]');

// Test missing property is undef, not missing
typeAssert(o.foobar, '["and", "undef", ["not", "missing"]]');

