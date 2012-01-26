foo('bar');

var g2 = g1 + 2;

typeAssert(g2, '["and", "int", "string", ["not", "undef"]]');

