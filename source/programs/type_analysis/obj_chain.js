var o1 = {};

var o2 = {};

var o3 = {};

o3.o = o2;

o2.o = o1;

o3.o.o.v = 3;

typeAssert(o1.v, '["and", "int"]');
//typeAssert(o1.v, '["and", "int", ["not", "undef"]]');

