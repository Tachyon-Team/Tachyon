var a = 'foo';

//var a = b + 1;

var b = 1;

var c = a + b;

typeAssert(c, '["and", "string", ["not", "int"]]');

