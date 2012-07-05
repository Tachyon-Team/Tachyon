for (var i = 1; i < 100; ++i);

var n = 1 / i;

typeAssert(n, '["and", "float", ["not", ["val", 1]]]');

