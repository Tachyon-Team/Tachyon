var arr = new Array(10);

typeAssert(Array, '"function"');
typeAssert(Array.prototype, '"object"');

typeAssert(arr, '["and", "array", ["not", "object"], ["not", "undef"]]');

typeAssert(arr.push, '"function"');
typeAssert(arr.pop, '"function"');

