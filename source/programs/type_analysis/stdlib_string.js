var str = new String('foo');

typeAssert(String, '"function"');
typeAssert(String.prototype, '"object"');

typeAssert(str, '["and", "object", ["not", "undef"]]');

//typeAssert(str.indexOf, '"function"');

