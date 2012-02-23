var strLit = 'foo';
var strObj = new String('foo');

typeAssert(String, '"function"');
typeAssert(String.prototype, '"object"');

typeAssert(strObj, '["and", "object", ["not", "undef"]]');

typeAssert(strObj.indexOf, '"function"');
typeAssert(strLit.indexOf, '["and", "function", ["not", "undef"]]');

typeAssert(strObj.length, '"int"');
typeAssert(strLit.length, '["and", "int", ["not", "undef"]]');

typeAssert(strLit[0], '"string"');

