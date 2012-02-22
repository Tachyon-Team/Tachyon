typeAssert(Function, '"function"');

typeAssert(Function.prototype, '"object"');

// Function inherited from Object.prototype
typeAssert(Function.prototype.hasOwnProperty, '["and", "function", ["not", "undef"]]');

// Function inherited from Function.prototype
typeAssert(Object.call, '["and", "function", ["not", "undef"]]');

function foo() {}

// Function inherited from Function.prototype
typeAssert(foo.call, '["and", "function", ["not", "undef"]]');

