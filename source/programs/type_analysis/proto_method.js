function Foo()
{
}

Foo.prototype.m = function () {};

var o = new Foo();

typeAssert(Foo.prototype, '"object"');

typeAssert(o.m, '"function"');

