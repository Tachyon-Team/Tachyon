function Foo(v)
{
    this.v = v;
}

Foo.prototype.theFun = function ()
{
}

Foo.o1 = new Foo(1);
Foo.o2 = new Foo(2);
Foo.o3 = new Foo(3);

typeAssert(Foo.o1.theFun, '"function"');
