function Ctor()
{
    this.x = 1;
}

var o1 = new Ctor();

o1.x = 'foo';

var o2 = new Ctor();

typeAssert(o1.x, '"string"');

