function ctor()
{
    this.x = 3;
}

a = new ctor();

a.x = 'foo';

b = new ctor();

a.y = 3;

typeAssert(a.y, '["and", "int"]');
typeAssert(b.y, '["and", "undef"]');

typeAssert(a.x, '["and", "string"]');
typeAssert(b.x, '["and", "int"]');

