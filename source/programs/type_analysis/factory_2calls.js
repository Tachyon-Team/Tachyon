function factory()
{
    return {};
}

a = factory();
b = factory();

a.f = 3;

typeAssert(a.f, '["and", "int", "undef"]');
typeAssert(b.f, '["and", "int", "undef"]');

