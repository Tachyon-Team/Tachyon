function f1()
{
}

function f2()
{
}

var k = 0;
for (var i = 0; i < 100; ++i)
    if (i % 2 === 0)
        k = i;

if (k % 2 === 0)
    var r = f1;
else
    var r = f2;

r.prototype.x = 3;

typeAssert(f1.prototype.x, '["and", "int", "undef"]');
typeAssert(f2.prototype.x, '["and", "int", "undef"]');

