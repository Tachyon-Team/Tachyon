var foo = undefined;

var foo = function () { return 1; };

function bar(f)
{
    c = f() + f();
}

bar(foo);

typeAssert(c, '["val", 2]');

