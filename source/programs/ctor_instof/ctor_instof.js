function A()
{
}
A.prototype = {};

function B()
{
}
B.prototype = {};

function test()
{
    var a = new A();
    var b = new B();

    if (a instanceof B)
        return 1;

    if (b instanceof A)
        return 1;

    if (!(a instanceof A))
        return 1;

    if (!(b instanceof B))
        return 1;

    if (2 instanceof A)
        return 1;

    return 0;
}

