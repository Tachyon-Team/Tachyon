function A()
{
}
A.prototype = {};

function B()
{
}
B.prototype = {};

function C()
{
}
C.prototype = new B();

function test()
{
    var a = new A();
    var b = new B();
    var c = new C();

    if (!(a instanceof A))
        return 1;
    if (a instanceof B)
        return 2;
    if (a instanceof C)
        return 3;

    if (b instanceof A)
        return 4;
    if (!(b instanceof B))
        return 5;
    if (b instanceof C)
        return 6;

    if (c instanceof A)
        return 7;
    if (!(c instanceof B))
        return 8;
    if (!(c instanceof C))
        return 9;

    if (2 instanceof A)
        return 10;

    return 0;
}

