function foo(v)
{
    function bar()
    {
        return v;
    }

    function bif()
    {
        return bar();
    }

    return bif;
}

function test(v)
{
    var f = foo(v);

    return f();
}
