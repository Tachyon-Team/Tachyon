function bar(n)
{
    function bif()
    {
        var v = ++n;

        return v;
    }

    return bif;
}

function foo(n)
{
    var c = bar(n);

    c();

    c();

    return c();
}
