function bar(n)
{
    function bif()
    {
        return n;
    }

    return bif;
}

function foo(n)
{
    var c = bar(n);

    return c();    
}
