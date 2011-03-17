function test()
{
    function foo() { thevar; }

    var thevar = null;

    if (thevar !== null)
        return 1;

    return 0;
}

