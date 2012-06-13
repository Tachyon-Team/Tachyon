function bar()
{
    for (var i = 0; i < 5; ++i)
    {
        var r = {};

        bif();
    }

    typeAssert(r, '"object"');
}

bar();

