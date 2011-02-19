function test()
{
    var a = [];

    var newSize = 100;

    a.length = newSize;

    if (a.length != newSize)
        return 1;

    for (var i = 0; i < newSize; ++i)
    {
        if (a[i] !== undefined)
            return 2;
    }

    return 0;
}

