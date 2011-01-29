function foo()
{
    var arr = [];

    if (arr.length !== 0)
        return 1;

    arr.length = 4;

    if (arr.length !== 4)
        return 1;

    for (var i = 0; i < arr.length; ++i)
        if (arr[i])
            return 1;

    arr[arr.length] = 3;

    if (arr.length != 5)
        return 1;

    arr.length = 1;

    if (arr.length !== 1)
        return 1;

    return 0;
}
