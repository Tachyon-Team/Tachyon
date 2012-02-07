function sum()
{
    var s = 0;

    for (var i = 0; i < arguments.length; ++i)
        s += arguments[i];

    return s;
}

sum(1, 2, 3, 4);
