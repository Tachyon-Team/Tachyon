function retComma()
{
    return 2,7;
}

function test()
{
    if (retComma() !== 7)
        return 1;

    var a = (1, 2, 3, 4);
    if (a !== 4)
        return 2;

    var b = (((1, 2), 3), 4);
    if (b !== 4)
        return 3;

    var c = 5;
    var d;
    c = (d = c, c + 1);
    if (d !== 5)
        return 4;
    if (c !== 6)
        return 5;

    return 0;
}

