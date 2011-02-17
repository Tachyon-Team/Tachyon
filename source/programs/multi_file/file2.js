var global1;

var global2 = global2 + 1;

function test()
{
    if (global1 !== 97966)
        return 1;

    if (global2 !== 3476)
        return 2;

    if (global3 !== undefined)
        return 3;

    if (bar() != 31337)
        return 4;

    return 0;
}

