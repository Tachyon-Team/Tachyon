function test()
{
    function myClos()
    {
        a += 1;
    }

    var a = 1;

    if (a != 1)
        return 1;

    myClos();

    if (a != 2)
        return 2;

    a++;

    if (a != 3)
        return 3;

    return 0;
}

