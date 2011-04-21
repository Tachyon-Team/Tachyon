function test()
{
    var t1 = currentTimeMillis();

    if (t1 < 0)
        return 1;

    var t2 = currentTimeMillis();

    if (t2 < 0)
        return 2;

    if (t2 < t1)
        return 3;

    if (t2 - t1 > 1000)
        return 4;

    return 0;
}
