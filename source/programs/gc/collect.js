function test()
{
    var liveObj1 = { v:3 };

    gcCollect();

    if (liveObj1.v !== 3)
        return 1;

    return 0;
}

