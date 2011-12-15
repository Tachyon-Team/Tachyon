function test()
{
    const NUM_ARRS = 300;

    const ARR_LEN = 1000000;

    for (var i = 0; i < NUM_ARRS; ++i)
    {
        var arr = [];

        arr.length = ARR_LEN;
    }

    return 0;
}

