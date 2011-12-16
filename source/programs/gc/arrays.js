function test()
{
    const NUM_ARRS = 300;

    const ARR_LEN = 1000000;

    var ctx = iir.get_ctx();
    var initGCCount = get_ctx_gccount(ctx);

    for (var i = 0; i < NUM_ARRS; ++i)
    {
        var curGCCount = get_ctx_gccount(ctx);
        if (curGCCount >= initGCCount + u32(2))
            break;

        var arr = [];

        arr.length = ARR_LEN;
    }

    gcCollect();

    return 0;
}

