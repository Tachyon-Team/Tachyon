function test()
{
    const ARR_LEN = 100000;

    var ctx = iir.get_ctx();

    // Shrink the heap for testing
    var heapSize = get_ctx_heapsize(ctx);
    shrinkHeap(puint(2000000));

    // Get the initial collection count
    var initGCCount = get_ctx_gccount(ctx);

    for (;;)
    {
        var curGCCount = get_ctx_gccount(ctx);
        if (curGCCount >= initGCCount + u32(3))
            break;

        var arr = [];

        arr.length = ARR_LEN;
    }

    // Restore the old heap size
    set_ctx_heapsize(ctx, heapSize);

    gcCollect();

    return 0;
}

