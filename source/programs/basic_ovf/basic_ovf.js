function test(v1, v2, vHalfMax, vHalfMin)
{
    if (vHalfMax + vHalfMax !== v2 * vHalfMax)
        return 1;

    if (vHalfMin + vHalfMin !== v2 * vHalfMin)
        return 2;

    return 0;
}

function proxy()
{
    const INT_MAX = ~(-1 << boxInt(BOX_NUM_BITS_INT));

    const HALF_MAX = INT_MAX >> 1;

    return test(1, 2, HALF_MAX, -HALF_MAX);
}

