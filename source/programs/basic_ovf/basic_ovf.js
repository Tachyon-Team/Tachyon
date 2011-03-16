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
    const HALF_MAX = MAX_FIXNUM >> 1;

    return test(1, 2, HALF_MAX, -HALF_MAX);
}

