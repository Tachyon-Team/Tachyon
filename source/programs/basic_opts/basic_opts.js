function test(v3, v16)
{
    var v3i = pint(v3);
    var v16i = pint(v16);

    //
    // Multiplication by powers of 2
    //    

    if (v3 * 4 !== 12)
        return 1;

    if (4 * v3 !== 12)
        return 2;

    if (v3i * pint(4) !== pint(12))
        return 3;

    if (pint(4) * v3i !== pint(12))
        return 4;

    //
    // Division by powers of 2
    //

    if (v16 / 4 !== 4)
        return 5;

    if (v16i / pint(4) !== pint(4))
        return 6;

    //
    // Addition elimination
    //

    if (v3 + 0 !== v3)
        return 7;

    if (0 + v3 !== v3)
        return 8;

    //
    // Subtraction elimination
    //

    if (v3 - 0 !== v3)
        return 9;

    //
    // Multiplication elimination
    //

    if (v3 * 0 !== 0)
        return 10;

    if (0 * v3 !== 0)
        return 11;

    if (v3 * 1 !== v3)
        return 12;

    if (1 * v3 !== v3)
        return 13;

    //
    // Division elimination
    //

    if (v3 / 1 !== v3)
        return 14;

    if (v3i / pint(1) !== v3i)
        return 15;

    //
    // Bitwise OR elimination
    //

    if ((v3 | 0) !== v3)
        return 16;

    if ((0 | v3) !== v3)
        return 17;

    //
    // Shift elimination
    //

    if (v3 << 0 !== v3)
        return 18;

    if (v3 >> 0 !== v3)
        return 19;

    if (v3 >>> 0 !== v3)
        return 20;

    //
    // Compound expressions
    //

    if (v3 * (v16 * 4) !== 192)
        return 21;

    if (v3i * (v16i * pint(4)) !== pint(192))
        return 22;

    if ((v16 * 4) * v3 !== 192)
        return 23;

    if ((v16i * pint(4)) * v3i !== pint(192))
        return 24;

    if (v3 * (v16 << 2) !== 192)
        return 25;

    if (v3i * (v16i << pint(2)) !== pint(192))
        return 26;

    if ((v16 << 2) * v3 !== 192)
        return 27;

    if ((v16i << pint(2)) * v3i !== pint(192))
        return 28;

    if (4 * (v16 << 2) !== 256)
        return 29;

    if (pint(4) * (v16i << pint(2)) !== pint(256))
        return 30;

    if (4 * (v16 << 0) !== 64)
        return 31;

    if (pint(4) * (v16i << pint(0)) !== pint(64))
        return 32;

    return 0;
}

function proxy()
{
    return test(3, 16);
}

