function test()
{
    var ra = iir.get_ra();
    var bp = iir.get_bp();

    if (ra === NULL_PTR)
        return 1;

    if (bp === NULL_PTR)
        return 2;

    // Note: 8 byte offset to data

    for (var i = pint(0); i < pint(12); ++i)
    {
        var b = iir.load(IRType.u8, ra, i);
        b = boxInt(iir.icast(IRType.pint, b));
        print(b);
    }







    return 0;
}

