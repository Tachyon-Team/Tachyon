function t1()
{
    var a = pint(1);
    var b = pint(2);

    return boxInt(a + b);
}

function t2(v)
{
    var a = pint(1);
    var b = pint(2);

    if (v)
    {
    }
    else
    {
    }

    return boxInt(a + b);
}

function t3(arg)
{
    if (true)
    {
        var a = pint(1);
        arg = unboxInt(arg);

        return boxInt(a + arg);
    }
    else
    {
        return 0;
    }
}

function t4(arg, v)
{
    if (v)
    {
        var a = pint(1);
        arg = unboxInt(arg);

        var c = boxInt(a + arg);
    }
    else
    {
        var d = 4;
    }

    return 0;
}

function t5()
{
    var sum = u16(0);

    for (var i = 0; i < 10; ++i)
    {
        var a = u16(1);
        sum += a;
    }

    return boxInt(iir.icast(IRType.pint, sum));
}

function t6()
{
    if (true)
    {
        for (var i = 0; i < 5; ++i)
        {
            var ch = u16(0);
        }
    }

    return 0;
}

function test()
{
    if (t1() !== 3)
        return 1;

    if (t2() !== 3)
        return 2;

    if (t3(4) !== 5)
        return 3;

    if (t4(4) !== 0)
        return 4;

    if (t5() !== 10)
        return 5;

    if (t6() !== 0)
        return 6;

    return 0;
}

