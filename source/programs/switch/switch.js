function switch_int(intVal)
{
    switch (intVal)
    {
        case -1: return 1;
        case 0 : return 2;
        case 1 : return 3;
        default: return 4;
    }
}

function switch_int_nodef(intVal)
{
    switch (intVal)
    {
        case -1: return 1;
        case 0 : return 2;
        case 1 : return 3;
    }

    return 4;
}

function switch_empty(val)
{
    switch (val)
    {
    }

    return 3;
}

function switch_testvar(val)
{
    var v;
    switch (v = val)
    {
        default:
        return v;
    }
}

function test()
{
    if (switch_int(-1) !== 1)
        return 101;
    if (switch_int(0) !== 2)
        return 102;
    if (switch_int(1) !== 3)
        return 103;
    if (switch_int(-100) !== 4)
        return 104;

    if (switch_int_nodef(-1) !== 1)
        return 201;
    if (switch_int_nodef(0) !== 2)
        return 202;
    if (switch_int_nodef(1) !== 3)
        return 203;
    if (switch_int_nodef(-100) !== 4)
        return 204;

    if (switch_empty(1337) !== 3)
        return 301;

    if (switch_testvar(777) !== 777)
        return 401;

    return 0;
}

