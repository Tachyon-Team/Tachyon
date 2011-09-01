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

function switch_fallvar(val)
{
    var v = 1;

    switch (val)
    {
        case 1:
        v += 1;
        case 2:
        v += 1;
        case 3:
        v += 2;
        break;

        default:
        v += 101;
    }

    return v + 3;
}

function switch_str(val)
{
    switch (val)
    {
        case 'up':
        return 'down';

        case 'north':
        return 'south';

        case 'in':
        return 'out';

        default:
        return 'here';
    }
}

function switch_mixed(val)
{
    function fn() { return 1337; }

    switch (val)
    {
        case 1:
        return 2;

        case 'a':
        return 'b';

        case switch_mixed:
        return 'lol';

        case fn():
        return '13371';

        default:
        return -1;
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

    if (switch_fallvar(1) != 8)
        return 501;
    if (switch_fallvar(2) != 7)
        return 502;

    if (switch_str('up') != 'down')
        return 601;
    if (switch_str('north') != 'south')
        return 602;
    if (switch_str('in') != 'out')
        return 603;
    if (switch_str('') != 'here')
        return 604;

    if (switch_mixed(1) != 2)
        return 701;
    if (switch_mixed('a') != 'b')
        return 702;
    if (switch_mixed(switch_mixed) != 'lol')
        return 703;
    if (switch_mixed(1337) != 13371)
        return 704;
    if (switch_mixed(null) != -1)
        return 705;

    return 0;
}

