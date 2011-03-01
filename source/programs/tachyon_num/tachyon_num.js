function test()
{
    var min = getIntMin(30, false);
    var max = getIntMax(30, false);

    if (!num_lt(min, max))
        return 1;

    if (!num_lt(min, 0))
        return 2;

    if (!num_gt(max, 0))
        return 3;

    var a = num_shift(1, 29);

    if (!num_gt(a, 1))
        return 4;

    var b = num_shift(a, -2);

    if (!num_gt(b, 1))
        return 5;

    if (!num_gt(a, b))
        return 6;

    var c = num_sub(b, 1);
    var d = num_add(c, 1);

    if (!num_eq(d, b))
        return 7;

    //print('min: ' + num_to_string(min));
    //print('max: ' + num_to_string(max));
    //print('(1<<29): ' + num_to_string(a));

    return 0;
}

