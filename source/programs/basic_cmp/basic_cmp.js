/**
Comparison operator test.
The value of the argument passed should be 5.
*/
function test(v5)
{
    var v6 = v5 + 1;

    if (v5 !== v5)
        return 1;

    if (v5 !== 5)
        return 2;

    if (v6 === v5)
        return 3;

    if (v5 != v5)
        return 4;

    if (v5 != 5)
        return 5;

    if (v6 == v5)
        return 6;

    if (v6 < 5)
        return 7;

    if (v5 < 5)
        return 8;

    if (v6 < v6)
        return 9;

    if (v5 > 5)
        return 10;

    if (v5 > 6)
        return 11;

    if (v6 > v6)
        return 12;

    if (v5 >= 5)
    {}
    else
        return 13;

    if (v5 >= v6)
        return 14;

    if (v5 <= 5)
    {}
    else
        return 15;

    if (v6 <= v5)
        return 16;

    return 0;
}

