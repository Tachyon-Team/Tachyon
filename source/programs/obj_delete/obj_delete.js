function test()
{
    var a = { x:1, y:2, z:3 };

    if (a.x !== 1)
        return 1;

    delete a.x;

    if (a.x)
        return 2;
    if (a.y != 2)
        return 3;
    if (a.z != 3)
        return 4;

    var b = [0,1,2,3];

    if (b.length != 4)
        return 5;
    if (b[0] != 0)
        return 6;
    if (b[1] != 1)
        return 7;
    if (b[2] != 2)
        return 8;
    if (b[3] != 3)
        return 9;

    delete b[1];

    if (b.length != 4)
        return 10;
    if (b[0] != 0)
        return 11;
    if (b[1])
        return 12;
    if (b[2] != 2)
        return 13;
    if (b[3] != 3)
        return 14;

    delete b[3];

    if (b.length != 3)
        return 15;
    if (b[0] != 0)
        return 16;
    if (b[1])
        return 17;
    if (b[2] != 2)
        return 18;
    if (b[3])
        return 19;

    return 0;
}
