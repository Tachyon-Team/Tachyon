function array_eq(a1, a2)
{
    if (a1.length !== a2.length)
        return false;

    for (var i = 0; i < a1.length; ++i)
        if (a1[i] !== a2[i])
            return false;

    return true;
}

function test_push()
{
    var a = [0,1,2];

    a.push(3);
    if (!array_eq(a, [0,1,2,3]))
        return 1;

    a.push(4,5)
    if (!array_eq(a, [0,1,2,3,4,5]))
        return 2;

    return 0;
}

function test_pop()
{
    var a = [0,1,2,3,4];

    var r = a.pop();
    if (r != 4)
        return 1;
    if (!array_eq(a, [0,1,2,3]))
        return 2;

    var r = a.pop();
    if (r != 3)
        return 3;
    if (!array_eq(a, [0,1,2]))
        return 4;

    while (a.length > 0)
        a.pop();
    if (!array_eq(a, []))
        return 5;

    return 0;
}

function test_splice()
{
    // TODO



    return 0;
}

function test_map()
{
    var a = [0,1,2,3,4,5];

    var o = a.map(function (v) { return 2*v + 1; });

    if (!array_eq(o, [1,3,5,7,9,11]))
        return 1;

    return 0;
}

function test_foreach()
{
    // TODO






    return 0;
}

function test()
{
    /*
    var r = test_push();
    if (r != 0)
        return 100 + r;
    */

    var r = test_pop();
    if (r != 0)
        return 200 + r;

    var r = test_splice();
    if (r != 0)
        return 300 + r;

    /*
    var r = test_map();
    if (r != 0)
        return 400 + r;
    */

    /*
    var r = test_foreach();
    if (r != 0)
        return 500 + r;
    */

    return 0;
}

