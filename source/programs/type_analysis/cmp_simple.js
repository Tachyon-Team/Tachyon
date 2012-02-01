function cmp_lt5(n)
{
    typeAssert(n, '"int"');

    if (n < 5)
    {
        typeAssert(n, '["and", "int", ["<=", 4]]');
        return -1;
    }
    else
    {
        typeAssert(n, '["and", "int", [">=", 5]]');
        return 1;
    }
}

function cmp_ns3(n)
{
    typeAssert(n, '"int"');

    if (n !== 3)
        return true
    else
        return false;
}

function cmp_nsm1(n)
{
    typeAssert(n, '"int"');

    if (n !== -1)
        return true
    else
        return false;
}

function cmp_nsm1r(n)
{
    typeAssert(n, '"int"');

    if (-1 !== n)
        return true
    else
        return false;
}

function test()
{
    if (cmp_lt5(3) !== -1)
        return 1;
    if (cmp_lt5(6) !== 1)
        return 2;

    var r = cmp_ns3(3);
    typeAssert(r, '["and", "false", ["not", "true"]]');
    if (r !== false)
        return 3;

    var r = cmp_nsm1(1);
    typeAssert(r, '"true"');
    if (r !== true)
        return 4;

    var r = cmp_nsm1(-1);
    typeAssert(r, '"false"');
    if (r !== false)
        return 5;

    var r = cmp_nsm1r(-1);
    typeAssert(r, '"false"');
    if (r !== false)
        return 7;

    var r = cmp_nsm1r(1);
    typeAssert(r, '"true"');
    if (r !== true)
        return 6;

    return 0;
}

test();
