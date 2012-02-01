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

function test()
{
    if (cmp_lt5(3) !== -1)
        return 1;
    if (cmp_lt5(6) !== 1)
        return 2;

    return 0;
}

test();
