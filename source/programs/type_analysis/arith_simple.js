function test1_add3(x)
{
    return x + 3;
}

function test2_add3(x)
{
    return x + 3;
}

function test3_addbar(x)
{
    return x + 'bar';
}

function test4_sub3(x)
{
    return x - 3;
}

function test5_sub3(x)
{
    return x - 3;
}

function test6_mul5(x)
{
    return x * 5;
}

function test7_mul_loop(k, n)
{
    var prod = k;

    for (var i = 0; i < n; ++i)
        prod *= 2;

    return prod;
}

function test8_div3(x)
{
    return x / 3;
}

function test9_divm5(x)
{
    return x / -5;
}

function test10_div7(x)
{
    return x / 7;
}

function test()
{
    var r = test1_add3(4);
    typeAssert(r, '["and", "int", ["val", 7], ["not", "float"], ["not", "string"]]');
    if (r !== 7)
        return 1;

    var r = test2_add3(-4);
    typeAssert(r, '["and", "int", ["val", -1], ["not", "float"], ["not", "string"]]');
    if (r !== -1)
        return 2;

    var r = test3_addbar('foo');
    typeAssert(r, '["and", "string", ["val", "foobar"], ["not", "float"], ["not", "int"]]');
    if (r !== 'foobar')
        return 3;

    var r = test4_sub3(-4);
    typeAssert(r, '["and", "int", ["val", -7], ["not", "float"], ["not", "string"]]');
    if (r !== -7)
        return 4;

    var r = test5_sub3(4);
    typeAssert(r, '["and", "int", ["val", 1], ["not", "float"], ["not", "string"]]');
    if (r !== 1)
        return 5;

    var r = test6_mul5(4);
    typeAssert(r, '["and", "int", ["val", 20], ["not", "float"], ["not", "string"]]');
    if (r !== 20)
        return 6;

    var r = test7_mul_loop(1, 20);
    typeAssert(r, '["and", "int", [">=", 1], ["not", "float"], ["not", "string"]]');
    if (r < 1)
        return 7;

    var r = test8_div3(12);
    typeAssert(r, '["and", "int", ["val", 4], ["not", "float"], ["not", "string"]]');
    if (r !== 4)
        return 8;

    var r = test9_divm5(40);
    typeAssert(r, '["and", "int", ["val", -8], ["not", "float"], ["not", "string"]]');
    if (r !== -8)
        return 9;

    var r = test10_div7(3);
    typeAssert(r, '["and", "float", [">=", 0], ["not", "string"]]');
    if (r < 0)
        return 10;

    return 0;
}

test();

