function test_ctor()
{
    if (typeof Function !== 'function')
        return 1;
    if (!(Function instanceof Function))
        return 2;
    if (typeof test_ctor !== 'function')
        return 3;
    if (!(test_ctor instanceof Function))
        return 4;

    return 0;
}

function test_toString()
{
    var s = test_toString.toString();

    if (typeof s !== 'string')
        return 1;

    if (s.length < 8)
        return 2;

    return 0;
}

function sum()
{
    var sum = 0;

    for (var i = 0; i < arguments.length; ++i)
        sum += arguments[i];

    return sum;
}

function test_apply()
{
    if (sum.apply(null, [1, 2, 3]) !== 6)
        return 1;

    if (sum.apply(null, [1, 2, 3, 4, 5, 6]) !== 21)
        return 2;

    return 0;
}

function test_call()
{
    if (sum.call(null, 1, 2, 3) !== 6)
        return 1;

    if (sum.call(null, 1, 2, 3, 4, 5, 6) !== 21)
        return 2;

    return 0;
}

function test()
{
    var r = test_ctor();
    if (r != 0)
        return 100 + r;

   var r = test_toString();
    if (r != 0)
        return 200 + r;

   var r = test_apply();
    if (r != 0)
        return 300 + r;

   var r = test_call();
    if (r != 0)
        return 400 + r;

    return 0;
}

