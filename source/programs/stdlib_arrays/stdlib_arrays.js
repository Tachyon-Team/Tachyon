function array_eq(a1, a2)
{
    if (a1.length !== a2.length)
        return false;

    for (var i = 0; i < a1.length; ++i)
        if (a1[i] !== a2[i])
            return false;

    return true;
}

function test_ctor()
{
    if (!array_eq(new Array(3), [undefined, undefined, undefined]))
        return 1;

    if (!array_eq(new Array(0,1,2), [0,1,2]))
        return 2;

    if (!array_eq(Array(0,1,2), [0,1,2]))
        return 3;

    return 0;
}

function test_indexOf()
{
    var a = ['a', 'b', 'c', 'd'];

    if (a.indexOf('a') != 0)
        return 1;
    if (a.indexOf('b') != 1)
        return 2;
    if (a.indexOf('c') != 2)
        return 3;
    if (a.indexOf('d') != 3)
        return 4;
    if (a.indexOf('e') != -1)
        return 5;

    return 0;
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

function test_unshift()
{
    var a = [0,1,2];

    a.unshift(3);
    if (!array_eq(a, [3,0,1,2]))
        return 1;

    a.unshift(4)
    if (!array_eq(a, [4,3,0,1,2]))
        return 2;

    return 0;
}

function test_shift()
{
    var a = [0,1,2,3,4];

    var r = a.shift();
    if (r != 0)
        return 1;
    if (!array_eq(a, [1,2,3,4]))
        return 2;

    var r = a.shift();
    if (r != 1)
        return 3;
    if (!array_eq(a, [2,3,4]))
        return 4;

    while (a.length > 0)
        a.shift();
    if (!array_eq(a, []))
        return 5;

    return 0;
}

function test_slice()
{
    var a = [0,1,2,3];

    if (!array_eq(a.slice(0), [0,1,2,3]))
        return 1;

    if (!array_eq(a.slice(1,3), [1,2]))
        return 2;

    return 0;
}

function test_splice()
{
    /* TODO: Check standard, this matches V8's response, standard is unclear
    var a = [0,1,2,3];
    var b = a.splice(0);
    if (!array_eq(b, [0,1,2,3]))
        return 1;
    if (!array_eq(a, []))
        return 2;
    */

    var a = [0,1,2,3];
    var b = a.splice(1,2);
    if (!array_eq(b, [1,2]))
        return 3;
    if (!array_eq(a, [0,3]))
        return 4;

    var a = [0,1,2,3];
    var b = a.splice(1,2,4,5,6)
    if (!array_eq(b, [1,2]))
        return 5;
    if (!array_eq(a, [0,4,5,6,3]))
        return 6;

    return 0;
}

function test_reverse()
{
    var a = [0,1,2,3,4];

    var b = a.reverse();

    if (!array_eq(b, [4,3,2,1,0]))
        return 1;

    return 0;
}

function numeric_comparefn(x, y)
{
    if (x < y)
        return -1;
    else if (x > y)
        return 1;
    else
        return 0;
}

function test_sort()
{
    var a = [0,-5,3,15,12,-33,7];

    a.sort(numeric_comparefn);

    var b = [-33,-5,0,3,7,12,15];

    if (!array_eq(a, b))
        return 1;

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

function test_forEach()
{
    // TODO






    return 0;
}

function test()
{
    var r = test_ctor();
    if (r != 0)
        return 100 + r;

    var r = test_indexOf();
    if (r != 0)
        return 200 + r;

    var r = test_push();
    if (r != 0)
        return 300 + r;

    var r = test_pop();
    if (r != 0)
        return 400 + r;

    var r = test_unshift();
    if (r != 0)
        return 500 + r;

    var r = test_shift();
    if (r != 0)
        return 600 + r;

    var r = test_slice();
    if (r != 0)
        return 700 + r;

    var r = test_splice();
    if (r != 0)
        return 800 + r;

    var r = test_reverse();
    if (r != 0)
        return 900 + r;

    var r = test_sort();
    if (r != 0)
        return 1000 + r;

    /*
    var r = test_map();
    if (r != 0)
        return 400 + r;
    */

    /*
    var r = test_forEach();
    if (r != 0)
        return 500 + r;
    */

    return 0;
}

