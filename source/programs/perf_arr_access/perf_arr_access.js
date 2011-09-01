function foo(n)
{
    var arr = new Array(n);

    for (var i = 0; i < arr.length; ++i)
        arr[i] = i+1;

    for (var i = 0; i < n; ++i)
    {
        for (var j = 0; j < arr.length; ++j)
            arr[j] += 1;
    }

    var sum = 0;
    for (var i = 0; i < arr.length; ++i)
        sum += arr[i];

    //print(sum);

    return sum;
}

function test()
{
    var r = foo(50);

    if (r !== 3775)
        return 1;

    return 0;
}

