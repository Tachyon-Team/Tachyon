function foo(n)
{
    var arr = [];

    for (var i = 0; i < n; ++i)
        arr[i] = i;

    for (var i = 0; i < n; ++i)
        arr[i] = 2 * i;

    var sum = 0;

    for (var i = 0; i < n; ++i)    
        sum += arr[i];

    return sum;
}

