function foo(n)
{
    var obj = {};

    var sum = 0;

    for (var i = 0; i < n; ++i)
    {
        obj[i] = i;
    }

    for (var i = 0; i < n; ++i)
    {
        sum += obj[i];
    }
    
    for (var i = 0; i < n; ++i)
    {
        obj[i] = 2 * i;
    }
    
    for (var i = 0; i < n; ++i)
    {
        sum += obj[i];
    }

    return sum;
}

