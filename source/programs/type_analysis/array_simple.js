var a = [1, 2, 3, 4];

function foo()
{
    var sum = 0;

    for (var i = 0; i < a.length; ++i)
        sum += a[i];

    return sum;
}

foo();

