function foo(n)
{
    var sum = 0;

    while (n > 0)
    {
        n--;

        for (var i = 0; i < 10; ++i)
        {
            sum += i;
        }
    }

    return sum;
}

