function foo(a)
{
    for (var i = 0; i < 5; ++i)
    {
        for (var j = 0; j < 10; ++j)
        {
            a += (i * j) + 1;
        }
    }

    return a;
}

// Returns 503
return foo(3);
