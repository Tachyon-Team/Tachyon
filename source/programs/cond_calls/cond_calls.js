function bar(v)
{
    return v + 1;
}

function bif(v)
{
    return v + 2;
}

function foo(a)
{
    printInt(222);

    var b = 3;

    if (a === 1)
    {
        // b += 4
        b += bar(b);
    }
    else
    {
        // b += 5
        b += bif(b);
    }

    b += 1;

    return a + b;
}

function fee()
{
    printInt(111);

    // 9 + 11 === 20
    return foo(1) + foo(2);
}

