function sumArgs()
{
    var s = 0;

    for (var i = 0; i < arguments.length; ++i)
    {
        // Ensure that the integer type is included
        typeAssert(arguments[i], '"int"');

        s += arguments[i];
    }

    // Ensure that unbounded arguments accesses have the undefined type
    typeAssert(arguments[100], '"undef"');

    return s;
}

var s = sumArgs(1, 2, 3, 4);

typeAssert(s, '"int"');

