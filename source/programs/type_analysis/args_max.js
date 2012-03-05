function argsMax()
{
    var m = 0;

    for (var i = 0; i < arguments.length; ++i)
    {
        if (arguments[i] > m)
            m = arguments[i];
    }

    return m;
}

var m = argsMax(1, 2, 3, 4);

typeAssert(m, '["and", "int", ["not", "float"], ["not", "undef"]]');

