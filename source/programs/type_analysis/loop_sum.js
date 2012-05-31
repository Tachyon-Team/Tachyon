function foo(n)
{
    var sum = 0;

    for (var i = 0; i < n; ++i)
    {
        sum += i;
    }

    typeAssert(sum, '["and", "int", [">=", 0], ["not", "float"], ["not", "string"]]');

    return sum;
}

var r = foo(5);

typeAssert(r, '"int"');

