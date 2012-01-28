function bar(n)
{
    var a1 = { b:3 };

    if (n)
        var r = a1;

    typeAssert(r, '["and", "object", "undef"]');

    typeAssert(r.b, '["and", "int", ["val", 3], ["not", "undef"]]');
}

bar(0);
bar(1);

