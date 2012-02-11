function foo()
{
    var a = 3;
    var b;
    var c;

    function bar()
    {
        typeAssert(a, '"int"');
        typeAssert(c, '"undef"');

        b = 'foo';
    }

    bar();

    typeAssert(a, '"int"');
    typeAssert(b, '"string"');
    typeAssert(c, '"undef"');
}

foo();

