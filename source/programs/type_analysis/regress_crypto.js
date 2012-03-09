function bnp(b) 
{
    var k = undefined;

    if (b == 16)        k = 4;
    else if (b == 16)   k = 3;

    return k;
}

var r = bnp(null);

typeAssert(r, '"undef"');

