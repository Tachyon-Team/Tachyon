for (var i = 0; i <= 10; ++i)
{
    var closFn = function () {};

    if (i % 2 === 0)
        closEvn = closFn;
    else
        closOdd = closFn;
}

closEvn.prototype.x = 4;

typeAssert(closEvn.prototype.x, '"int"');
typeAssert(closOdd.prototype.x, '"undef"');

