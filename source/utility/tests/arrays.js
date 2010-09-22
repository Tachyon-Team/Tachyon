/**
@fileOverview

Unit tests for arrays.

@copyright
Copyright (c) 2010 Tachyon Javascript Engine, All Rights Reserved
*/

tests.utility = tests.utility || tests.testSuite();

tests.utility.arrays = tests.testSuite();

tests.utility.arrays.arrayRange = function ()
{
    var a = arrayRange(3);

    assert(a.length === 3);
    assert(a[0] === 0);
    assert(a[1] === 1);
    assert(a[2] === 2);

    b = arrayRange(1,3);
    assert(b.length === 2);
    assert(b[0] === 1);
    assert(b[1] === 2);


    c = arrayRange(1,5,2);
    assert(c.length === 2);
    assert(c[0] === 1);
    assert(c[1] === 3);
};
