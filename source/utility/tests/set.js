/**
@fileOverview

Unit tests for sets

@copyright
Copyright (c) 2010 Tachyon Javascript Engine, All Rights Reserved
*/

tests.utility = tests.utility || tests.testSuite();

tests.utility.set = tests.testSuite();

tests.utility.set.hashSet = function ()
{
    var s1 = new HashSet();

    assert(s1.length === 0);

    s1.add(1);
    assert(s1.has(1));
    assert(s1.length === 1);

    assert(!s1.has(2));
    s1.add(2);
    assert(s1.has(2));
    assert(s1.length === 2);

    s1.rem(1);
    assert(!s1.has(1));
    assert(s1.length === 1);

    var s2 = s1.copy();
    assert(s1.length === s2.length);
    assert(s2.diff(s1).length === 0);

    var s3 = new HashSet().addArray([1,2,3]); 
    assert(s3.length === 3);
    assert(s3.has(1));
    assert(s3.has(2));
    assert(s3.has(3));

    var s4 = s3.copy().diff(s1);
    assert(s4.length === 2);
    assert(s4.has(1));
    assert(s4.has(3));
    assert(!s4.has(2));

    var s5 = s3.copy().remArray([1,2,3]);
    assert(s5.length === 0);
    assert(!s5.has(1));
    assert(!s5.has(2));
    assert(!s5.has(3));

    var s6 = s3.copy().union(new HashSet().addArray([4,5,6]));
    assert(s6.length === 6);
    assert(s6.has(1));
    assert(s6.has(2));
    assert(s6.has(3));
    assert(s6.has(4));
    assert(s6.has(5));
    assert(s6.has(6));

    var s7 = s3.copy().intr(new HashSet().addArray([2,3,4]));
    assert(s7.length === 2);
    assert(!s7.has(1));
    assert(s7.has(2));
    assert(s7.has(3));
    assert(!s7.has(4));

    assert(s3.equal(s3.copy()));

    assert(s3.copy().clear().length === 0);

};
