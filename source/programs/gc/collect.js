function theLenFunc(str)
{
    return str.length;
}

function test()
{
    var liveObj = { v:3 };

    theLenFunc('foo');

    // Trigger a collection
    gcCollect();

    theLenFunc('foobar');

    if (boxIsObj(liveObj) === false)
        return 1;

    // Test that our live object hasn't been corrupted
    if (liveObj.v !== 3)
        return 2;

    // Test that the string table still works properly
    if ('foo' + 'bar' !== 'foobar')
        return 3;

    var theNewObj = { x:1 };

    // Test that new objects can still be created and manipulated
    if (theNewObj.x !== 1)
        return 4;

    // Test that linked strings are preserved
    var len = theLenFunc('foobarbif');
    if (len !== 9)
        return 5;

    return 0;
}

