function test_ctor()
{
    if (String(5) != '5')
        return 1;

    if (String('foo') !== 'foo')
        return 2;

    // TODO: add more tests for constructor

    return 0;
}

function test_charCodeAt()
{
    var s = 'foo';

    if (s.charCodeAt(0) !== 102)
        return 1;
    if (s.charCodeAt(1) !== 111)
        return 2;
    if (s.charCodeAt(2) !== 111)
        return 3;

    return 0;
}

function test_charAt()
{
    var s = 'foo';

    if (s.charAt(0) !== 'f')
        return 1;
    if (s.charAt(1) !== 'o')
        return 2;
    if (s.charAt(2) !== 'o')
        return 3;
    if (s.charAt(3) !== '')
        return 4;

    return 0;
}

function test_indexOf()
{
    if ('foo'.indexOf('f') != 0)
        return 1;
    if ('foo'.indexOf('oo') != 1)
        return 2;
    if ('foo'.indexOf('a') != -1)
        return 3;

    return 0;
}

function test_toLowerCase()
{
    if ('FOO'.toLowerCase() != 'foo')
        return 1;
    if ('FoO'.toLowerCase() != 'foo')
        return 2;
    if ('foo'.toLowerCase() != 'foo')
        return 3;

    return 0;
}

function test_slice()
{
    if ('foo'.slice(0) !== 'foo')
        return 1;
    if ('foo'.slice(1) !== 'oo')
        return 2;
    if ('foobar'.slice(1,4) !== 'oob')
        return 3;

    return 0;
}

function test_substring()
{
    if ('foo'.substring(0) !== 'foo')
        return 1;
    if ('foo'.substring(1) !== 'oo')
        return 2;
    if ('foobar'.substring(1,4) !== 'oob')
        return 3;

    return 0;
}

function test_replace()
{
    if ('foobif'.replace('oo', 'oobar') !== 'foobarbif')
        return 1;

    return 0;
}

//function test_split()

function test()
{
    var r = test_ctor();
    if (r != 0)
        return 100 + r;

    var r = test_charCodeAt();
    if (r != 0)
        return 200 + r;

    var r = test_charAt();
    if (r != 0)
        return 300 + r;

    var r = test_indexOf();
    if (r != 0)
        return 400 + r;

    var r = test_toLowerCase();
    if (r != 0)
        return 500 + r;

    var r = test_slice();
    if (r != 0)
        return 600 + r;

    var r = test_substring();
    if (r != 0)
        return 700 + r;

    /*FIXME: bug in replace
    var r = test_replace();
    if (r != 0)
        return 800 + r;
    */









    return 0;
}

