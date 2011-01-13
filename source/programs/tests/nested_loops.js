/**
@fileOverview
Nested loops unit test.

@copyright
Copyright (c) 2010-2011 Tachyon Javascript Engine, All Rights Reserved
*/

tests.nested_loops = tests.testSuite();

tests.nested_loops.main = function ()
{
    var func = compileFileToJSFunc(
        'programs/nested_loops/nested_loops.js',
        config.clientParams
    );

    var x = func();

    func.free();

    assert(x === (503 << 2), "Invalid return value: " + x);
};

