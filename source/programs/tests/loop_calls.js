/**
@fileOverview
Test of function calls before, inside and after a loop.

@copyright
Copyright (c) 2010-2011 Tachyon Javascript Engine, All Rights Reserved
*/

tests.loop_calls = tests.testSuite();

tests.loop_calls.main = function ()
{
    var func = compileFileToJSFunc(
        'programs/loop_calls/loop_calls.js', 
        config.clientParams
    );

    var x = func();

    func.free();

    assert(x === (14338 << 2), "Invalid return value: " + x);
};

