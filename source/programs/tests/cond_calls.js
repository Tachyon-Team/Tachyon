/**
@fileOverview
This test is meant to ensure that values are correctly merged after 
conditionals and that local variable values are properly preserved across
calls.

@copyright
Copyright (c) 2010-2011 Tachyon Javascript Engine, All Rights Reserved
*/

tests.cond_calls = tests.testSuite();

tests.cond_calls.main = function ()
{
    var func = compileFileToJSFunc(
        'programs/cond_calls/cond_calls.js',
        config.clientParams
    );

    var x = func();

    func.free();

    assert(x === (20 << 2), "Invalid return value: " + x);
};

