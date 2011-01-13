/**
@fileOverview
Fibonacci implementation to test the whole compiler.

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

