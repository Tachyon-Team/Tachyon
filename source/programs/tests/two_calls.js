/**
@fileOverview
Fibonacci implementation to test the whole compiler.

@copyright
Copyright (c) 2010-2011 Tachyon Javascript Engine, All Rights Reserved
*/

tests.two_calls = tests.testSuite();

tests.two_calls.main = function ()
{
    var func = compileFileToJSFunc(
        'programs/two_calls/two_calls.js',
        config.clientParams
    );

    var x = func();

    func.free();

    assert(x === (39 << 2), "Invalid return value: " + x);
};

