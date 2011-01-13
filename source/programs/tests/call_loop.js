/**
@fileOverview
Fibonacci implementation to test the whole compiler.

@copyright
Copyright (c) 2010-2011 Tachyon Javascript Engine, All Rights Reserved
*/

tests.call_loop = tests.testSuite();

tests.call_loop.main = function ()
{
    var func = compileFileToJSFunc(
        'programs/call_loop/call_loop.js', 
        config.clientParams
    );

    var x = func();

    func.free();

    assert(x === (15 << 2), "Invalid return value: " + x);
};

