/**
@fileOverview
Test of two loops, one after the other, each performing function calls.

@copyright
Copyright (c) 2010-2011 Tachyon Javascript Engine, All Rights Reserved
*/

tests.loop_loop = tests.testSuite();

tests.loop_loop.main = function ()
{
    var func = compileFileToJSFunc(
        'programs/loop_loop/loop_loop.js',
        config.clientParams
    );

    var x = func();

    func.free();

    assert(x === (60 << 2), "Invalid return value: " + x);
};

