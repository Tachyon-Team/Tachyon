/**
@fileOverview
Fibonacci implementation to test the whole compiler.

@copyright
Copyright (c) 2010 Tachyon Javascript Engine, All Rights Reserved
*/

tests.fib = tests.testSuite();

tests.fib.main = function ()
{
    var fib = compileFileToJSFunc('programs/fib/fib.js', {tachyonSrc:true});

    var startTimeMs = new Date().getTime();
    var x = fib();
    var endTimeMs = new Date().getTime();

    fib.free();
    var timeS = (endTimeMs - startTimeMs) / 1000;
    print('time: ' + timeS + ' s');

    assert(x === (6765 << 2), "Invalid return value: " + x);
};

