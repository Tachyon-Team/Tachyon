/**
@fileOverview
Fibonacci implementation to test the whole compiler.

@copyright
Copyright (c) 2010 Tachyon Javascript Engine, All Rights Reserved
*/

tests.loop_sum = tests.testSuite();

tests.loop_sum.main = function ()
{
    //config.clientParams.print = print;

    var loop_sum = compileFileToJSFunc('programs/loop_sum/loop_sum.js', config.clientParams);

    var startTimeMs = new Date().getTime();
    var x = loop_sum();
    var endTimeMs = new Date().getTime();

    loop_sum.free();
    var timeS = (endTimeMs - startTimeMs) / 1000;
    print('time: ' + timeS + ' s');

    assert(x === (45 << 2), "Invalid return value: " + x);
};

