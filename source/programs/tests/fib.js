/**
@fileOverview
Fibonacci implementation to test the whole compiler.

@copyright
Copyright (c) 2010 Tachyon Javascript Engine, All Rights Reserved
*/

tests.fib = tests.testSuite();

tests.fib.main = function ()
{
    var ast = parse_src_file('programs/fib.js');
    
    var ir = unitToIR(ast, true);

    lowerIRFunc(ir);

    //var codeblock = backend.compile(ir);
    //var codeblock = backend.compile(ir, undefined, backend.usedPrimitives(ir));
    var codeblock = backend.compile(ir, print, backend.usedPrimitives(ir));    
    //print(backend.listing(codeblock));

    var startTimeMs = new Date().getTime();

    var x = backend.execute(codeblock);

    var endTimeMs = new Date().getTime();
    var timeS = (endTimeMs - startTimeMs) / 1000;
    print('time: ' + timeS + ' s');

    assert(x === (6765 << 2), "Invalid return value: " + x);
};

