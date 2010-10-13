/**
@fileOverview
Fibonacci implementation to test the whole compiler.

@copyright
Copyright (c) 2010 Tachyon Javascript Engine, All Rights Reserved
*/

tests.sumloop = tests.testSuite();

tests.sumloop.main = function ()
{
    var ast = parse_src_file('programs/sumloop.js');
    
    var ir = unitToIR(ast);

    lowerIRFunc(ir);

    //var codeblock = backend.compile(ir);
    //var codeblock = backend.compile(ir, undefined, backend.usedPrimitives(ir));
    var codeblock = backend.compile(ir, print, backend.usedPrimitives(ir));    
    print(backend.listing(codeblock));

    var startTimeMs = new Date().getTime();

    var x = backend.execute(codeblock);

    var endTimeMs = new Date().getTime();
    var timeS = (endTimeMs - startTimeMs) / 1000;
    print('time: ' + timeS + ' s');

    assert(x === (45 << 2), "Invalid return value: " + x);
};

