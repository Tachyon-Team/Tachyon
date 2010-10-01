/**
@fileOverview
Fibonacci implementation to test the whole compiler.

@copyright
Copyright (c) 2010 Tachyon Javascript Engine, All Rights Reserved
*/

tests.fib = tests.testSuite();

tests.fib.main = function ()
{

    var filename = 'programs/fib.js';
    var port = new File_input_port(filename);
    var p = new Parser(new Scanner(port), true);
    var ast = p.parse();
    var normalized_ast = ast_normalize(ast);
    
    var ir = unitToIR(normalized_ast);

    //var codeblock = backend.compile(ir);
    //var codeblock = backend.compile(ir, undefined, backend.usedPrimitives(ir));
    var codeblock = backend.compile(ir, print, backend.usedPrimitives(ir));
    
    //print(backend.listing(codeblock));
    var x = backend.execute(codeblock);
    assert(x === (6765 << 2), "Invalid return value: " + x);

};
