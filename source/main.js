/**
@fileOverview
Code specific to the JavaScript VM's entry point, handling of
command-line arguments, etc.

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010 Maxime Chevalier-Boisvert, All Rights Reserved
*/

function testIR()
{
    var ast = parse_src_file('parser/tests/test4.js');

    var ir = unitToIR(ast, true);

    //pp(ast); // pretty-print AST
    //print('\n');
    
    /*
    print(ir);

    lowerIRFunc(ir);

    print(ir);
    */

    /*
    var codeblock = backend.compile(ir, print);
    print(backend.listing(codeblock));
    backend.execute(codeblock);
    */
};

testIR();

