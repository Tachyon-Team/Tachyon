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
    
    print(ir);

    lowerIRFunc(ir);

    print(ir);

    ir.validate();    
    


    /*
    // Parse the source string
    var ast = parse_src_file('parser/tests/test4.js');

    // Translate the AST to IR
    var ir = unitToIR(ast);

    // Copy the resulting function
    ir.copy();

    // Validate the IR
    ir.validate();

    // Perform lowering on the IR
    lowerIRFunc(ir);

    // Validate the IR
    ir.validate();
    */




    /*
    var codeblock = backend.compile(ir, print);
    print(backend.listing(codeblock));
    backend.execute(codeblock);
    */
};

testIR();

