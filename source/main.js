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
    var filename = 'parser/tests/testfib.js';
    var port = new File_input_port(filename);
    var p = new Parser(new Scanner(port), true);
    var ast = p.parse();
    var normalized_ast = ast_normalize(ast);
    
    var ir = unitToIR(normalized_ast);

    //pp(normalized_ast); // pretty-print AST
    //print('\n');

    var codeblock = backend.compile(ir, print);
    print(backend.listing(codeblock));
    backend.execute(codeblock);
};

testIR();

