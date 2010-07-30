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
    var filename = 'parser/tests/test4.js';
    var port = new File_input_port(filename);
    var p = new Parser(new Scanner(port), true);
    var ast = p.parse();
    var normalized_ast = ast_normalize(ast);

    pp(normalized_ast); // pretty-print AST
    print('\n');

    ir = unitToIR(normalized_ast);

    print(ir);

    /*
    var cfg = ir.childFuncs[0].virginIR;
    var order = orderBlocks(cfg);
    numberInstrs(cfg, order);
    liveIntervals(cfg, order);
    */



}

//testIR();

