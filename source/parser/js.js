//=============================================================================

// File: "js.js", Time-stamp: <2010-06-08 16:28:03 feeley>

// Copyright (c) 2010 by Marc Feeley, All Rights Reserved.

//=============================================================================

function main()
{
    var args = command_line();

    if (args.length == 1)
    {
        var filename = args[0];
        var port = new File_input_port(filename);
        var s = new Scanner(port);
        var p = new Parser(s);
        var ast = p.parse();
        pp(ast); // pretty-print AST
    }
}

main();

//=============================================================================
