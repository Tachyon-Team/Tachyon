//=============================================================================

// File: "js.js", Time-stamp: <2010-06-09 08:50:26 feeley>

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
        var p = new Parser(s, true);
        var ast = p.parse();
        pp(ast); // pretty-print AST
    }
}

main();

//=============================================================================
