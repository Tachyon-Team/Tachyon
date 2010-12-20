//=============================================================================

// File: "js.js", Time-stamp: <2010-12-20 16:11:27 feeley>

// Copyright (c) 2010 by Marc Feeley, All Rights Reserved.

//=============================================================================

function main()
{
    var args = command_line();
    var opt_debug = false;
    var opt_ast = false;
    var i = 0;
    var n;

    while (i < args.length)
    {
        if (args[i] == "-debug")
            opt_debug = true;
        else if (args[i] == "-ast")
            opt_ast = true;
        else
            break;
        i++;
    }

    n = args.length - i;

    while (i < args.length)
    {
        var filename = args[i];

        if (n > 1)
        {
            print(filename + ":");
        }
        var port = new File_input_port(filename);
        var s = new Scanner(port);
        var p = new Parser(s, true);
        var prog = p.parse();
        if (prog != null)
        {
            var normalized_prog = ast_normalize(prog, opt_debug);
            if (opt_ast)
                pp(normalized_prog);
        }
        i++;
    }
}

main();

//=============================================================================
