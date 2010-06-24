//=============================================================================

// File: "js2scm.js", Time-stamp: <2010-06-23 21:50:57 feeley>

// Copyright (c) 2010 by Marc Feeley, All Rights Reserved.

//=============================================================================

function main()
{
    var args = command_line();
    var statements = [];
    var prog = null;
    var opt_ast = false;
    var opt_noscm = false;
    var i = 0;

    while (i < args.length)
    {
        if (args[i] == "-ast")
            opt_ast = true;
        else if (args[i] == "-noscm")
            opt_noscm = true;
        else
            break;
        i++;
    }

    while (i < args.length)
    {
        var filename = args[i];
        var port = new File_input_port(filename);
        var s = new Scanner(port);
        var p = new Parser(s, true);
        prog = p.parse();
        statements.push(prog.block.statements);
        i++;
    }

    if (prog != null)
    {
        prog = new Program(prog.loc,
                           null,
                           null,
                           null,
                           new BlockStatement(prog.loc,
                                              Array.prototype.concat.apply([], statements)));

        var normalized_prog = ast_normalize(prog);

        if (opt_ast)
            pp(normalized_prog);

        if (!opt_noscm)
            compile_to_scm(normalized_prog);
    }
}

main();

//=============================================================================
