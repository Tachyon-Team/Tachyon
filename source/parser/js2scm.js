//=============================================================================

// File: "js2scm.js", Time-stamp: <2010-12-31 11:27:16 feeley>

// Copyright (c) 2010 by Marc Feeley, All Rights Reserved.

//=============================================================================

function main()
{
    var args = command_line();
    var statements = [];
    var prog = null;
    var opt_debug = false;
    var opt_ast = false;
    var opt_pp = false;
    var opt_noscm = false;
    var i = 0;

    while (i < args.length)
    {
        if (args[i] === "-debug")
            opt_debug = true;
        else if (args[i] === "-ast")
            opt_ast = true;
        else if (args[i] === "-pp")
            opt_pp = true;
        else if (args[i] === "-noscm")
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

    if (prog !== null)
    {
        prog = new Program(prog.loc,
                           new BlockStatement(prog.loc,
                                              Array.prototype.concat.apply([], statements)));

        var normalized_prog = ast_normalize(prog, opt_debug);

        if (opt_ast)
            pp(normalized_prog);

        if (opt_pp)
            js_pp(normalized_prog);

        if (!opt_noscm)
            compile_to_scm(normalized_prog);
    }
}

main();

//=============================================================================
