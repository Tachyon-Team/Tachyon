/**
@fileOverview
Code specific to the JavaScript VM's entry point, handling of
command-line arguments, etc.

@author
Maxime Chevalier-Boisvert
Marc Feeley

@copyright
Copyright (c) 2010-2011 Maxime Chevalier-Boisvert, All Rights Reserved
Copyright (c) 2011 Marc Feeley, All Rights Reserved
*/

/**
Entry point for the Tachyon VM.
*/
function main()
{
    // If we are running in bootstrap mode
    if (RUNNING_IN_TACHYON)
    {
        // Initialize Tachyon in minimal mode
        initialize(false, boolToBox(PTR_NUM_BYTES === pint(8)));

        tachyonRepl();

        // Uninitialize Tachyon
        uninitialize();

        return;
    }

    // Parse the command-line arguments
    var args = parseCmdLine();

    // If bootstrap compilation is requested
    if (args.options['bootstrap'])
    {
        // Initialize Tachyon in bootstrap mode
        initialize(true, args.options['x86_64']);

        // ???
        // Profit        
    }

    // If gc code generation is requested
    else if (args.options['gc'])
    {
        // Initialize the Tachyon configuration
        initConfig();

        // Generate the GC code
        genGCCode(config.hostParams);
    }

    // If there are no filenames on the command line, start shell mode
    else if (args.files.length === 0)
    {
        // Initialize Tachyon in minimal mode
        initialize(false, args.options['x86_64']);

        // Call the Tachyon read-eval-print loop
        tachyonRepl();
    }
    else
    {
        // Initialize Tachyon in minimal mode
        initialize(false, args.options["x86_64"]);

        config.hostParams.printAST = args.options["ast"];
        config.hostParams.printHIR = args.options["hir"];
        config.hostParams.printLIR = args.options["lir"];
        config.hostParams.printASM = args.options["asm"];

        for (var i = 0; i < args.files.length; i++)
        {
            if (args.options["time"])
            {
                print("Executing " + args.files[i]);
            }

            var startTimeMs = (new Date()).getTime();

            var ir = compileSrcFile(args.files[i], config.hostParams);

            var midTimeMs = (new Date()).getTime();

            var bridge = makeBridge(
                ir,
                config.hostParams,
                [],
                new CIntAsBox()
            );

            bridge(config.hostParams.ctxPtr);

            var endTimeMs = (new Date()).getTime();
            var compTimeMs = midTimeMs - startTimeMs;
            var execTimeMs = endTimeMs - midTimeMs;

            if (args.options["time"])
            {
                print("  compilation time: " + compTimeMs + " ms");
                print("  execution time:   " + execTimeMs + " ms");
            }
        }
    }

    // Uninitialize Tachyon
    uninitialize();
}

/**
Tachyon read-eval-print loop
*/
function tachyonRepl()
{
    // Print a help listing
    function printHelp()
    {
        print('Available special commands:');
        print('  /load <filename>       load and execute a script');
        print('  /time <command>        time the compilation/execution of a command');
        print('  /time_comp <command>   time the compilation of a command');
        print('  /time_exec <command>   time the execution of a command');
        print('  /ast  <command>        view AST produced for a command/file');
        print('  /hir  <command>        view HIR produced for a command/file');
        print('  /lir  <command>        view LIR produced for a command/file');
        print('  /asm  <command>        view ASM produced for a command/file');
        print('  /reg  <command>        view register allocation for a command/file');
        print('  /help                  print a help listing');
        print('  /exit                  exit the read-eval-print loop');
    }

    // Load and execute a script
    function loadScript(fileName)
    {
        print('Loading script: "' + fileName + '"');

        var ir = compileSrcFile(fileName, config.hostParams);

        var bridge = makeBridge(
            ir,
            config.hostParams,
            [],
            new CIntAsBox()
        );

        bridge(config.hostParams.ctxPtr);
    }

    // Test if a string could be a source file name
    function isSrcFile(str)
    {
        return str.indexOf('.js') === (str.length - 3);
    }

    // Execute a special command
    function execSpecial(cmd)
    {
        var spaceIdx = cmd.indexOf(' ');
        if (spaceIdx !== -1)
        {
            var args = cmd.slice(spaceIdx + 1);
            var cmd = cmd.slice(0, spaceIdx);
        }
        else
        {
            var args = '';
        }

        switch (cmd)
        {
            case 'exit':
            case 'quit':
            return true;

            case 'help':
            printHelp();
            break;

            case 'load':
            loadScript(args);
            break;

            case 'time':
            var startTimeMs = (new Date()).getTime();
            execCode(args);
            var endTimeMs = (new Date()).getTime();
            var time = (endTimeMs - startTimeMs) / 1000;
            print('time: ' + time + 's');
            break;

            case 'time_comp':
            var startTimeMs = (new Date()).getTime();
            if (isSrcFile(args))
                compFile(args)
            else
                compString(args);
            var endTimeMs = (new Date()).getTime();
            var time = (endTimeMs - startTimeMs) / 1000;
            print('time: ' + time + 's');
            break;

            case 'time_exec':
            if (isSrcFile(args))
                var ir = compFile(args)
            else
                var ir = compString(args);
            var startTimeMs = (new Date()).getTime();
            execIR(ir);
            var endTimeMs = (new Date()).getTime();
            var time = (endTimeMs - startTimeMs) / 1000;
            print('time: ' + time + 's');
            break;

            case 'ast':
            config.hostParams.printAST = true;
            if (isSrcFile(args))
                compFile(args)
            else
                compString(args);
            config.hostParams.printAST = false;
            break;

            case 'hir':
            config.hostParams.printHIR = true;
            if (isSrcFile(args))
                compFile(args)
            else
                compString(args);
            config.hostParams.printHIR = false;
            break;

            case 'lir':
            config.hostParams.printLIR = true;
            if (isSrcFile(args))
                compFile(args)
            else
                compString(args);
            config.hostParams.printLIR = false;
            break;

            case 'asm':
            config.hostParams.printASM = true;
            if (isSrcFile(args))
                compFile(args)
            else
                compString(args);
            config.hostParams.printASM = false;
            break;

            case 'reg':
            config.hostParams.printRegAlloc = true;
            if (isSrcFile(args))
                compFile(args)
            else
                compString(args);
            config.hostParams.printRegAlloc = false;
            break;

            default:
            print('Unknown special command: "' + cmd + '"');
            break;
        }
    }

    // Execute a code string
    function execCode(str)
    {
        try
        {
            var ir = compString(str);

            execIR(ir);
        }

        catch (e)
        {
            if (e.stack)
                print(e.stack);
            else
                print(e);
        }
    }

    // Execute a compiled IR function
    function execIR(ir)
    {
        var bridge = makeBridge(
            ir,
            config.hostParams,
            [],
            new CIntAsBox()
        );

        bridge(config.hostParams.ctxPtr);
    }

    // Compile a code string
    function compString(str)
    {
        // Add an extra semicolon to avoid syntax errors
        str += ';';

        try
        {
            var ir = compileSrcString(str, config.hostParams);
        }

        catch (e)
        {
            if (e.stack)
                print(e.stack);
            else
                print(e);
        }

        return ir;
    }

    // Compile a source file
    function compFile(str)
    {
        try
        {
            var ir = compileSrcFile(str, config.hostParams);
        }

        catch (e)
        {
            if (e.stack)
                print(e.stack);
            else
                print(e);
        }

        return ir;
    }

    print('');
    print('Entering read-eval-print loop.');
    print('Type commands below and press enter to execute them.');
    print('For a listing of special commands, type /help');
    print('To exit, type /exit');

    for (;;)
    {
        var cmd = readConsole('\nt> ');
        if (cmd === undefined || cmd === null) return;

        // Remove extra whitespaces from the command
        cmd = stripStr(cmd);

        // If this is a special command    
        if (cmd.charAt(0) === '/')
        {
            var exit = execSpecial(cmd.slice(1));
            if (exit === true)
                return;
        }
        else
        {
            // Execute the code string
            execCode(cmd);
        }
    }
}

try
{
    main();
}

catch (e)
{
    if (e.stack)
        print(e.stack);
    else
        print(e);
}
