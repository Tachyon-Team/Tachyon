/**
@fileOverview
Code specific to the JavaScript VM's entry point, handling of
command-line arguments, etc.

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010-2011 Maxime Chevalier-Boisvert, All Rights Reserved
*/

/**
Entry point for the Tachyon VM.
*/
function main()
{
    // Parse the command-line arguments
    var args = parseCmdLine();

    // If bootstrap compilation is requested
    if (args.options['bootstrap'])
    {
        // Initialize Tachyon in bootstrap mode
        initialize(true);

        // ???
        // Profit        
    }

    // Otherwise, assume we are running in shell mode
    else
    {
        // Initialize Tachyon in minimal mode
        initialize(false);

        // Call the Tachyon read-eval-print loop
        tachyonRepl();
    }

    // Uninitialize Tachyon
    uninitialize();
}

/**
Tachyon read-eval-print loop
*/
function tachyonRepl()
{
    /* TODO

        /hir <command>
        /lir <command>
        /asm <command>  produce assembly listing for command

        Compile and log
    */

    // Print a help listing
    function printHelp()
    {
        print('Available special commands:');
        print('  /load <filename>    load and execute a script');
        print('  /time <command>     time the execution of a command');
        print('  /hir  <command>     view HIR produced for a command/file');
        print('  /lir  <command>     view LIR produced for a command/file');
        print('  /asm  <command>     view ASM produced for a command/file');
        print('  /reg  <command>     view register allocation for a command/file');
        print('  /help               print a help listing');
        print('  /exit               exit the read-eval-print loop');
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

            var bridge = makeBridge(
                ir,
                config.hostParams,
                [],
                new CIntAsBox()
            );

            bridge(config.hostParams.ctxPtr);
        }

        catch (e)
        {
            if (e.stack)
                print(e.stack);
            else
                print(e);
        }
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

