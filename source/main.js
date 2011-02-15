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
    // Print a help listing
    function printHelp()
    {
        print('Available special commands:');
        print('  /load <filename>    load and execute a script');
        print('  /time <command>     time the execution of a command');
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
            'int'
        );

        bridge(config.hostParams.ctxPtr);
    }

    // Execute a special command
    function execSpecial(cmd)
    {
        var spaceIdx = cmd.indexOf(' ');
        if (spaceIdx != -1)
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

            default:
            print('Unknown special command: "' + cmd + '"');
            break;
        }
    }

    // Execute a code string
    function execCode(str)
    {
        // Add an extra semicolon to avoid syntax errors
        str += ';';

        try
        {
            var ir = compileSrcString(str, config.hostParams);

            var bridge = makeBridge(
                ir,
                config.hostParams,
                [],
                'int'
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

