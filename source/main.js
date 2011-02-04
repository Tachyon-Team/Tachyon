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
    var args = parseCmdLine()

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
        print('  /help           print a help listing');
        print('  /exit           exit the read-eval-print loop');
    }

    // Execute a special command
    function execSpecial(cmd)
    {
        switch (cmd)
        {
            case 'exit':
            return true;

            case 'help':
            printHelp();
            break;

            default:
            print('Unknown special command: "' + cmd + '"');
            break;
        }
    }

    // Execute a code string
    function execCode(str)
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
            // Add an extra semicolon to avoid syntax errors
            cmd += ';';

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

