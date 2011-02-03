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
    print('');
    print('Entering read-eval-print loop.');
    print('Type commands below and enter EOF to execute them.');

    for (;;)
    {
        var cmd = readConsole('\nt> ');
    
        var ir = compileSrcString(cmd, config.hostParams);

        var bridge = makeBridge(
            ir,
            config.hostParams,
            [],
            'int'
        );

        bridge(config.hostParams.ctxPtr);
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

