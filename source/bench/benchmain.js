/**
@fileOverview
Entry point for the benchmarking code.

@copyright
Copyright (c) 2010 Tachyon Javascript Engine, All Rights Reserved
*/

/**
Parse command-line arguments
*/
function parseArgs()
{
    var args = command_line();

    // Map for named arguments
    var options = {};

    // List for trailing unnamed arguments
    var files = [];

    var argIdx = 0;

    // For each named argument
    for (; argIdx < args.length; argIdx += 2)
    {
        var arg = args[argIdx];

        // If this is not an option argument, stop
        if (arg.charAt(0) != '-')
            break;

        // Ensure that the option value is present
        if (argIdx >= args.length - 1)
            error('missing command-line option value for "' + arg + '"');

        // Get the option name
        var optName = arg.slice(1);

        // Store the option value
        options[optName] = args[argIdx + 1];
    }

    // For each remaining argument
    for (; argIdx < args.length; ++argIdx)
    {
        // Add it to the file arguments
        files.push(args[argIdx]);
    }

    // Return the parsed arguments
    return {
        "cmd"       : args[0],
        "options"   : options,
        "files"     : files
    };
}

/**
Entry point function for the benchmarking code
*/
function main()
{
    //
    // TODO: mode to generate report file?
    //

    // Parse the command-line arguments
    var args = parseArgs();

    // If a config file argument is supplied
    if (args.options['cfgFile'])
    {
        bench.loadConfig(args.options['cfgFile']);

        bench.runBenchs();
    }

    // Otherwise, if an output file argument is supplied
    else if (args.options['outFile'])
    {
        bench.loadOutput(args.options['outFile']);

        bench.runBench(
            Number(args.options['platIdx']),
            Number(args.options['benchIdx']),
            Boolean(args.options['testRun'])
        );

        bench.storeOutput(args.options['outFile']);
    }

    // Otherwise, if a report should be generated
    else if (args.options['genReport'])
    {
        bench.loadOutput(args.options['genReport']);

        bench.genReport(args.options['genReport']);
    }
    
    // Otherwise, arguments are missing
    else
    {
        print('expected config file argument');
        return;
    }
}

try 
{
    // Initialize Tachyon
    if (this.initialize)
        initialize();

    main();

    // Uninitialize Tachyon
    if (this.uninitialize)
        uninitialize();
}
catch (e)
{
    if (e.stack)
        print(e.stack);
    else
        print(e);
}

