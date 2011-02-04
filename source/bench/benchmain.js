/**
@fileOverview
Entry point for the benchmarking code.

@copyright
Copyright (c) 2010 Tachyon Javascript Engine, All Rights Reserved
*/

/**
Entry point function for the benchmarking code
*/
function main()
{
    // Parse the command-line arguments
    var args = parseCmdLine();

    /*
    for (argName in args.options)
    {
        print('"' + argName + '" = "' + args.options[argName] + '"');
    }
    */

    // If a config file argument is supplied
    if (args.options['cfgFile'])
    {
        bench.loadConfig(args.options['cfgFile']);

        bench.runBenchs();
    }

    // Otherwise, if a data file argument is supplied
    else if (args.options['dataFile'])
    {
        bench.loadOutput(args.options['dataFile']);

        bench.loadConfig(bench.cfgFile);

        // If a benchmark should be run
        if (args.options['platIdx'])
        {
            bench.runBench(
                Number(args.options['platIdx']),
                Number(args.options['benchIdx']),
                Boolean(args.options['testRun'])
            );

            bench.storeOutput(args.options['dataFile']);
        }
    }
    
    // Otherwise, arguments are missing
    else
    {
        print('expected config file or data file argument');
        return;
    }

    // If a report file argument is supplied
    if (args.options['genReport'])
    {
        bench.loadOutput(bench.dataFile);

        // Generate a report file
        bench.genReport(args.options['genReport']);
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

