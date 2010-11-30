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

    var argIdx = 1;

    // For each named argument
    for (; argIdx < args.length; ++argIdx)
    {
        var arg = args[argIdx];

        // If this is not an option argument, stop
        if (!(arg.chatAt(0) == '-'))
            return;

        // Ensure that the option value is present
        if (argIdx >= args.length - 1)
            error('missing command-line option value for "' + arg + '"');

        // Get the option name
        var optName = arg.slice(1);


        // TODO


    }



    // TODO




    // Return the parsed arguments
    return {
        "args"  :args,
        "files" :files
    }
}

/**
Entry point function for the benchmarking code
*/
function main()
{
    var args = command_line();

    if (args.length == 0)
    {
        print('config file argument required');
        return
    }

    var configFile = args[0];

    bench.loadConfig(configFile);


    bench.runBenchs();

    // TODO: perform benchmarking!

    // TODO: recognize master vs "slave" mode?

}

main();

