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
    var args = command_line();

    if (args.length == 0)
    {
        print('config file argument required');
        return
    }

    var configFile = args[0];

    bench.loadConfig(configFile);



    // TODO: perform benchmarking!

    // TODO: recognize master vs "slave" mode?

}

main();

