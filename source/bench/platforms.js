/**
@fileOverview
Implementation of benchmarking platforms.

@copyright
Copyright (c) 2010 Tachyon Javascript Engine, All Rights Reserved
*/

/**
Benchmarking namespace
*/
var bench = bench || {};

/**
@class base class for benchmarking platforms. Represents a virtual machine
to test benchmarks under.
*/
bench.Platform = function ()
{
    // TODO
}
bench.Platform.prototype = {};

/**
Load configuration parameters for this platform
*/
bench.Platform.prototype.loadParams = function (cfg)
{
    // For each config parameter
    for (param in cfg)
    {
        // Try to parse the parameter
        this.setParam(param, cfg[param]);
    }
}

/**
Set the generic parameters for this platform
*/
bench.Platform.prototype.setParam = function (param, val)
{
    switch (param)
    {
        // If this is the id parameter, ignore it
        case 'id':
        break;

        default:
        {
            error(
                'unknown config param: "' + param + '" (' +
                val + ') for ' + this.name + ' platform'
            );
        }   
    }
}

/**
Start a new (child) VM process, passing it command-line arguments
*/
bench.Platform.prototype.callVM = function (args)
{
    var argStr = this.shellCmd + ' --';

    for (var argName in args)
        argStr += ' -' + argName + ' "' + args[argName] + '"';

    print('Calling: ' + argStr);

    shellCommand(argStr);
}

/**
@class Represents the V8 platform
@extends bench.Platform
*/
bench.Platform.V8 = function ()
{
    // TODO
}
bench.Platform.V8.prototype = new bench.Platform();

/**
Identifier for this platform, name of the constructor
*/
bench.Platform.V8.prototype.id = 'V8';

/**
Name of the platform
*/
bench.Platform.V8.prototype.name = 'Google V8';

/**
Shell command used to start this platform
*/
bench.Platform.V8.prototype.shellCmd = 'd8';

/**
Set the parameters for this environment
*/
bench.Platform.V8.prototype.setParam = function (param, val)
{
    switch (param)
    {
        //
        // TODO: parse dimension-specific params here
        //

        // If the param is unknown, try using the default handler
        default:
        {
            var res = bench.Platform.prototype.setParam.apply(
                this, [param, val]
            );
        }
    }
}

