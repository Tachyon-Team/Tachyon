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
};
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
};

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

        // Parameter to set a custom name for this platform
        case 'name':
        {
            this.name = name;
        }
        break;

        default:
        {
            error(
                'unknown config param: "' + param + '" (' +
                val + ') for ' + this.name + ' platform'
            );
        }   
    }
};

/**
Start a new (child) VM process, passing it command-line arguments
*/
bench.Platform.prototype.callVM = function (args)
{
    var argStr = this.shellCmd;

    for (var argName in args)
        argStr += ' -' + argName + ' "' + args[argName] + '"';

    //print('Calling: ' + argStr);

    var output = shellCommand(argStr);

    //print(output);

    return output;
};

/**
Load a source file dynamically
*/
bench.Platform.prototype.loadSrc = function (srcFile)
{
    //print('loading src: "' + srcFile + '"');

    // FIXME: TEMPORARY HACK
    // Until we have global function calling in compiled code in Tachyon
    var tokens = srcFile.split('.js');
    var file = tokens[0];
    srcFile = file + '_v8.js';

    load(srcFile);
};

/**
Call a global function by name
*/
bench.Platform.prototype.callFunc = function (funcName, args)
{
    var globalObj = getGlobalObj();

    var func = globalObj[funcName];

    func.apply(globalObj, args);
};

/**
@class Represents the V8 platform
@extends bench.Platform
*/
bench.Platform.V8 = function ()
{
};
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
bench.Platform.V8.prototype.shellCmd = './bench/d8.sh';

/**
Set the parameters for this platform
*/
bench.Platform.V8.prototype.setParam = function (param, val)
{
    switch (param)
    {
        //
        // TODO: parse platform-specific params here
        //

        // If the param is unknown, try using the default handler
        default:
        {
            var res = bench.Platform.prototype.setParam.apply(
                this, [param, val]
            );
        }
    }
};

/**
@class Represents the Tachyon platform
@extends bench.Platform
*/
bench.Platform.Tachyon = function ()
{
};
bench.Platform.Tachyon.prototype = new bench.Platform();

/**
Identifier for this platform, name of the constructor
*/
bench.Platform.Tachyon.prototype.id = 'Tachyon';

/**
Name of the platform
*/
bench.Platform.Tachyon.prototype.name = 'Tachyon';

/**
Shell command used to start this platform
*/
bench.Platform.Tachyon.prototype.shellCmd = './bench/tachyon.sh';

/**
Set the parameters for this platform
*/
bench.Platform.Tachyon.prototype.setParam = function (param, val)
{
    switch (param)
    {
        //
        // TODO: parse platform-specific params here
        //

        // If the param is unknown, try using the default handler
        default:
        {
            var res = bench.Platform.prototype.setParam.apply(
                this, [param, val]
            );
        }
    }
};

/**
Load a source file dynamically
*/
bench.Platform.Tachyon.prototype.loadSrc = function (srcFile)
{
    // TODO
    // TODO
    // TODO
    // TODO
    // TODO

    /*
    Could compile a separate version just for Tachyon as a
    temporary hax. Use this for the unit tests?
    */

    // FIXME: TEMPORARY HACK
    // Until we have global function calling in compiled code in Tachyon
    var tokens = srcFile.split('.js');
    var file = tokens[0];
    srcFile = file + '_tach.js';

    // Compile the unit as a function
    this.func = compileFileToJSFunc(srcFile);
};

/**
Call a global function by name
*/
bench.Platform.Tachyon.prototype.callFunc = function (funcName, args)
{
    // TODO
    // TODO
    // TODO
    // TODO
    // TODO

    // Call the code for this compilation unit
    this.func();
};

