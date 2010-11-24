/**
@fileOverview
Benchmarking framework implementation.

@copyright
Copyright (c) 2010 Tachyon Javascript Engine, All Rights Reserved
*/

/*
TODO: eliminate numRuns, numDryRuns, benchAlone from dimension config

TODO: v8 extension for shell commands
- cmd string input,
- return stdout in string

TODO: how do we store the output and config info?
- Want JSON file to start
- Output in dimension list?
- Probably want output separate, not config param...

TODO: how do we run the benchmarks? in an external VM?
- This may be necessary to run in Tachyon now
- Need scripts to run with Tachyon inside V8
- Can store benchmark and dimension(s) to test inside JSON file?
- Output comes back in JSON file as well?
- Reset VM between each benchmark, iteration ***
  - shell command, bench id, config file

TODO: design/write env/platform driver
- Needs to do all pre-runs, run benchmark, post-runs
- Sufficient iterations for all dimensions
- Store output when done
- Tricky part is how to run benchmark in Tachyon/V8
- Support for pulling from git

TODO: assemble sample benchmark program?
- start with fib benchmark, for simplicity

TODO: in benchmark description
max number of runs to perform for this benchmark

TODO: dump all in JSON file, do postprocessing separately
*/


/**
Benchmarking namespace
*/
var bench = bench || {};

/**
@class Base class for benchmarking dimension. Represents a criteria
to be benchmarked.
*/
bench.Dimension = function ()
{
    /**
    Number of decimals to keep in the final result.
    By default, keep all decimals.
    @field
    */
    this.numDecimals = undefined;

    /**
    String describing the measurement units.
    By default, no units
    @field
    */
    this.units = '';

    /**
    Array of output values gathered for this dimension.
    @field
    */
    this.output = undefined;
}
bench.Dimension.prototype = {};

/**
Load configuration parameters for this dimension
*/
bench.Dimension.prototype.loadParams = function (cfg)
{
    // For each config parameter
    for (param in cfg)
    {
        // Try to parse the parameter
        this.setParam(param, cfg[param]);
    }
}

/**
Set the generic parameters for this dimension
*/
bench.Dimension.prototype.setParam = function (param, val)
{
    switch (param)
    {
        // If this is the id parameter, ignore it
        case 'id':
        break;

        case 'numDecimals':
        {
            assert (
                val === undefined || isPosInt(val),
                'expected positive integer or undefined'
            );

            this.numDecimals = val;
        }

        default:
        {
            error(
                'unknown config param: "' + param + '" (' +
                val + ') for ' + this.name + ' dimension'
            );
        }   
    }
}

/**
Store the dimension parameters in a configuration object
*/
bench.Dimension.prototype.storeParams = function (obj)
{
    // Store the dimension id
    obj.id = this.id;

    // Store the number of decimals to keep
    obj.numDecimals = this.numDecimals;
}

/**
Get the geometric mean accross output values
*/
/*
bench.Dimension.prototype.getOutMean = function ()
{
    assert (
        a instanceof Array && a.length > 0,
        'output must be array value'
    );

    var prod = 1;

    for (var i = 0; i < this.output.length; ++i)
    {
        assert (typeof this.output[i] == 'number');

        prod *= this.output[i];
    }

    return Math.pow(prod, 1 / this.output.length);
}
*/

/**
@class Run-time benchmarking dimension
@extends bench.Dimension
*/
bench.Dimension.RunTime = function ()
{
    /**
    Keep 2 decimals by default.
    @field
    */
    this.numDecimals = 2;

    /**
    The unit measured is the time in seconds
    @field
    */
    this.units = 's';
}
bench.Dimension.RunTime.prototype = new bench.Dimension();

/**
Identifier for this construction, name of the constructor
*/
bench.Dimension.RunTime.prototype.id = 'RunTime';

/**
Name of the benchmarking dimension
*/
bench.Dimension.RunTime.prototype.name = 'running-time';

/**
Set the parameters for this dimension
*/
bench.Dimension.RunTime.prototype.setParam = function (param, val)
{
    switch (param)
    {
        //
        // TODO: parse dimension-specific params here
        //

        // If the param is unknown, try using the default handler
        default:
        {
            var res = bench.Dimension.prototype.setParam.apply(
                this, [param, val]
            );
        }
    }
}

/**
Store the dimension parameters in a configuration object
*/
bench.Dimension.RunTime.prototype.storeParams = function (obj)
{
    var obj = {};

    //
    // TODO: store dimension-specific parameters here
    //

    // Store the generic parameters for this dimension
    bench.Dimension.prototype.setParam.apply(
        this, [obj]
    );

    return obj;
}

/**
Code to be executed before the benchmark is run
*/
bench.Dimension.RunTime.prototype.preRun = function ()
{
    // Store the start time in ms
    this.startTimeMs = new Date().getTime();
}

/**
Code to be executed after the benchmark is run
*/
bench.Dimension.RunTime.prototype.postRun = function ()
{
    // Get the end time in ms
    var endTimeMs = new Date().getTime();

    // Calculate the total time in seconds
    var totalTimeS = (endTimeMs - this.startTimeMs) / 1000;

    // TODO: output object?? addOutput function?

    // Store the total time in the output values
    this.addOutput(totalTimeS);
}

/**
Number of dry (non-measuring) runs to perform before measuring.
*/
bench.numDryRuns = 0;

/**
Number of measuring runs (excluding dry runs) to perform.
*/
bench.numRuns = 1;

/**
List of benchmarking dimensions to measure
*/
bench.dimList = [];

/**
Path to the directory containing the benchmarks
*/
bench.benchPath = '';

/**
List of benchmarks to test
*/
bench.benchList = [];

/**
List of environments (VMs) to test
*/
bench.envList = []

/**
Map of output gathered, per-environment, per-dimension
*/
bench.output = {};


// TODO:
// bench.addOutput(env, dim, val)

// TODO:
// bench.loadOutput

// TODO:
// bench.storeOutput
// need to store link name of cfg in this

// TODO:
// bench.storeConfig?
// is this needed?

/**
Load a benchmarking configuration file
*/
bench.loadConfig = function (configFile)
{
    print('Loading config file: "' + configFile + '"');

    // Reviver function to parse the config
    function reviver(key, value)
    {
        if (key == 'dimList' && typeof value == 'object')
        {
            for (var idx in value)
            {
                var dimCfg = value[idx];

                // Try to find the corresponding dimension constructor
                var ctor = bench.Dimension[dimCfg.id];

                // Ensure that this is a valid dimension
                if (!ctor || !(ctor.prototype instanceof bench.Dimension))
                    error('unknown dimension: "' + dimCfg.id + '"');

                // Create an object for the dimension
                var dimObj = new ctor();

                // Load the configuration parameters for the dimension
                dimObj.loadParams(dimCfg);

                // Add the dimension to the list
                bench.dimList.push(dimObj);
            }
        }

        return value;
    }

    // Parse JSON code from the config file
    var cfg = JSON.parse(read(configFile), reviver);

    // Store the number of dry runs
    bench.numDryRuns = cfg.numDryRuns;

    // Store the number of measuring runs
    bench.numRuns = cfg.numRuns;

    // Store the benchmarks director
    bench.benchDir = cfg.benchDir;

    // Store the list of benchmarks
    bench.benchList = cfg.benchList;

    // Store the list of environments
    bench.envList = cfg.envList;

    print(bench.numDryRuns);
    print(bench.numRuns);
    print(bench.benchDir);
    print(bench.benchList);
    print(bench.dimList);
    print(bench.envList);
}

/**
Perform benchmarking
*/
bench.runBenchs = function ()
{

    // For each env
        // For each ben
            // For each dry run
                // start child vm
                    // load JSON output file
                    // add output
                    // write JSON output file
            // For each run
                // start child vm

    // TODO: postRuns in reverse order of preRuns
}

