/**
@fileOverview
Benchmarking framework implementation.

@copyright
Copyright (c) 2010 Tachyon Javascript Engine, All Rights Reserved
*/

/**
Benchmarking namespace
*/
var bench = bench || {};

/**
@class Base class for benchmarking dimension. Represents a criteria
to be benchmarked.
*/
bench.Dimension = function (
)
{
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
        // If this is the id parameter, ignore it
        if (param == 'id')
        {
            continue;
        }
        else
        {
            // Try to parse the parameter
            var res = this.setParam(param, cfg[param]);

            // If the parsing failed
            if (!res)
            {
                error(
                    'unknown or invalid config param: "' + param + '" (' +
                    val + ') for ' + this.name + ' dimension'
                );
            }
        }
    }
}

/**
@class Run-time benchmarking dimension
*/
bench.Dimension.RunTime = function (cfg)
{
    /* TODO: fill in default params 
    numRuns,
    numDryRuns, 
    benchAlone, 
    dataKind, 
    sigDigits
    */



}
bench.Dimension.RunTime.prototype = new bench.Dimension();

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
        case 'numRuns':
        {
            if (typeof val !== 'number' || val < 1)
                return false;

            this.numRuns = val;
        }
        return true;

        default:
        return false;
    }
}

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





/*
TODO: need to be able to specify set of benchmarks, platforms, dimensions
to test against... use a config script?


Specify:
platforms to test,
dimensions to benchmark
- optional params for dimensions (defaults otherwise)




For each dimension:
- Want a loader function to parse/validate JSON data, fill in defaults
- Want pre-run and post-run functions to gather data

Here, each dimension has a "kind", an identifier (id field)

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

                bench.dimList.push(dimObj);
            }
        }

        return value;
    }

    // Parse JSON code from the config file
    var cfg = JSON.parse(read(configFile), reviver);

    // Store the benchmarks director
    bench.benchDir = cfg.benchDir;

    // Store the list of benchmarks
    bench.benchList = cfg.benchList;


    print(bench.benchList);
    print(bench.benchDir);
    print(bench.dimList);





}








