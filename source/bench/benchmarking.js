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
Path to the configuration file for this benchmarking run
*/
bench.cfgFile = '';

/**
Number of dry (non-measuring) runs to perform before measuring.
*/
bench.numDryRuns = 0;

/**
Number of measuring runs (excluding dry runs) to perform.
*/
bench.numRuns = 1;

/**
List of platforms (VMs) to test
*/
bench.platList = []

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
Map of output gathered, per-environment, per-dimension
*/
bench.output = {};

/**
Add output for a given dimension of a given run
*/
bench.addOutput = function (env, dim, val)
{
    // If there is no entry for this environment, create one
    if (!bench.output[env.id])
        bench.output[env.id] = {};

    // If there is no entry for this dimension, create one
    if (!bench.output[env.id][dim.id])
        bench.output[env.id][dim.id] = [];

    // Add the value to the output
    bench.output[env.id][dim.id].push(val);
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
Load the output accumulated up to this point
*/
bench.loadOutput = function (outFile)
{
    // Parse JSON code from the output file
    var out = JSON.parse(read(outFile));

    // Load the config file associated with the output data
    bench.loadConfig(out.cfgFile);

    // Get the current output data
    bench.output = out.output;
}

/**
Store the output accumulated up to this point
*/
bench.storeOutput = function (outFile)
{
    // Create an object to contain the output data
    var out = {
        cfgFile: bench.cfgFile,
        output: bench.output
    };

    // Serialize the object into JSON code
    var fileData = JSON.stringify(out);

    // Write the data to the output file
    writeFile(outFile, fileData);
}

/**
Load a benchmarking configuration file
*/
bench.loadConfig = function (cfgFile)
{
    print('Loading cfg file: "' + cfgFile + '"');

    // Reviver function to parse the config
    function reviver(key, value)
    {
        // Function to create an object from configuration parameters
        function buildObj(cfgObj, baseObj)
        {
            // Try to find the corresponding constructor
            var ctor = baseObj[cfgObj.id];

            // Ensure that this is a valid constructor id
            if (!ctor || !(ctor.prototype instanceof baseObj))
                error('unknown dimension: "' + cfgObj.id + '"');

            // Create an object from the constructor
            var newObj = new ctor();

            // Load the configuration parameters for the object
            newObj.loadParams(cfgObj);

            return newObj;
        }

        if (key == 'dimList' && typeof value == 'object')
        {
            for (var idx in value)
            {
                var dimCfg = value[idx];

                // Create a new object for this dimension
                var cfgObj = buildObj(dimCfg, bench.Dimension);

                // Add the dimension to the list
                bench.dimList.push(cfgObj);
            }
        }

        if (key == 'platList' && typeof value == 'object')
        {
            for (var idx in value)
            {
                var platCfg = value[idx];

                // Create a new object for this platform
                var platObj = buildObj(platCfg, bench.Platform);

                // Add the platform to the list
                bench.platList.push(platObj);
            }
        }

        if (key == 'benchList' && typeof value == 'object')
        {
            for (var idx in value)
            {
                var benchCfg = value[idx];

                // Create a new object for this benchmark
                var benchObj = new bench.Benchmark(benchCfg);

                // Add the benchmark to the list
                bench.benchList.push(benchObj);
            }
        }

        return value;
    }

    // Parse JSON code from the config file
    var cfg = JSON.parse(read(cfgFile), reviver);

    // Store the path to the config file
    bench.cfgFile = cfgFile;

    // Store the number of dry runs
    bench.numDryRuns = cfg.numDryRuns;

    // Store the number of measuring runs
    bench.numRuns = cfg.numRuns;

    // Store the benchmarks director
    bench.benchDir = cfg.benchDir;

    // Store the list of benchmarks
    bench.benchList = cfg.benchList;

    print('Number of dry runs : ' + bench.numDryRuns);
    print('Number of test runs: ' + bench.numRuns);
    print('Benchmark directory: "' + bench.benchDir + '"');
    print('Platforms:');
    for (var i = 0; i < bench.platList.length; ++i)
        print('* ' + bench.platList[i].name);
    print('Dimensions:');
    for (var i = 0; i < bench.dimList.length; ++i)
        print('* ' + bench.dimList[i].name);
    print('Benchmarks:');
    for (var i = 0; i < bench.benchList.length; ++i)
        print('* "' + bench.benchList[i].dir + '"');
}

/**
Perform benchmarking
*/
bench.runBenchs = function ()
{
    //
    // TODO: create initial JSON output file
    //

    // For each platform
    for (var platIdx = 0; platIdx < bench.platList.length; ++platIdx)
    {
        var platform = bench.platList[platIdx];

        // For each benchmark
        for (var benchIdx = 0; benchIdx < bench.benchList.length; ++benchIdx)
        {
            var benchmark = bench.benchList[benchIdx];

            // For each dry run
            for (var dryRunIdx = 0; dryRunIdx < bench.numDryRuns; ++dryRunIdx)
            {
                platform.callVM(
                    {
                        "dataFile"  :"test file.json",
                        "platIdx"   : platIdx,
                        "benchIdx"  : benchIdx,
                        "testRun"   : false
                    }
                );
            }

            // For each test run
            for (var testRunIdx = 0; testRunIdx < bench.numRuns; ++testRunIdx)
            {
                platform.callVM(
                    {
                        "dataFile"  :"test file.json",
                        "platIdx"   : platIdx,
                        "benchIdx"  : benchIdx,
                        "testRun"   : true
                    }
                );
            }
        }
    }
}

/**
Run one benchmark iteration under a given platform
@param platIdx platform index
@param benchIdx benchmark index
@param testRun flag indicating that this is an actual test run
*/
bench.runBench = function (platIdx, benchIdx, testRun)
{
    // TODO: load data file
    // Do this in main?

    // TODO: postRuns in reverse order of preRuns

    // TODO: store data file







}

