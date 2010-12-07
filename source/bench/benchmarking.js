/**
@fileOverview
Benchmarking framework implementation.

@copyright
Copyright (c) 2010 Tachyon Javascript Engine, All Rights Reserved
*/

/*
TODO: design/write env/platform driver
- Needs to do all pre-runs, run benchmark, post-runs
- Sufficient iterations for all dimensions
- Store output when done
- Tricky part is how to run benchmark in Tachyon/V8
- Support for pulling from git

TODO: dump all in JSON file, do postprocessing separately

TODO: driver for Tachyon under V8
- Wait for Erick to be done refactoring w/linking
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
bench.platList = [];

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
bench.addOutput = function (platform, dimension, benchmark, outVal)
{
    assert (
        bench.output,
        'output object not defined'
    );

    // If there is no entry for this environment, create one
    if (!bench.output[platform.id])
        bench.output[platform.id] = {};

    // If there is no entry for this dimension, create one
    if (!bench.output[platform.id][dimension.id])
        bench.output[platform.id][dimension.id] = {};

    // If there is no entry for this benchmark, create one
    if (!bench.output[platform.id][dimension.id][benchmark.dir])
        bench.output[platform.id][dimension.id][benchmark.dir] = [];

    // Add the value to the output
    bench.output[platform.id][dimension.id][benchmark.dir].push(outVal);
};

/*
Get all outputs for a given platform/dimension/benchmark combination
*/
bench.getOutputs = function (platform, dimension, benchmark)
{
    return bench.output[platform.id][dimension.id][benchmark.dir];
};

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

    assert (
        typeof bench.output == 'object',
        'could not load output object from output file'
    );
};

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
};

/**
Load a benchmarking configuration file
*/
bench.loadConfig = function (cfgFile)
{
    print('Loading cfg file: "' + cfgFile + '"');

    // Function to parse include directives
    function parseInclude(value)
    {
        // If this is not an include directive, return the value unchanged
        if (typeof value.include != 'string')
            return value;

        var inclFile = value.include;

        // Parse JSON code from the include file
        var obj = JSON.parse(read(inclFile), reviver);

        return obj;
    }

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
            value = parseInclude(value);

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
            value = parseInclude(value);

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
            value = parseInclude(value);

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
};

/**
Perform benchmarking
*/
bench.runBenchs = function ()
{
    // Until an available file name is found
    for (var num = 1; num < 1000; ++num)
    {
        var date = new Date();

        // Generate a file name containing today's date
        var outFile =
            'benchdata-' +
            date.getFullYear() + '-' + 
            leftPadStr(date.getMonth(), '0', 2) + '-' + 
            leftPadStr(date.getDate(), '0', 2) + '#' +
            leftPadStr(num, '0', 3) +
            '.json';

        // Test if the file exists
        var avail = false;
        try
        {
            read(outFile);
        }
        catch (e)
        {
            avail = true;
        }

        // If the name is availanble, stop
        if (avail)
            break;
    }

    // Create the initial JSON output file
    bench.storeOutput(outFile);

    print();
    print('Starting tests');
    print('Output file: ' + outFile);
    print();

    // For each platform
    for (var platIdx = 0; platIdx < bench.platList.length; ++platIdx)
    {
        var platform = bench.platList[platIdx];

        print(
            '* Platform ' + platform.name + ' ('  + (platIdx+1) + '/' + 
            bench.platList.length + ')'
        );

        // For each benchmark
        for (var benchIdx = 0; benchIdx < bench.benchList.length; ++benchIdx)
        {
            var benchmark = bench.benchList[benchIdx];

            print(
                '** Benchmark "' + benchmark.dir + '" (' + (benchIdx+1) + '/' +
                bench.benchList.length + ')'
            );

            // For each dry run
            for (var dryRunIdx = 0; dryRunIdx < bench.numDryRuns; ++dryRunIdx)
            {
                print(
                    '*** Dry run ' + (dryRunIdx+1) + '/' + bench.numDryRuns
                );

                platform.callVM(
                    {
                        "outFile"   : outFile,
                        "platIdx"   : platIdx,
                        "benchIdx"  : benchIdx,
                        "testRun"   : false
                    }
                );
            }

            // For each test run
            for (var testRunIdx = 0; testRunIdx < bench.numRuns; ++testRunIdx)
            {
                print(
                    '*** Test run ' + (testRunIdx+1) + '/' + bench.numRuns
                );

                platform.callVM(
                    {
                        "outFile"   : outFile,
                        "platIdx"   : platIdx,
                        "benchIdx"  : benchIdx,
                        "testRun"   : true
                    }
                );
            }
        }
    }

    print('Tests done');
};

/**
Run one benchmark iteration under a given platform
@param platIdx platform index
@param benchIdx benchmark index
@param testRun flag indicating that this is an actual test run
*/
bench.runBench = function (platIdx, benchIdx, testRun)
{
    var platform = bench.platList[platIdx];

    var benchmark = bench.benchList[benchIdx];

    //print('Benchmark run: "' + benchmark.dir + '"');

    // Load/initialize the benchmark
    benchmark.init(platform);

    // Execute pre-run code for each dimension in listed order
    for (var i = 0; i < bench.dimList.length; ++i)
        bench.dimList[i].preRun();

    // Run the benchmark once
    benchmark.run(platform);

    // Execute post-run code for each dimension in reverse order
    for (var i = bench.dimList.length - 1; i >= 0; --i)
    {
        var dim = bench.dimList[i];

        var outVal = dim.postRun();

        assert (bench.output);

        // If this is a test run, add the output for this benchmark
        if (testRun)
        {
            bench.addOutput(
                platform, 
                dim, 
                benchmark, 
                outVal
            );
        }
    }

    // Perform cleanup
    benchmark.cleanup(platform);
};

