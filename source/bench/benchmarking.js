/* _________________________________________________________________________
 *
 *             Tachyon : A Self-Hosted JavaScript Virtual Machine
 *
 *
 *  This file is part of the Tachyon JavaScript project. Tachyon is
 *  distributed at:
 *  http://github.com/Tachyon-Team/Tachyon
 *
 *
 *  Copyright (c) 2011, Universite de Montreal
 *  All rights reserved.
 *
 *  This software is licensed under the following license (Modified BSD
 *  License):
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions are
 *  met:
 *    * Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *    * Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions and the following disclaimer in the
 *      documentation and/or other materials provided with the distribution.
 *    * Neither the name of the Universite de Montreal nor the names of its
 *      contributors may be used to endorse or promote products derived
 *      from this software without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 *  IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 *  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 *  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL UNIVERSITE DE
 *  MONTREAL BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 *  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 *  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 *  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 *  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 *  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 *  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * _________________________________________________________________________
 */

/**
@fileOverview
Benchmarking framework implementation.

@copyright
Copyright (c) 2010 Tachyon Javascript Engine, All Rights Reserved
*/

/**
Path to the configuration file for this benchmarking run
*/
bench.cfgFile = '';

/**
Path to the file into which benchmark data should be stored
*/
bench.dataFile = '';

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
bench.loadOutput = function (dataFile, loadConfig)
{
    // Store the path to the data file
    this.dataFile = dataFile;

    // Parse JSON code from the output file
    var out = JSON.parse(readFile(dataFile));

    // Store the config file path
    bench.cfgFile = out.cfgFile;

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
bench.storeOutput = function (dataFile)
{
    // Store the path to the data file
    this.dataFile = dataFile;

    // Create an object to contain the output data
    var out = {
        cfgFile: bench.cfgFile,
        output: bench.output
    };

    // Serialize the object into JSON code
    var fileData = JSON.stringify(out);

    // Write the data to the output file
    writeFile(dataFile, fileData);
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
        var obj = JSON.parse(readFile(inclFile), reviver);

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
    var cfg = JSON.parse(readFile(cfgFile), reviver);

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
        var dataFile =
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
            readFile(dataFile);
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
    bench.storeOutput(dataFile);

    print();
    print('Starting tests');
    print('Data file: ' + dataFile);
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
                        "dataFile"   : dataFile,
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
                        "dataFile"  : dataFile,
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

    print('Benchmark run');
    print('Platform: ' + platform.name + ' (' + platIdx + ')');
    print('Benchmark: "' + benchmark.dir + '"');

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

