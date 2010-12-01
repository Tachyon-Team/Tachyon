/**
@fileOverview
Implementation of benchmark management.

@copyright
Copyright (c) 2010 Tachyon Javascript Engine, All Rights Reserved
*/

/**
Benchmarking namespace
*/
var bench = bench || {};

/**
@class Represents a benchmark to be tested.
*/
bench.Benchmark = function (cfg)
{
    assert (
        typeof cfg.dir == 'string'
    );

    /**
    Benchmark directory
    @field
    */
    this.dir = cfg.dir;

    /**
    Benchmark hook object
    @field
    */
    this.hook = null;
}
bench.Benchmark.prototype = {};

/**
Initialize the benchmark and prepare to run it
*/
bench.Benchmark.prototype.init = function (platform)
{
    // Assemble the path to this benchmark
    var benchPath = bench.benchDir + this.dir + '/';

    load(benchPath + 'benchhook.js');

    this.hook = benchHook;

    benchHook = null;

    this.hook.init(platform, benchPath);
}

/**
Cleanup after the benchmark run
*/
bench.Benchmark.prototype.cleanup = function (platform)
{
    this.hook.cleanup(platform);

    this.hook = null;
}

/**
Run the benchmark
*/
bench.Benchmark.prototype.run = function (platform)
{
    this.hook.run(platform);
}

