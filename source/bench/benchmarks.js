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
}
bench.Benchmark.prototype = {};

