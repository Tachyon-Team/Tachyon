/**
@fileOverview
Hook to run this program as a benchmark.

@copyright
Copyright (c) 2010 Tachyon Javascript Engine, All Rights Reserved
*/

/**
Namespace for the benchmark hook
*/
benchHook = {};

/**
Initializate the benchmark
*/
benchHook.init = function (platform, benchPath)
{
    platform.loadSrc(benchPath + 'loop_sum.js');

    this.args = [500000000];
};

/**
Cleanup code for the benchmark
*/
benchHook.cleanup = function (platform)
{
};

/**
Run the benchmark
*/
benchHook.run = function (platform)
{
    // TODO: output validation function?

    return platform.callFunc('loop_sum', this.args);
};

