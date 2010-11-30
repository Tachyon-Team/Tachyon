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
benchHook.init = function (benchPath)
{
    load(benchPath + 'fib.js');
}

/**
Cleanup code for the benchmark
*/
benchHook.cleanup = function ()
{
}

/**
Run the benchmark
*/
benchHook.run = function ()
{
    fib(40);
}

