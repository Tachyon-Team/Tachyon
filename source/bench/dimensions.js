/**
@fileOverview
Implementation of benchmarking dimensions.

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
};
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
};

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
};

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
};
bench.Dimension.RunTime.prototype = new bench.Dimension();

/**
Identifier for this dimension, name of the constructor
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
};

/**
Code to be executed before the benchmark is run
*/
bench.Dimension.RunTime.prototype.preRun = function ()
{
    // Store the start time in ms
    this.startTimeMs = new Date().getTime();
};

/**
Code to be executed after the benchmark is run
*/
bench.Dimension.RunTime.prototype.postRun = function ()
{
    // Get the end time in ms
    var endTimeMs = new Date().getTime();

    // Calculate the total time in seconds
    var totalTimeS = (endTimeMs - this.startTimeMs) / 1000;

    // Return the total time as the output value
    return totalTimeS;
};

