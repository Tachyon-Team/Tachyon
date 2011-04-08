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
    Index of the platform to use as a reference to
    rescale the output values.
    @field
    */
    this.refPlat = undefined;

    /**
    Indicates whether the best value for this dimension
    is the minimum or the maximum.
    @field
    */
    this.best = 'min';

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
        break;

        case 'refPlat':
        {
            assert (
                isNonNegInt(val) && val < bench.platList.length,
                'invalid reference platform index : "' + val + '"'
            );

            this.refPlat = val;
        }
        break;

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

