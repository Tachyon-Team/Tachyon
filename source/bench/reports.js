/**
@fileOverview
Benchmark report generation.

@copyright
Copyright (c) 2010 Tachyon Javascript Engine, All Rights Reserved
*/

/**
Map of output gathered, per-environment, per-dimension
*/
bench.output = {};

/**
Get the geometric mean accross output values
*/
bench.getOutMean = function (outputs)
{
    /*
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
    */
};

/**
Generate a report file
*/
bench.genReport = function ()
{

};

