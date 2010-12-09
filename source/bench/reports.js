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
    assert (
        outputs instanceof Array && outputs.length > 0,
        'output must be array value'
    );

    var prod = 1;

    for (var i = 0; i < outputs.length; ++i)
    {
        assert (typeof outputs[i] == 'number');

        prod *= outputs[i];
    }

    return Math.pow(prod, 1 / outputs.length);
};

/**
Generate a report file
*/
bench.genReport = function (outFile)
{
    /*
    Report title
    - cfg file
    - data file
    - machine specs (eventually?)

    Header/section for each dimension (units)s

    Table with platforms on cols, benchmarks on rows
    - Avg for all benchmarks if data numeric?
    */

    // TODO: highlight best performer in bold/italic/green

    // TODO: specify one platform as reference, rescale others

    repFile = 'benchreport.html';

    print();
    print('Generating report');
    print('Report file: ' + repFile);
    print();

    var page = new HTMLPage('Benchmarking Report');

    page.addContents(new HTMLHeader(1, 'Benchmarking Report', true));

    page.addContents(new HTMLHeader(2, 'Configuration'));
    page.addContents(new HTMLPar('Config file: "' + bench.cfgFile + '"'));
    page.addContents(new HTMLPar('Data file: "' + outFile + '"'));
    page.addContents(new HTMLPar('Dry runs: ' + bench.numDryRuns));
    page.addContents(new HTMLPar('Test runs: ' + bench.numRuns));

    // For each dimension
    for (var i = 0; i < bench.dimList.length; ++i)
    {
        page.addContents(new HTMLSep());

        var dimension = bench.dimList[i];

        page.addContents(new HTMLHeader(2, 'Dimension - ' + dimension.name));

        page.addContents(new HTMLPar('Mean values (' + dimension.units + ')'));

        var table = [];
        var platRow = ['Benchmark'];
        table.push(platRow);

        // Add the platform names
        for (var platIdx = 0; platIdx < bench.platList.length; ++platIdx)
            platRow.push(bench.platList[platIdx].name);

        // For each benchmark
        for (var benchIdx = 0; benchIdx < bench.benchList.length; ++benchIdx)
        {
            var benchmark = bench.benchList[benchIdx];

            var benchRow = [];
            table.push(benchRow);

            benchRow.push(benchmark.dir);
        
            // For each platform
            for (var platIdx = 0; platIdx < bench.platList.length; ++platIdx)
            {
                var platform = bench.platList[platIdx];

                var outputs = bench.getOutputs(platform, dimension, benchmark);
                var mean = bench.getOutMean(outputs);
                mean = fmtNumDecimals(mean, dimension.numDecimals);

                benchRow.push(mean);
            }
        }

        page.addContents(new HTMLTable(table));
    }

    // Write the HTML file
    page.toFile(repFile);
};

