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
        var nameRow = ['Platform\\Benchmark'];
        table.push(nameRow);

        // Add the benchmark names
        for (var benchIdx = 0; benchIdx < bench.benchList.length; ++benchIdx)
            nameRow.push(bench.benchList[benchIdx].dir);
        
        // For each platform
        for (var platIdx = 0; platIdx < bench.platList.length; ++platIdx)
        {
            var platform = bench.platList[platIdx];

            var platRow = [];
            table.push(platRow);

            platRow.push(platform.name);

            // For each benchmark
            for (var benchIdx = 0; benchIdx < bench.benchList.length; ++benchIdx)
            {
                var benchmark = bench.benchList[benchIdx];

                var outputs = bench.getOutputs(platform, dimension, benchmark)
                var mean = bench.getOutMean(outputs);
                mean = fmtNumDecimals(mean, dimension.numDecimals);

                platRow.push(mean);
            }
        }

        print(table);
        print(table[0]);
        print(table[1]);

        page.addContents(new HTMLTable(table));
    }

    // Write the HTML file
    page.toFile(repFile);
};

