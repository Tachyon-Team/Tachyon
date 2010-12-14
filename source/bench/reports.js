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
bench.genReport = function (repFile)
{
    /*
    Report title
    - cfg file
    - data file
    - machine specs (eventually?)

    Header/section for each dimension (units)s

    Table with platforms on cols, benchmarks on rows
    - Avg for all benchmarks if data is numeric
    */

    print();
    print('Generating report');
    print('Report file: ' + repFile);
    print();

    var page = new HTMLPage('Benchmarking Report');

    page.addContents(new HTMLHeader(1, 'Benchmarking Report', true));

    // Print configuration info about this benchmarking run
    page.addContents(new HTMLHeader(2, 'Configuration'));
    page.addContents(new HTMLPar('Config file: "' + bench.cfgFile + '"'));
    page.addContents(new HTMLPar('Data file: "' + bench.dataFile + '"'));
    page.addContents(new HTMLPar('Dry runs: ' + bench.numDryRuns));
    page.addContents(new HTMLPar('Test runs: ' + bench.numRuns));

    // Print information about the system specifications
    page.addContents(new HTMLSep());
    page.addContents(new HTMLHeader(2, 'System Information'));
    page.addContents(new HTMLPar(bench.getSystemInfo()));

    // For each dimension
    for (var dimIdx = 0; dimIdx < bench.dimList.length; ++dimIdx)
    {
        var dimension = bench.dimList[dimIdx];

        page.addContents(new HTMLSep());
        page.addContents(new HTMLHeader(2, 'Dimension - ' + dimension.name));

        if (dimension.refPlat)
            page.addContents(new HTMLPar('Rescaled values (reference is ' + bench.platList[dimension.refPlat].name + ')'));
        else
            page.addContents(new HTMLPar('Mean values (' + dimension.units + ')'));

        page.addContents(new HTMLPar('Best performer highlighted in green.'));

        // Create an array of rows for the table
        var rows = [];

        // Create an array for the best values found so far
        var bestVals = [];

        // For each benchmark
        for (var benchIdx = 0; benchIdx < bench.benchList.length; ++benchIdx)
        {
            var benchmark = bench.benchList[benchIdx];

            var row = []
            rows.push(row);
        
            // For each platform
            for (var platIdx = 0; platIdx < bench.platList.length; ++platIdx)
            {
                var platform = bench.platList[platIdx];

                // Compute the mean of the outputs for this benchmark
                var outputs = bench.getOutputs(platform, dimension, benchmark);
                var mean = bench.getOutMean(outputs);
                row.push(mean);
            }

            // If there is a reference platform
            if (dimension.refPlat)
            {
                // Get the mean for the reference platform
                var refMean = row[dimension.refPlat];

                // For each platform
                for (var platIdx = 0; platIdx < bench.platList.length; ++platIdx)
                {
                    // Rescale this value based on the reference mean
                    row[platIdx] = row[platIdx] / refMean;
                }
            }

            var bestVal = Infinity;

            // For each platform
            for (var platIdx = 0; platIdx < bench.platList.length; ++platIdx)
            {
                // Format the value based on the number of decimals required
                row[platIdx] = fmtNumDecimals(row[platIdx], dimension.numDecimals);

                // Update the best value found so far
                if ((dimension.best == 'min' && Number(row[platIdx]) < bestVal) ||
                    (dimension.best == 'max' && Number(row[platIdx]) > bestVal))
                    bestVal = row[platIdx];
            }

            // Store the best value for this benchmark
            bestVals.push(bestVal);
        }

        // Create a table for the output
        var table = new HTMLTable();

        table.addRow();
        table.addCell('Benchmark');

        // Add the platform names
        for (var platIdx = 0; platIdx < bench.platList.length; ++platIdx)
            table.addCell(bench.platList[platIdx].name);

        // For each benchmark
        for (var benchIdx = 0; benchIdx < bench.benchList.length; ++benchIdx)
        {
            var benchmark = bench.benchList[benchIdx];

            table.addRow();
            table.addCell(benchmark.dir);
        
            // For each platform
            for (var platIdx = 0; platIdx < bench.platList.length; ++platIdx)
            {
                var platform = bench.platList[platIdx];

                curVal = rows[benchIdx][platIdx];

                var attribs = { 
                    "bgcolor":((curVal == bestVals[benchIdx])? "#00CC00":"#CCCCCC") 
                };

                table.addCell(String(curVal), attribs);
            }
        }

        page.addContents(table);
    }

    // Write the HTML file
    page.toFile(repFile);
};

/**
Produce information about the system we are running on
*/
bench.getSystemInfo = function ()
{
    // TODO: For mac, system_profiler

    function parseLines(str, delim)
    {
        var inLines = str.split('\n');

        var outLines = [];

        for (var i = 0; i < inLines.length; ++i)
        {
            var tokens = inLines[i].split(delim);

            tokens = tokens.filter(function (t) { return (t && t != delim); });

            //print(tokens.length);

            if (tokens.length < 2)
                continue;

            var key = stripStr(tokens[0]);
            var val = stripStr(tokens[1]);

            //print('key "' + key + '"');
            //print('val "' + val + '"');

            outLines.push([key, val]);
        }

        return outLines;
    }

    var osInfo = stripStr(shellCommand('uname -s -r -o'));

    var cpuInfo = shellCommand('cat /proc/cpuinfo');

    var ramInfo = shellCommand('free -m');

    var cpuInfo = parseLines(cpuInfo, ':').filter(
        function (l) { return l[0] == 'cpu MHz' || l[0] == 'model name'; }
    );

    var ramInfo = parseLines(ramInfo, ' ').filter(
        function (l) { return l[0] == 'Mem:'; }
    );

    output = '';

    output += 'OS: ' + osInfo + '\n\n';
    
    output += 'RAM: ' + ramInfo[0][1] + 'MB' + '\n\n';

    output += 'CPU cores:';
    for (var i = 0; i < cpuInfo.length; ++i)
        output += '\n' + cpuInfo[i][0] + ': ' + cpuInfo[i][1];

    return output;
}

