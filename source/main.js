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
 *  TO, THE IMPLIED WARRANTIES OF MERCHApNTABILITY AND FITNESS FOR A
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
Code specific to the JavaScript VM's entry point, handling of
command-line arguments, etc.

@author
Maxime Chevalier-Boisvert
Marc Feeley
*/

/**
Entry point for the Tachyon VM.
*/
function main()
{
    // If we are running bootstrapped
    if (RUNNING_IN_TACHYON)
    {
        // Initialize the Tachyon configuration
        initConfig(PLATFORM_64BIT, log.level(undefined));

        // Perform a minimal Tachyon compilation
        initPrimitives(config.hostParams);
        initStdlib(config.hostParams);

        // Call the Tachyon read-eval-print loop
        tachyonRepl();

        return;
    }

    // Parse the command-line arguments
    var args = parseCmdLine();

    // Get the verbosity command-line option value
    var verbosity = log.level(args.options['v']);

    // Initialize the Tachyon configuration
    initConfig(PLATFORM_64BIT, verbosity);

    // If the unit tests should be run
    if (args.options['test'])
    {
        // Run all unit tests, don't catch exceptions
        tests.run(false, undefined, true);
    }

    // If bootstrap compilation is requested
    else if (args.options['bootstrap'])
    {
        // Perform a full bootstrap without writing an image
        bootstrap(config.bootParams);

        // ???
        // Profit!
    }

    // If we are to write an executable image
    else if (args.options['image'])
    {
        // TODO
    }

    // If type analysis should be performed
    else if (args.options['ta'])
    {
        const params = config.hostParams;

        var taName = args.options['ta'];

        var analysis;

        switch (taName)
        {
            case true:
            taName = 'SPSTF';
            case 'SPSTF':
            analysis = new SPSTF();
            break;

            //case true:
            //taName = 'TypeProp';
            case 'TypeProp':
            analysis = new TypeProp();
            break;

            case 'MozTI':
            analysis = new MozTI();
            break;

            default:
            error('invalid analysis name: "' + taName + '"');
        }

        if (args.files.length === 0)
            error('no files specified for type analysis');

        // Perform a partial initialization of the Tachyon runtime
        // without compiling machine code
        initPrimitives(config.hostParams, true);

        print('Running ' + taName + ' analysis on: ' + args.files);

        // Run the type analysis
        try 
        {
            analysis.testOnFiles(args.files, true, false);
        }
        catch (e)
        {
            if (e.stack)
                print(e.stack);
            else
                print(e);
        }

        // Output analysis results
        analysis.logResults(args.options['outfile']);

        // Output HTML visualization
        if (args.options['html'])
            analysis.writeHTML(args.options['html'])
    }

    // If source files or inline source are provided    
    else if (args.files.length > 0 || args.options['e'])
    {
        // Perform a minimal Tachyon compilation
        initPrimitives(config.hostParams);
        initStdlib(config.hostParams);

        config.hostParams.printAST = args.options["ast"];
        config.hostParams.printHIR = args.options["hir"];
        config.hostParams.printLIR = args.options["lir"];
        config.hostParams.printASM = args.options["asm"];

        if (args.options['e'])
        {
            var ir = compileSrcString(args.options['e'] + ";", config.hostParams);
            var bridge = makeBridge(
                ir,
                config.hostParams,
                [],
                new CIntAsBox()
            );

            bridge(config.hostParams.ctxPtr);
        }

        for (var i = 0; i < args.files.length; i++)
        {
            if (args.options["time"])
            {
                print("Executing " + args.files[i]);
            }

            var startTimeMs = (new Date()).getTime();

            var ir = compileSrcFile(args.files[i], config.hostParams);

            var midTimeMs = (new Date()).getTime();

            var bridge = makeBridge(
                ir,
                config.hostParams,
                [],
                new CIntAsBox()
            );

            bridge(config.hostParams.ctxPtr);

            var endTimeMs = (new Date()).getTime();
            var compTimeMs = midTimeMs - startTimeMs;
            var execTimeMs = endTimeMs - midTimeMs;

            if (args.options["time"])
            {
                print("  compilation time: " + compTimeMs + " ms");
                print("  execution time:   " + execTimeMs + " ms");
            }
        }
    }

    // If there are no filenames on the command line, start shell mode
    else
    {
        // Perform a minimal Tachyon compilation
        initPrimitives(config.hostParams);
        initStdlib(config.hostParams);

        // Call the Tachyon read-eval-print loop
        tachyonRepl();
    }
}

try
{
    main();
}

catch (e)
{
    if (e.stack)
        print(e.stack);
    else
        print(e);
}

