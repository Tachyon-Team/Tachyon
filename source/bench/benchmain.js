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
Entry point for the benchmarking code.

@copyright
Copyright (c) 2010 Tachyon Javascript Engine, All Rights Reserved
*/

/**
Entry point function for the benchmarking code
*/
function main()
{
    // Parse the command-line arguments
    var args = parseCmdLine();

    /*
    for (argName in args.options)
    {
        print('"' + argName + '" = "' + args.options[argName] + '"');
    }
    */

    // If a config file argument is supplied
    if (args.options['cfgFile'])
    {
        bench.loadConfig(args.options['cfgFile']);

        bench.runBenchs();
    }

    // Otherwise, if a data file argument is supplied
    else if (args.options['dataFile'])
    {
        bench.loadOutput(args.options['dataFile']);

        bench.loadConfig(bench.cfgFile);

        // If a benchmark should be run
        if (args.options['platIdx'])
        {
            bench.runBench(
                Number(args.options['platIdx']),
                Number(args.options['benchIdx']),
                Boolean(args.options['testRun'])
            );

            bench.storeOutput(args.options['dataFile']);
        }
    }
    
    // Otherwise, arguments are missing
    else
    {
        print('expected config file or data file argument');
        return;
    }

    // If a report file argument is supplied
    if (args.options['genReport'])
    {
        bench.loadOutput(bench.dataFile);

        // Generate a report file
        bench.genReport(args.options['genReport']);
    }
}

try 
{
    // Initialize Tachyon
    if (this.initialize)
        initialize();

    main();

    // Uninitialize Tachyon
    if (this.uninitialize)
        uninitialize();
}
catch (e)
{
    if (e.stack)
        print(e.stack);
    else
        print(e);
}

