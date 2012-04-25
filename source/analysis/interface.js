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
Code to test the type analysis and visualize its output.

@author
Maxime Chevalier-Boisvert
*/

/**
@class Base/interface class for type analyses
*/
function TypeAnalysis()
{
    /**
    Library IR units
    */
    this.libUnits = [];

    /**
    Verbose logging flag, enables debug output
    */
    this.verbose = false;
}

/**
Test type analysis on a source file or a list of source files
*/
TypeAnalysis.prototype.testOnFiles = function (fileList, useStdlib)
{
    if (typeof fileList === 'string')
        fileList = [fileList];

    // Clear existing analysis results
    this.init();

    // Get the host and client compilation parameters
    var hostParams = config.hostParams;
    var clientParams = config.clientParams;

    // TODO: improve stdlib support
    // Get the stdlib source file names
    //const libFiles = TACHYON_STDLIB_SRCS;
    const libFiles = [
        'stdlib/object.js',
        'stdlib/array.js',
        'stdlib/function.js',
        'stdlib/boolean.js',

        // TODO
        // FIXME: big type sets happening
        //'stdlib/error.js',

        'stdlib/number.js',
        'stdlib/string.js',

        // TODO
        //'stdlib/regexp.js',

        'stdlib/math.js'
    ];

    // Clear the library IR units
    this.libUnits = [];

    // If the standard library should be included
    if (useStdlib === true)
    {
        // For each stdlib file
        for (var i = 0; i < libFiles.length; ++i)
        {
            var fileName = libFiles[i];

            if (this.verbose === true)
                print('Running type analysis on: "' + fileName + '"');

            // Get the IR for this file
            var ast = parse_src_file(fileName, hostParams);
            var ir = unitToIR(ast, hostParams);

            // Add the code unit to the analysis
            this.addUnit(ir);

            // Add the code to the list of library units
            this.libUnits.push(ir);
        }
    }

    // For each file to be analyzed
    for (var i = 0; i < fileList.length; ++i)
    {
        var fileName = fileList[i];

        if (this.verbose === true)
            print('Running type analysis on: "' + fileName + '"');

        // Get the IR for this file
        var ast = parse_src_file(fileName, clientParams);
        var ir = unitToIR(ast, clientParams);

        if (this.verbose === true)
            print(ir);

        // Add the code unit to the analysis
        this.addUnit(ir);
    }

    // Run the analysis
    this.run();

    // Evaluate the type assertions
    this.evalTypeAsserts();
}

/**
Log information about analysis results
*/
TypeAnalysis.prototype.logResults = function (outFile)
{
    // Log analysis time
    print('itr count: ' + this.itrCount);
    print('time: ' + this.totalTime.toFixed(2) + 's');
    print('');

    // Dump info about functions analyzed
    this.dumpFunctions();

    // Dump info about objects analyzed
    this.dumpObjects();

    // Compute and dump type statistics
    var stats = this.compTypeStats();

    // Print the statistics
    if (stats)
    {
        for (var i = 0; i < stats.length; ++i)
            print(stats[i]);
        print('');
    }

    // If an output file was specified
    if (outFile)
    {
        var outCSV = '';

        function writeRow()
        {
            for (var i = 0; i < arguments.length; ++i)
                outCSV += arguments[i] + ',';

            outCSV += '\n';
        }

        writeRow();
        writeRow('benchmark', outFile);
        writeRow();
        writeRow('itr count', this.itrCount);
        writeRow('time', this.totalTime.toFixed(2))
        writeRow();

        // Write the statistics
        if (stats)
        {
            for (var i = 0; i < stats.length; ++i)
                writeRow(stats[i].name, stats[i].getValue());
        }

        // Write the output file
        writeFile(outFile, outCSV);
    }
}

/**
Test if an instruction, basic block or function originates in library code
*/
TypeAnalysis.prototype.fromLib = function (elem)
{
    var block;
    if (elem instanceof BasicBlock)
        block = elem;
    else if (elem instanceof IRInstr)
        block = elem.parentBlock;
    else if (elem instanceof IRFunction)
        block = elem.hirCFG.entry;
    else
        return false;

    var parentFunc = block.parentCFG.ownerFunc;
    while (parentFunc.parentFunc !== null)
        parentFunc = parentFunc.parentFunc;

    return this.libUnits.indexOf(parentFunc) !== -1;
}

/**
Test if an instruction is a type assertion
*/
function isTypeAssert(instr)
{
    return (
        instr instanceof JSCallInstr &&
        instr.uses.length === 4 &&
        instr.uses[0] instanceof GetGlobalInstr &&
        instr.uses[0].uses[1] instanceof IRConst &&
        instr.uses[0].uses[1].value === 'typeAssert'
    );
}

/**
Test if an instruction was inserted for a type assertion
*/
function isTypeAssertUse(instr)
{
    return (
        instr instanceof GetGlobalInstr &&
        instr.uses[1].value === 'typeAssert'
    );
}

