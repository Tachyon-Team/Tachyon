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
Test type analysis on a source file
*/
TypeProp.prototype.testOnFile = function (fileName, verbose)
{
    if (verbose)
        print('Running type analysis on: "' + fileName + '"');

    // Get the IR for this file
    var ast = parse_src_file(fileName, this.params);
    var ir = unitToIR(ast, this.params);

    // Start timing the analysis
    var startTimeMs = (new Date()).getTime();

    // Clear existing analysis results
    this.init();

    // Analyze the code unit
    this.queueUnit(ir);
    var itrCount = this.run();

    // Stop the timing
    var endTimeMs = (new Date()).getTime();
    var time = (endTimeMs - startTimeMs) / 1000;

    // Log analysis time
    if (verbose)
    {
        print('-----------------------------');
        print('itr count: ' + itrCount);
        print('time: ' + time + 's');
        print('');
    }

    // Dump info about functions analyzed
    if (verbose)
        this.dumpFunctions();

    // Dump info about classes analyzed
    if (verbose)
        this.dumpClasses();
}

/**
Dump information gathered about functions
*/
TypeProp.prototype.dumpFunctions = function ()
{





    /*
    // For each function info object
    for (var itr = this.funcInfo.getItr(); itr.valid(); itr.next())
    {
        var pair = itr.get();
        var irFunc = pair.key;
        var funcInfo = pair.value;

        // If this is a unit-level function, skip it
        if (irFunc.astNode instanceof Program)
            continue;

        print('function "' + irFunc.funcName + '"');

        for (var i = 1; i < funcInfo.argTypes.length; ++i)
        {
            var argName = (i == 1)? 'this':irFunc.argVars[i-2];
            
            print('arg ' + argName + ' : ' + funcInfo.argTypes[i]);
        }

        print('ret => ' + funcInfo.retType);

        print('');
    }
    */
}

/**
Dump information gathered about classes
*/
TypeProp.prototype.dumpClasses = function ()
{






    /*
    // For each class descriptor
    for (var itr = ClassDesc.classMap.getItr(); itr.valid(); itr.next())
    {
        var classDesc = itr.get().value;

        print(classDesc);
        print('');
    }
    */   
}

// TODO: HTML + highlighting visualization?

