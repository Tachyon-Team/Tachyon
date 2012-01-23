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
    // Check the verbose flag
    var oldVerbose = this.verbose;
    this.verbose = (verbose === true);

    if (this.verbose === true)
        print('Running type analysis on: "' + fileName + '"');

    // Get the IR for this file
    var ast = parse_src_file(fileName, this.params);
    var ir = unitToIR(ast, this.params);

    if (this.verbose === true)
        print(ir);

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
    if (this.verbose === true)
    {
        print('-----------------------------');
        print('itr count: ' + itrCount);
        print('time: ' + time + 's');
        print('');
    }

    // Dump info about functions analyzed
    if (this.verbose === true)
        this.dumpFunctions();

    // Dump info about classes analyzed
    if (this.verbose === true)
        this.dumpClasses();

    // Compute and dump type statistics
    if (this.verbose === true)
        this.compTypeStats();

    // Restore the verbose flag
    this.verbose = oldVerbose;
}

/**
Dump information gathered about functions
*/
TypeProp.prototype.dumpFunctions = function ()
{
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

        var retGraph = funcInfo.retGraph;

        for (var i = 1; i < funcInfo.argNodes.length; ++i)
        {
            var argName = (i == 1)? 'this':irFunc.argVars[i-2];
            var argType = retGraph.getTypeSet(funcInfo.argNodes[i]);
            print('arg ' + argName + ' : ' + argType);
        }

        var retType = retGraph.getTypeSet(funcInfo.retNode);
        print('ret => ' + retType);
        
        print('');
    }
}

/**
Dump information gathered about classes
*/
TypeProp.prototype.dumpClasses = function ()
{
    // Get the type graph for the return of the last unit analyzed
    var lastUnit = this.unitList[this.unitList.length-1];
    var typeGraph = this.funcInfo.get(lastUnit).retGraph;

    // For each class descriptor
    for (var itr = TGObject.objMap.getItr(); itr.valid(); itr.next())
    {
        var obj = itr.get().key;

        if (obj.origin instanceof IRFunction)
            continue;

        print('object (' + obj.origin + ')');
        print('{');

        var protoType = typeGraph.getTypeSet(obj.proto);
        print('    proto: ' + protoType);

        for (name in obj.props)
        {
            var propNode = obj.getPropNode(name);
            var propType = typeGraph.getTypeSet(propNode);
            print('    ' + name + ': ' + propType);
        }

        print('}');
        print('');
    }
}

/**
Compute statistics about type sets
*/
TypeProp.prototype.compTypeStats = function ()
{
    var numSets = 0;
    var numAny = 0;
    var numSingleType = 0;
    var numWithUndef = 0;
    var numIntOnly = 0;
    var numStrOnly = 0;
    var numObjOnly = 0;
    var numKnownObj = 0;
    var maxNumObjs = 0;

    // Accumulate stats for one type set occurrence
    function accumStats(set)
    {
        var flags = set.flags;

        var numObjs = set.objSet? set.objSet.length:0;

        var numFlags = 0;
        for (flag in TypeFlags)
        {
            var tf = TypeFlags[flag];

            if (tf === TypeFlags.EMPTY ||
                tf === TypeFlags.OBJEXT ||
                tf === TypeFlags.ANY)
                continue;

            if (flags & TypeFlags[flag])
                ++numFlags;
        }

        numSets += 1;

        if (numFlags === 1)
            numSingleType += 1;

        if (flags === TypeFlags.ANY)
            numAny += 1;

        if (flags & TypeFlags.UNDEF)
            numWithUndef += 1;

        if (flags === TypeFlags.INT)
            numIntOnly += 1;

        if (flags === TypeFlags.STRING)
            numStrOnly += 1;

        if (flags === TypeFlags.OBJECT)
            numObjOnly += 1;

        if (flags === TypeFlags.OBJECT && numObjs === 1)
            numKnownObj += 1;

        maxNumObjs = Math.max(maxNumObjs, numObjs);
    }

    // Accumulate statistics for each use type set seen
    for (var itr = this.typeSets.getItr(); itr.valid(); itr.next())
    {
        var typeSet = itr.get().value;
        accumStats(typeSet);
    }

    print('Num type sets    : ' + numSets);
    print('Num any type     : ' + numAny);
    print('Num single type  : ' + numSingleType);
    print('Num with undef   : ' + numWithUndef);
    print('Num int only     : ' + numIntOnly);
    print('Num string only  : ' + numStrOnly);
    print('Num obj. only    : ' + numObjOnly);
    print('Num known obj.   : ' + numKnownObj);
    print('Max num objs.    : ' + maxNumObjs);

    print('');
}

