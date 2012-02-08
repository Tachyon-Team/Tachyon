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
Test type analysis on a source file or a list of source files
*/
TypeProp.prototype.testOnFile = function (fileList, useStdlib, verbose)
{
    if (typeof fileList === 'string')
        fileList = [fileList];

    // Check the verbose flag
    var oldVerbose = this.verbose;
    this.verbose = (verbose === true);

    // Clear existing analysis results
    this.init();

    // Get the host and client compilation parameters
    var hostParams = config.hostParams;
    var clientParams = config.clientParams;

    // TODO: improve stdlib support
    // Get the stdlib source file names
    //const libFiles = TACHYON_STDLIB_SRCS;
    const libFiles = [
        'stdlib/array.js',
        'stdlib/math.js'
    ];

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

    // Start timing the analysis
    var startTimeMs = (new Date()).getTime();

    // Run the analysis
    var itrCount = this.run();

    assert (
        itrCount >= fileList.length,
        'less than one analysis iteration per code unit'
    );

    assert (
        this.blockGraphs.numItems > 0,
        'no blocks were analyzed'
    );

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

    // Evaluate the type assertions
    this.evalTypeAsserts();

    // Dump info about functions analyzed
    if (this.verbose === true)
        this.dumpFunctions();

    // Dump info about objects analyzed
    if (this.verbose === true)
        this.dumpObjects();

    // Compute and dump type statistics
    if (this.verbose === true)
        this.compTypeStats();

    // Restore the verbose flag
    this.verbose = oldVerbose;
}

/**
Evaluate type assertions
*/
TypeProp.prototype.evalTypeAsserts = function ()
{
    function fail(desc, msg)
    {
        throw Error(
            'Type assertion failed: ' + msg + '\n' +
            'set : ' + desc.typeSet + '\n' +
            'test: ' + desc.test
        );
    }

    function evalExpr(expr, set)
    {
        if (typeof expr === 'string')
        {
            var flag = TypeFlags[expr.toUpperCase()];

            assert (
                typeof flag === 'number',
                'invalid flag name: "' + expr + '"'
            );

            return (set.flags & flag) !== 0;
        }

        if (expr instanceof Array)
        {
            var numArgs = expr.length - 1;

            // Value equality test
            if (expr[0] === 'val' && numArgs === 1)
            {
                var val = expr[1];

                if (typeof val === 'string')
                    return (set.strVal === val);
                if (typeof val === 'number')
                    return (set.rangeMin === val && set.rangeMax === val);

                error('invalid value in test');
            }

            // Greater-or-equal-to test
            if (expr[0] === '>=' && typeof expr[1] === 'number' && numArgs === 1)
            {
                var val = expr[1];
                return set.rangeMin >= val;
            }

            // Less-or-equal-to test
            if (expr[0] === '<=' && typeof expr[1] === 'number' && numArgs === 1)
            {
                var val = expr[1];
                return set.rangeMax <= val;
            }

            // Conjunction
            if (expr[0] === 'and')
            {
                var r = true;
                for (var i = 1; i <= numArgs; ++i)
                    r = r && evalExpr(expr[i], set);

                return r;
            }

            // Disjunction
            if (expr[0] === 'or')
            {
                var r = false;
                for (var i = 1; i <= numArgs; ++i)
                    r = r || evalExpr(expr[i], set);

                return r;
            }

            // Negation
            if (expr[0] === 'not' && numArgs === 1)
            {
                return !evalExpr(expr[1], set);
            }

            error('invalid type test operator: ' + expr[0]);
        }

        error ('invalid type expr type');
    }

    // For each type assertion
    for (var itr = this.typeAsserts.getItr(); itr.valid(); itr.next())
    {
        var desc = itr.get().value;

        var set = desc.typeSet;

        // Parse the test expression
        var testExpr = JSON.parse(desc.test);

        if (set.flags === TypeFlags.ANY)
            fail(desc, 'type set is any');

        // Evaluate the expression
        var r = evalExpr(testExpr, set);

        if (r !== true)
            fail(desc, 'test failed');
    }
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
            var argType = retGraph.getType(funcInfo.argNodes[i]);
            print('arg ' + argName + ' : ' + argType);
        }

        var idxArgType = retGraph.getType(funcInfo.idxArgNode);
        print('idx arg : ' + idxArgType);

        var retType = retGraph.getType(funcInfo.retNode);
        print('ret => ' + retType);
        
        print('');
    }
}

/**
Dump information gathered about classes
*/
TypeProp.prototype.dumpObjects = function ()
{
    // Get the type graph for the return of the last unit analyzed
    var lastUnit = this.unitList[this.unitList.length-1];
    var typeGraph = lastUnit.retGraph;

    assert (
        typeGraph instanceof TypeGraph,
        'no ret type graph for last unit'
    );

    // For each class descriptor
    for (var itr = TGObject.objMap.getItr(); itr.valid(); itr.next())
    {
        var obj = itr.get().key;

        if (obj.origin instanceof IRFunction)
            continue;

        assert (
            obj instanceof TGObject,
            'invalid object: ' + obj
        );

        print('object <' + obj.getName() + '>');
        print('{');

        var protoType = typeGraph.getType(obj.proto);
        print('    proto: ' + protoType);

        for (name in obj.props)
        {
            var propNode = obj.getPropNode(name);
            var propType = typeGraph.getType(propNode);
            print('    ' + name + ': ' + propType);
        }

        var idxType = typeGraph.getType(obj.idxProp);
        print('    []: ' + idxType);

        print('}');
        print('');
    }
}

/**
Compute statistics about type sets
*/
TypeProp.prototype.compTypeStats = function ()
{
    var maxNumObjs = 0;

    var numGetProp      = 0;
    var numGetObj       = 0;
    var numGetSingle    = 0;
    var numGetDef       = 0;

    var numPutProp      = 0;
    var numPutObj       = 0;
    var numPutSingle    = 0;

    var numArithOp  = 0;
    var numArithInt = 0;

    const ta = this;

    function accumStats(instr)
    {
        assert (
            instr instanceof IRInstr,
            'invalid instruction'
        );

        function getUseType(idx)
        {
            return ta.typeSets.get({ instr:instr, idx:idx });
        }

        var outType = ta.typeSets.get({ instr:instr });    

        var numObjs = outType.objSet? outType.objSet.length:0;
        maxNumObjs = Math.max(maxNumObjs, numObjs);

        // Get property instruction
        if (instr instanceof GetPropInstr || instr instanceof GetGlobalInstr)
        {
            var u0 = getUseType(0);

            numGetProp += 1;

            if ((u0.flags & ~TypeFlags.OBJEXT) === 0)
            {
                numGetObj += 1;

                if (u0.objSet && u0.objSet.length === 1)
                    numGetSingle += 1;
            }

            if ((outType.flags & TypeFlags.UNDEF) === 0)
                numGetDef += 1;
        }

        // Put property instruction
        if (instr instanceof PutPropInstr)
        {
            var u0 = getUseType(0);

            numPutProp += 1;

            if ((u0.flags & ~TypeFlags.OBJEXT) === 0)
            {
                numPutObj += 1;

                if (u0.objSet && u0.objSet.length === 1)
                    numPutSingle += 1;
            }
        }

        // Arithmetic instructions
        else if (instr instanceof JSArithInstr)
        {
            var u0 = getUseType(0);
            var u1 = getUseType(1);

            numArithOp += 1;

            if (u0.flags === TypeFlags.INT && u1.flags === TypeFlags.INT)
                numArithInt += 1;
        }
    }

    // For each block type graph we have
    for (var itr = this.blockGraphs.getItr(); itr.valid(); itr.next())
    {
        var block = itr.get().key.block;

        // For each instruction of the block
        for (var i = 0; i < block.instrs.length; ++i)
        {
            var instr = block.instrs[i];

            accumStats(instr);
        }
    }

    function compPercent(num, denom)
    {
        if (denom === 0)
            return 'N/A';

        return Number(100 * num / denom).toFixed(0) + '%';
    }

    var perGetObj    = compPercent(numGetObj    , numGetProp);
    var perGetSingle = compPercent(numGetSingle , numGetProp);
    var perGetDef    = compPercent(numGetDef    , numGetProp);

    var perPutObj    = compPercent(numPutObj    , numPutProp);
    var perPutSingle = compPercent(numPutSingle , numPutProp);

    var perArithInt  = compPercent(numArithInt  , numArithOp);

    print('Max num objs: ' + maxNumObjs);
    print('');
    print('Get prop obj only  : ' + perGetObj    + ' (' + numGetObj + ')');
    print('Get prop single obj: ' + perGetSingle + ' (' + numGetSingle + ')');
    print('Get prop not undef : ' + perGetDef    + ' (' + numGetDef + ')');
    print('');
    print('Put prop obj only  : ' + perPutObj    + ' (' + numPutObj + ')');
    print('Put prop single obj: ' + perPutSingle + ' (' + numPutSingle + ')');
    print('');
    print('Arith on int & int : ' + perArithInt  + ' (' + numArithInt + ')');

    print('');
}

