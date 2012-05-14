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

    // List of library IR units
    this.libUnits = [];

    // List of all IR units analyzed
    this.allUnits = [];

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
            this.allUnits.push(ir);
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

        // Add this unit to the list of analyzed units
        this.allUnits.push(ir);
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
Compute statistics about type sets
*/
TypeAnalysis.prototype.compTypeStats = function (outFile)
{
    const ta = this;

    function compPercent(num, denom)
    {
        if (denom === 0)
            return 'N/A';

        return Number(100 * num / denom).toFixed(0);
    }

    function MaxStat(name)
    {
        this.name = name;
        this.maxVal = -Infinity;
    }

    MaxStat.prototype.count = function (val)
    {
        this.maxVal = Math.max(this.maxVal, val);
    }

    MaxStat.prototype.getValue = function ()
    {
        if (this.maxVal !== -Infinity)
            return this.maxVal;
        else
            return 'N/A';
    }

    MaxStat.prototype.toString = function ()
    {
        return this.name + ': ' + this.getValue();
    }

    function PercentStat(name)
    {
        this.name = name;
        this.trueCnt = 0;
        this.falseCnt = 0;
    }

    PercentStat.prototype.count = function (val)
    {
        if (val)
            this.trueCnt++;
        else
            this.falseCnt++;
    }

    PercentStat.prototype.getValue = function ()
    {
        var total = this.trueCnt + this.falseCnt;

        var percent = compPercent(this.trueCnt, total);

        return percent;
    }

    PercentStat.prototype.toString = function ()
    {
        var total = this.trueCnt + this.falseCnt;

        var percent = compPercent(this.trueCnt, total);

        return this.name + ': ' + percent + '% (' + this.trueCnt + '/' + total + ')';
    }

    // List of statistics to gather
    var stats = [];

    function addStat(stat)
    {
        stats.push(stat);
        return stat;
    }

    var maxNumObjs  = addStat(new MaxStat('max obj set size'));

    var getObj      = addStat(new PercentStat('getProp on object only'));
    var getSingle   = addStat(new PercentStat('getProp on known object'));
    var getDef      = addStat(new PercentStat('getProp output not undef'));

    var putObj      = addStat(new PercentStat('putProp on object only'));
    var putSingle   = addStat(new PercentStat('putProp on known object'));

    var callMono    = addStat(new PercentStat('function call monomorphic'));

    var arithInt    = addStat(new PercentStat('arith op on int & int'));
    var cmpInt      = addStat(new PercentStat('compare op on int & int'));

    var branchKnown = addStat(new PercentStat('branch direction known'));

    function accumStats(instr)
    {
        assert (
            instr instanceof IRInstr,
            'invalid instruction'
        );

        var outType = ta.getTypeSet(instr);

        var useTypes = [];

        for (var i = 0; i < instr.uses.length; ++i)
        {
            var type = ta.getTypeSet(instr, i);

            assert (
                type instanceof TypeSet,
                'invalid type set:\n' + 
                type + '\n' +
                'for use:\n' +
                i + '\n' +
                'of instruction:\n' +
                instr
            );

            useTypes.push(type);
        }

        if (outType instanceof TypeSet)
            maxNumObjs.count(outType.getNumObjs());

        // Get property instruction
        if (instr instanceof GetPropInstr || instr instanceof GetGlobalInstr)
        {
            var u0 = useTypes[0];

            getObj.count((u0.flags & ~TypeFlags.EXTOBJ) === 0);

            getSingle.count(
                (u0.flags & ~TypeFlags.EXTOBJ) === 0 &&
                u0.getNumObjs() === 1
            );

            getDef.count((outType.flags & TypeFlags.UNDEF) === 0);
        }

        // Put property instruction
        else if (instr instanceof PutPropInstr)
        {
            var u0 = useTypes[0];

            putObj.count((u0.flags & ~TypeFlags.EXTOBJ) === 0);

            putSingle.count(
                (u0.flags & ~TypeFlags.EXTOBJ) === 0 &&
                u0.getNumObjs() === 1
            );
        }

        // Function call/new instruction
        else if (instr instanceof JSCallInstr || instr instanceof JSNewInstr)
        {
            var u0 = useTypes[0];

            callMono.count(u0.getNumObjs() === 1);
        }

        // Arithmetic instructions
        else if (instr instanceof JSArithInstr)
        {
            var u0 = useTypes[0];
            var u1 = useTypes[1];

            arithInt.count(u0.flags === TypeFlags.INT && u1.flags === TypeFlags.INT);
        }

        // Comparison instructions
        else if (instr instanceof JSCompInstr)
        {
            var u0 = useTypes[0];
            var u1 = useTypes[1];

            cmpInt.count(u0.flags === TypeFlags.INT && u1.flags === TypeFlags.INT);
        }

        // If instruction
        else if (instr instanceof IfInstr)
        {
            var u0 = useTypes[0];
            var u1 = useTypes[1];

            branchKnown.count(
                (u0.flags === TypeFlags.TRUE || u0.flags === TypeFlags.FALSE) &&
                (u1.flags === TypeFlags.TRUE || u1.flags === TypeFlags.FALSE)
            );

            if ((u0.flags === TypeFlags.TRUE || u0.flags === TypeFlags.FALSE) &&
                (u1.flags === TypeFlags.TRUE || u1.flags === TypeFlags.FALSE))
            {
                print('Known branch: ' + u0 + ' (' + instr.parentBlock.parentCFG.ownerFunc.funcName + ')');
                print(instr.parentBlock);
                print('');
            }
        }
    }

    function visitFunc(irFunc)
    {
        // If this is library code, skip it
        if (ta.fromLib(irFunc.hirCFG.entry) === true)
            return;

        // Visit the sub-functions
        irFunc.childFuncs.forEach(visitFunc);

        // For each block of the function
        for (var i = 0; i < irFunc.hirCFG.blocks.length; ++i)
        {
            var block = irFunc.hirCFG.blocks[i];

            // If the block wasn't visited, skip it
            if (ta.blockVisited(block) === false)
                continue;

            // For each instruction of the block
            for (var j = 0; j < block.instrs.length; ++j)
            {
                var instr = block.instrs[j];

                // If this instruction wasn't visited, skip it
                var outType = ta.getTypeSet(instr);
                if (outType === null)
                    continue;

                accumStats(instr);
            }
        }
    }

    // Visit all analyzed units
    this.allUnits.forEach(visitFunc);

    return stats;
}

/**
Evaluate type assertions, throw an exception if any fail
*/
TypeAnalysis.prototype.evalTypeAsserts = function ()
{
    var ta = this;

    function fail(test, set, msg)
    {
        var error = Error(
            'Type assertion failed: ' + msg + '\n' +
            'set : ' + set + '\n' +
            'test: ' + test
        );

        throw error;
        //print(error);
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

    function evalAssert(testStr, set)
    {
        var testExpr = JSON.parse(testStr);

        if (set === null)
            fail(testExpr, set, 'type assertion not visited');

        // Evaluate the expression
        var r = evalExpr(testExpr, set);

        if (r !== true)
            fail(testExpr, set, 'test failed');
    }

    function visitFunc(irFunc)
    {
        // Visit the sub-functions
        irFunc.childFuncs.forEach(visitFunc);

        // For each block of the function
        for (var i = 0; i < irFunc.hirCFG.blocks.length; ++i)
        {
            var block = irFunc.hirCFG.blocks[i];

            // For each instruction of the block
            for (var j = 0; j < block.instrs.length; ++j)
            {
                var instr = block.instrs[j];

                // If this is a type assertion
                if (isTypeAssert(instr) === true)
                {
                    var test = instr.uses[3].value;
                    var typeSet = ta.getTypeSet(instr, 2);

                    // Evaluate the type assertion on the type set
                    evalAssert(test, typeSet);
                }
            }
        }
    }

    // Visit all analyzed units
    this.allUnits.forEach(visitFunc);
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

