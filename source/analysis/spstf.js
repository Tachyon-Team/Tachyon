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
Sparse Path-Sensitive Type Flow (SPSTF) analysis implementation.

@author
Maxime Chevalier-Boisvert
*/







/*
TODO
Try to minimize hash table lookups



SPSTFFunc
- func, original func
- entry

SPSTFBlock
- func, owner function
- block, original block,
- instrIdx, start instruction
- preds
- instrs

SPSTFInstr
- inputs (type sets -> incoming edges)
  - List of variables, list of lists of incoming edges (instrs)
  - inVars, inEdges
  - No hash table lookups***

- outputs (type sets -> outgoing edges)
- instr, original instruction
- flow function



Algorithm:
- blockWorkList, instrWorkList

- Queue entry block
  - SPSTFBlock gets constructed until end or first call
  - Construct SPSTFInstr nodes for block instructions
  - Make stumps for branch targets (or resolve if existing)
  - Queue the block instructions for evaluation

- Phi instrs must make sure to have in-edges for all reachable *edges*
  - Edges, not preds!

- Do we really need to queue blocks? Can probably just construct their
  representation immediately! ***






Methods:
SPSTF.queueBlock
- Constructs block representation



How do we deal with function calls and returns? **********
- Probably want SPSTFFunc object, so we can think in terms of call graphs
- Type flow edges for ArgValInstr go to corresponding definitions directly???
  - Need to tell the ArgValInstr what type sets to fetch
  - Special method for this *****
- Same for returns, tell the calls instrs what type sets to fetch
  - Special method for this *****




*/







/**
TODO
*/
function SPSTFFunc()
{
    // TODO



    this.nextUnit = undefined;
}

/**
TODO
*/
function SPSTFBlock()
{
    // TODO
}

/**
TODO
*/
function SPSTFInstr()
{
    // TODO
}






/**
@class Sparse Path-Sensitive Type Flow (SPSTF) Analysis
@extends TypeAnalysis
*/
function SPSTF()
{
    // Initialize the type analysis
    this.init();
}
SPSTF.prototype = new TypeAnalysis();

/**
Initialize/reset the analysis
*/
SPSTF.prototype.init = function ()
{

    /**
    Worklist of instructions queued to be analyzed
    */
    this.workList = new LinkedList();

    /**
    Set of instructions in the work list
    This is to avoid adding instructions to the work list twice
    */
    this.workSet = new HashSet();



    // TODO
    // TODO
    // TODO



    /**
    Ordered list of unit-level functions (function objects) to be analyzed
    */
    this.unitList = [];






    /**
    Total analysis iteration count
    */
    this.itrCount = 0;

    /**
    Total analysis time
    */
    this.totalTime = 0;
}

/**
Dump information gathered about functions during analysis
*/
SPSTF.prototype.dumpFunctions = function ()
{
    // TODO
}

/**
Dump information gathered about objects during analysis
*/
SPSTF.prototype.dumpObjects = function ()
{
    // TODO
}

/**
Compute statistics about type sets
*/
SPSTF.prototype.compTypeStats = function ()
{
    // TODO
}

/**
Evaluate type assertions, throw an exception if any fail
*/
SPSTF.prototype.evalTypeAsserts = function ()
{
    // For now, do nothing. The type assertions are representative
    // of the capabilities of the type propagation analysis.
    // TODO
}

/**
Queue a block to be (re)analyzed
*/
SPSTF.prototype.queueBlock = function (blockDesc)
{
    // TODO: construct block representation if it doesn't exist
    
    // TODO: queue block instructions

    /*
    // If the block is already queued, do nothing
    if (this.workSet.has(blockDesc) === true)
        return;

    this.workList.addLast(blockDesc);

    this.workSet.add(blockDesc);
    */
}

/**
Queue a function to be analyzed
*/
SPSTF.prototype.queueFunc = function (irFunc)
{
    assert (
        irFunc instanceof IRFunction,
        'expected IR function'
    );

    // Get the function's entry block
    var entry = irFunc.hirCFG.entry;

    // TODO
    // Queue the function's entry block
    //this.queueBlock(new BlockDesc(entry));
}

/**
Add a code unit to the analysis
*/
SPSTF.prototype.addUnit = function (ir)
{
    assert (
        ir.astNode instanceof Program,
        'IR object is not unit-level function'
    );

    // TODO: construct function object

    /*
    // Get the info object for this function
    var funcInfo = this.getFuncInfo(ir);

    assert (
        this.unitList.indexOf(funcInfo) === -1,
        'unit already added'
    );

    // If this is the first unit
    if (this.unitList.length === 0)
    {
        // Set the type graph at the unit entry
        var entry = funcInfo.entry;
        this.setTypeGraph(new BlockDesc(entry), this.initGraph);

        // Queue the unit function to be analyzed
        this.queueFunc(ir);
    }
    else
    {
        // Set the next unit of the previous unit
        var prevUnit = this.unitList[this.unitList.length - 1];
        prevUnit.nextUnit = funcInfo;
    }

    // Add the unit to the list of units
    this.unitList.push(funcInfo);
    */
}

/**
Run the analysis until fixed-point or until the maximum number of
iterations is reached
*/
SPSTF.prototype.run = function (maxItrs)
{
    // Start timing the analysis
    var startTimeMs = (new Date()).getTime();

    // Until the max iteration count is reached
    for (var numItrs = 0; maxItrs === undefined || numItrs < maxItrs; ++numItrs)
    {
        // If the work list is empty, stop
        if (this.workList.isEmpty() === true)
            break;

        // Run one analysis iteration
        this.iterate();
    }

    // Stop the timing
    var endTimeMs = (new Date()).getTime();
    var time = (endTimeMs - startTimeMs) / 1000;

    // Update the total iteration count
    this.itrCount += numItrs;

    // Update the total analysis time
    this.totalTime += time;

    // Return the number of iterations performed
    return numItrs;
}

/**
Run one analysis iteration
*/
SPSTF.prototype.iterate = function ()
{
    assert (
        this.workList.isEmpty() === false,
            'empty work list'
    );

    // TODO
}

