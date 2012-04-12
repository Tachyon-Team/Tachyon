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

Algorithm:
- blockWorkList, instrWorkList

- Queue entry block
  - SPSTFBlock gets constructed until end or first call instr
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
  - Need to tell the ArgValInstr what type sets (values) to fetch
  - Special method for this *****
- Same for returns, tell the calls instrs what type sets to fetch
  - Special method for this *****
*/




/**
@class Basic block stump
*/
function SPSTFStump(block, instrIdx)
{
    if (instrIdx === undefined)
        instrIdx = 0;

    this.block = block;

    this.instrIdx = instrIdx;
}

/**
Hash function for block descriptors
*/
SPSTFStump.hashFn = function (s)
{
    return defHashFunc(s.block) + s.instrIdx;
}

/**
Equality function for block descriptors
*/
SPSTFStump.equalFn = function (s1, s2)
{
    return s1.block === s2.block && s1.instrIdx === s2.instrIdx;
}

/**
@class Function representation for the SPSTF analysis
*/
function SPSTFFunc(irFunc)
{
    assert (
        irFunc instanceof IRFunction,
        'invalid function object'
    );

    /**
    Original IR function
    */
    this.irFunc = irFunc;

    /**
    Entry block
    */
    this.entry = undefined;

    /**
    Next unit in the chain, for unit-level functions
    */
    this.nextUnit = undefined;
}

/**
@class Basic block representation for the SPSTF analysis
*/
function SPSTFBlock(irBlock, instrIdx, func)
{
    assert (
        irBlock instanceof BasicBlock,
        'invalid block object'
    );

    assert (
        isNonNegInt(instrIdx) === true,
        'invalid instr index'
    );

    assert (
        func instanceof SPSTFFunc,
        'invalid function object'
    );

    this.irBlock = irBlock;

    this.instrIdx = instrIdx;

    this.func = func;

    this.preds = [];

    this.instrs = [];
}

/**
@class Instruction representation for the SPSTF analysis
*/
function SPSTFInstr(irInstr, instrIdx, block)
{
    assert (
        irInstr instanceof IRInstr,
        'invalid instr object'
    );

    assert (
        isNonNegInt(instrIdx) === true,
        'invalid instr index'
    );

    assert (
        block instanceof SPSTFBlock,
        'invalid block object'
    );


    this.irInstr = irInstr;


    this.block = block;


    this.targets = undefined;








    /**
    Flow function for this instruction
    */
    this.flowFunc = irInstr.spstfFlowFunc;







    // If the instruction has targets
    if (irInstr.targets !== undefined)
    {
        this.targets = new Array(irInstr.targets.length);
    
        for (var i = 0; i < this.targets.length; ++i)
            this.targets[i] = new SPSTFStump(irInstr.targets[i]);
    }

    // If this is a call instruction with no explicit targets
    else if (irInstr instanceof JSCallInstr || irInstr instanceof JSNewInstr)
    {
        this.targets = [new SPSTFStump(irInstr.parentBlock, instrIdx + 1)];
    }






    /* 
    TODO: special mapping for argument type sets, map to first input values
    - makes search faster

    // List of input values
    this.inVals = []

    // List of source objects
    {
        val

        srcs: []

            instr
            targetIdx
            outIdx
    }

    */






    // TODO: issue
    // Sometimes, need different outputs for different targets*****
    // - identify the targets by index
    // TODO: special mapping for output type set






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

    /**
    Map of block stumps to basic block representations
    */
    this.blockMap = new HashMap(SPSTFStump.hashFn, SPSTFStump.equalFn);




    /**
    Total number of type flow edges
    */
    this.edgeCount = 0;







    /**
    Ordered list of unit-level functions (function objects) to be analyzed
    */
    this.unitList = [];


    // TODO
    // TODO: handle type assertions
    // TODO


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
Queue an instruction for (re-)analysis
*/
SPSTF.prototype.queueInstr = function (instr)
{
    assert (
        instr instanceof SPSTFInstr,
        'invalid instruction object'
    );

    // TODO: test if the work set helps the performance at all
    // in a relatively large benchmark
    if (this.workSet.has(instr) === true)
        return;

    this.workList.addLast(instr);

    this.workSet.add(instr);
}

/**
Queue a block to be analyzed
*/
SPSTF.prototype.queueBlock = function (stump, func)
{
    if (instrIdx === undefined)
        instrIdx = 0;

    // Check if a representation has already been created for this block
    var block = this.blockMap.get(stump);

    // If no representation has yet been created
    if (block === HashMap.NOT_FOUND)
    {
        // Construct the block representation
        var block = SPSTFBlock(stump.block, stump.instrIdx, func);

        // For each instruction
        for (var i = 0; i < irBlock.instrs.length; ++i)
        {
            var irInstr = irBlock.instrs[i];

            // Create the instruction object and add it to the block
            var instr = new SPSTFInstr(irInstr, i, block);
            block.instrs.push(instr);

            // If this is a call/new instruction, stop. Remaining
            // instructions will be in a continuation block.
            if (irInstr instanceof JSCallInstr || irInstr instanceof JSNewInstr)
                break;

            // Queue the instruction for analysis;
            this.queueInstr(instr);
        }
    }

    // Return block representation
    return block;
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

    // Construct function representation
    var func = new SPSTFFunc(irFunc, 0, func);

    // Queue the function's entry block
    var entry = this.queueBlock(new SPSTFStump(irFunc.hirCFG.entry), func);

    // Set the function entry block
    func.entry = entry;

    // Return the function representation
    return func;
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

    // Construct the function object and queue it for analysis
    var func = this.queueFunc(ir);

    // If this is not the first unit
    if (this.unitList.length > 0)
    {
        // Set the next unit of the previous unit
        var prevUnit = this.unitList[this.unitList.length - 1];
        prevUnit.nextUnit = func;
    }

    // Add the unit to the list of units
    this.unitList.push(func);
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

//=============================================================================
//
// Per-instruction flow/transfer functions
//
//=============================================================================

IRInstr.prototype.spstfFlowFunc = function (ta)
{
    // TODO
}












