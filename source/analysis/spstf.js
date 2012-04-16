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

Phi instrs must make sure to have in-edges for all reachable *edges*
  - Edges, not preds!


How do we deal with function calls and returns? **********
- Probably want SPSTFFunc object, so we can think in terms of call graphs
- Type flow edges for ArgValInstr go to corresponding definitions directly???
  - Need to tell the ArgValInstr what type sets (values) to fetch
  - Special method for this *****
- Same for returns, tell the calls instrs what type sets to fetch
  - Special method for this *****


TODO: Need to handle value outputs we can't find.
- Map them in the type analysis, in a hash map



TODO: Object does not exist in one branch, how do we keep track of this? How do we merge?
- If you can't reach a prop type set on a given path, the object doesn't exist on that path
  - Resolves to initDefs, gives empty set
- Objects need to create undefined type sets for their properties at the creation site******



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

    /**
    Original IRInstr instance
    */
    this.irInstr = irInstr;

    /**
    SPSTFBlock instance this instruction belongs to
    */
    this.block = block;

    /**
    Flow function for this instruction
    */
    this.flowFunc = irInstr.spstfFlowFunc;

    /**
    List of target blocks
    */
    this.targets = undefined;

    /**
    Input value list.
    List of objects specifying the value and sources.
        {
            value,
            srcs: []
            {
                instr
                targetIdx
                outIdx
            }
        }
    */
    this.inVals = [];

    /**
    Output value list.
    List of lists (one per target) of objects specifying
    the defined value, type and destinations.
        {
            value,
            type,
            dests: []
        }
    */
    this.outVals = undefined;

    // If the instruction has targets
    if (irInstr.targets.length > 0)
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

    // There are no targets
    else
    {
        this.targets = [];
    }

    // Create the per-branch definitions list
    this.outVals = new Array((this.targets.length > 0)? this.targets.length:1);
    for (var i = 0; i < this.outVals.length; ++i)
        this.outVals[i] = [];
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
    // Clear the object map
    TGObject.objMap.clear();

    // Clear the closure cell map
    TGClosCell.cellMap.clear();

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
    Map of values to objects specifying the defined type and destinations.
        {
            type,
            dests: []
        }
    */
    this.initDefs = new HashMap();

    /**
    Global object node
    */
    this.globalObj = this.newObject(
        undefined,
        'global', 
        this.objProto, 
        undefined,
        undefined, 
        true
    );

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
    // Check if a representation has already been created for this block
    var block = this.blockMap.get(stump);

    // If no representation has yet been created
    if (block === HashMap.NOT_FOUND)
    {
        // Construct the block representation
        var block = new SPSTFBlock(stump.block, stump.instrIdx, func);

        // For each instruction
        for (var i = stump.instrIdx; i < stump.block.instrs.length; ++i)
        {
            var irInstr = stump.block.instrs[i];

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

    // Remove an instruction from the work list
    var instr = this.workList.remFirst();
    this.workSet.rem(instr);

    // Call the flow function for ths instruction
    instr.flowFunc(this);
}

/**
Create a new object abstraction
*/
SPSTF.prototype.newObject = function (
    instr,
    origin, 
    protoSet, 
    flags, 
    numClosVars, 
    singleton
)
{   
    // By default, the prototype is null
    if (protoSet === undefined)
        protoSet = TypeSet.null;

    // By default, this is a regular object
    if (flags === undefined)
        flags = TypeFlags.OBJECT;

    // By default, no closure variables
    if (numClosVars === undefined)
        numClosVars = 0;

    // By default, not singleton
    if (singleton === undefined)
        singleton = false;

    assert (
        protoSet.flags !== TypeFlags.EMPTY,
        'invalid proto set flags'
    );

    var obj = new TGObject(
        origin,
        flags, 
        numClosVars,
        singleton
    );

    // Set the prototype set for the object
    this.setType(instr, obj.proto, protoSet);

    return new TypeSet(
        flags, 
        undefined, 
        undefined, 
        undefined, 
        obj
    );
}

/**
Get the type set for a value
*/
SPSTF.prototype.getType = function (instr, value)
{
    assert (
        instr instanceof SPSTFInstr,
        'invalid instruction object'
    );

    // If this is a constant
    if (value instanceof IRConst)
    {
        // Create a type set for the constant
        return TypeSet.constant(value);
    }


    // TODO: test if value is already resolved


    /*
    TODO
    TODO
    TODO: for phi nodes, do we need to keep track of which pred values come from?

    Not necessary?

    Phi takes multiple values as input, one per edge. Only resolve values from
    reachable preds.

    Should also ignore unreachable preds in resolution process.
    - Can encode this in the normal resolution algorithm

    If a block becomes reachable, new definitions can be made
    - These can invalidate pred edges as normal?
    - BUT, there were no edges going through the new block before...
      - Which pred will we fall onto?

    Musn't forget, if a value doesn't dominate a successor, it can only be used
    by a phi node. If a value is used by a non-phi, it must be reachable along all paths******



    */






}

/**
Set the type set for a value
*/
SPSTF.prototype.setType = function (instr, value, type, targetIdx)
{
    if (targetIdx === undefined)
        targetIdx = 0;

    assert (
        instr === undefined || instr instanceof SPSTFInstr,
        'invalid instruction object'
    );

    assert (
        value instanceof SPSTFInstr ||
        value instanceof TGProperty ||
        value instanceof TGVariable
    );

    assert (
        targetIdx === 0 || targetIdx < instr.targets.length,
        'invalid target index'
    );

    // If this is an initial definition
    if (instr === undefined)
    {
        // Lookup the value in the initial definitions
        var def = this.initDefs.get(value);

        if (def === HashMap.NOT_FOUND)
        {
            var def = {
                type: undefined,
                dests: []
            }

            this.initDefs.set(value, def);
        }

        def.type = type;

        // Queue all destinations of this definition
        for (var i = 0; i < def.dests.length; ++i)
            this.queueInstr(def.dests[i]);

        return;
    }

    // Get the list of definitions for this target
    var defList = instr.outVals[targetIdx];

    var def = undefined;

    // Try to find the definition for this value in the list
    for (var j = 0; j < defList.length; ++j)        
    {
        if (defList[j].value === value)
        {
            def = defList[j];
            break;
        }
    }

    // If the definition was found
    if (def !== undefined)
    {
        // If the type hasn't changed, do nothing
        if (def.type.equal(type) === true)
            return;

        // Update the definition type
        def.type = type;

        // Queue all the destination instructions
        for (i = 0; i < def.dests.length; ++i)
            this.queueInstr(def.dests[i]);
    }
    else
    {
        // Resolve the predecessors for this value
        this.getType(instr, value);


        // TODO
        // TODO: loop through the predecessors, remove the pred-succ edges (both sides)
        // TODO: decrement edge count
        // TODO




        // Create a new definition object
        var def = {
            value: value,
            type: type,
            dests: []
        }

        // Add the new definition to the list for this target
        defList.push(def);
    }
}

/**
Get the type for an instruction input (use) value
*/
SPSTF.prototype.getInType = function (instr, useIdx)
{
    assert (
        useIdx < instr.irInstr.uses.length,
        'invalid use index'
    );

    return this.getType(instr, instr.irInstr.uses[useIdx]);
}

/**
Set an instruction output type
*/
SPSTF.prototype.setOutType = function (instr, normalType, exceptType)
{
    if (exceptType === undefined)
        exceptType = normalType;

    this.setType(instr, instr, normalType, 0);

    if (instr.targets.length > 0)
        this.setType(instr, instr, exceptType, 1);
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

JSAddInstr.prototype.spstfFlowFunc = function (ta)
{
    var t0 = ta.getInType(this, 0);
    var t1 = ta.getInType(this, 1);


    // TODO










    /*
    var t0 = typeGraph.getType(this.uses[0]);
    var t1 = typeGraph.getType(this.uses[1]);

    // Output type
    var outType;

    if (t0.flags === TypeFlags.INT && t1.flags === TypeFlags.INT)
    {
        var minVal = t0.rangeMin + t1.rangeMin;

        var maxVal = t0.rangeMax + t1.rangeMax;

        outType = new TypeSet(
            TypeFlags.INT,
            minVal,
            maxVal
        );
    }

    // TODO: addition of string + int, etc., string conversion
    else if (t0.flags === TypeFlags.STRING || t1.flags === TypeFlags.STRING)
    {
        var t0Str = t0.strVal;
        var t1Str = t1.strVal;

        var newStr = (t0Str && t1Str)? (t0Str + t1Str):undefined;

        outType = new TypeSet(
            TypeFlags.STRING,
            undefined,
            undefined,
            newStr
        );
    }

    // If the values are either int or string
    else if ((t0.flags & ~(TypeFlags.STRING | TypeFlags.INT)) === 0 &&
             (t1.flags & ~(TypeFlags.STRING | TypeFlags.INT)) === 0)
    {
        // The output is either int or string
        outType = new TypeSet(TypeFlags.INT | TypeFlags.STRING)
    }

    // If neither values can be string or object
    else if ((t0.flags & (TypeFlags.STRING | TypeFlags.EXTOBJ)) === 0 &&
             (t1.flags & (TypeFlags.STRING | TypeFlags.EXTOBJ)) === 0)
    {
        // The result can only be int or float
        outType = new TypeSet(TypeFlags.INT | TypeFlags.FLOAT);
    }

    // By default
    else
    {
        // The result can be int or float or string
        outType = new TypeSet(TypeFlags.INT | TypeFlags.FLOAT | TypeFlags.STRING);
    }

    ta.setOutput(typeGraph, this, outType);
    */
}

GlobalObjInstr.prototype.spstfFlowFunc = function (ta)
{
    // This refers to the global object
    ta.setOutType(this, ta.globalObj);
}

PutPropInstr.prototype.spstfFlowFunc = function (ta)
{
    var objType  = ta.getInType(this, 0);
    var nameType = ta.getInType(this, 1);
    var valType  = ta.getInType(this, 2);



    // TODO
}

GetPropInstr.prototype.spstfFlowFunc = function (ta)
{




    // TODO
}

