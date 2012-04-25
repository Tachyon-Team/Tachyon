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

TODO: Object does not exist in one branch, how do we keep track of this? How do we merge?
- If you can't reach a prop type set on a given path, the object doesn't exist on that path
  - Resolves to initDefs, gives empty set
- Objects need to create undefined type sets for their properties at the creation site******
*/


/**
@class Set of live value uses
*/
function SPSTFUseSet()
{
    HashSet.call(this);
}

SPSTFUseSet.prototype.union = function (that)
{
    if (this === that)
        return this;

    var newSet = this.copy();

    return HashSet.prototype.union.call(newSet, that);
}

SPSTFUseSet.prototype.add = function (use)
{
    var newSet = this.copy();

    HashSet.prototype.add.call(newSet, use);

    return newSet;
}

/**
Empty use set
*/
SPSTFUseSet.empty = new SPSTFUseSet();

/**
@class Map of live values to their future uses
*/
function SPSTFLiveMap()
{
    HashMap.call(this, undefined, undefined, 3);
}

SPSTFLiveMap.prototype.copy = function ()
{
    var newMap = new SPSTFLiveMap();

    for (var itr = this.getItr(); itr.valid(); itr.next())
    {
        var pair = itr.get();
        var val = pair.key;
        var set = pair.value;

        // Use sets do copy on write and so they are not copied here
        newMap.set(val, set);
    }

    return newMap;
}

SPSTFLiveMap.prototype.equal = function (that)
{
    if (this.length !== that.length)
        return false;

    for (var itr = this.getItr(); itr.valid(); itr.next())
    {
        var pair = itr.get();
        var val = pair.key;
        var setA = pair.value;

        var setB = that.get(val);

        if (setA.equal(setB) === false)
            return false;
    }

    return true;
}

SPSTFLiveMap.prototype.union = function (that)
{
    var newMap = this.copy();

    for (var itr = that.getItr(); itr.valid(); itr.next())
    {
        var pair = itr.get();
        var val = pair.key;
        var setB = pair.value;

        var setA = this.get(val);

        // Union the use sets
        var newSet = setA.union(setB);

        newMap.set(val, newSet);
    }

    return newMap;
}

/**
Add a use for a given value
*/
SPSTFLiveMap.prototype.addLive = function (value, use)
{
    var origSet = this.get(value);

    var newSet = origSet.add(use);

    this.set(value, newSet);
}

/**
Kill the uses for a given value
*/
SPSTFLiveMap.prototype.killLive = function (value)
{
    this.rem(value);
}

/**
Get the use set for a given value
*/
SPSTFLiveMap.prototype.get = function (value)
{
    var set = HashMap.prototype.get.call(this, value);

    if (set === HashMap.NOT_FOUND)
        return SPSTFUseSet.empty;

    return set;
}

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
        irFunc === null || irFunc instanceof IRFunction,
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
    List of return successor blocks
    */
    this.retSuccs = [];
}

/**
@class Basic block representation for the SPSTF analysis
*/
function SPSTFBlock(irBlock, instrIdx, func)
{
    assert (
        irBlock === null || irBlock instanceof BasicBlock,
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

    /**
    Map of live values to uses at the beginning of the block
    */
    this.liveMap = new SPSTFLiveMap();
}

SPSTFBlock.prototype.getBlockName = function ()
{
    if (this.irBlock instanceof BasicBlock)
        return this.irBlock.getBlockName();

    return 'null block';
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

    // Create a meta-unit to hold initial definitions and unit calls
    var metaUnit = new SPSTFFunc(null);
    var initEntry = new SPSTFBlock(null, 0, metaUnit)
    metaUnit.entry = initEntry;

    // Create the instruction holding the initial definitions
    var initInstr = new SPSTFInstr(new RetInstr(), 0, initEntry);
    initEntry.instrs.push(initInstr);

    /**
    Work list of instructions queued for type flow analysis
    */
    this.instrWorkList = new LinkedList();

    /**
    Set of instructions in the instruction work list
    This is to avoid adding instructions to the work list twice
    */
    this.instrWorkSet = new HashSet();

    /**
    Work list of blocks queued for live value analysis
    */
    this.blockWorkList = new LinkedList();

    /**
    Set of block in the block work list
    This is to avoid adding blocks to the work list twice
    */
    this.blockWorkSet = new HashSet();

    /**
    Map of block stumps to basic block representations
    */
    this.blockMap = new HashMap(SPSTFStump.hashFn, SPSTFStump.equalFn);

    /**
    Total number of type flow edges
    */
    this.edgeCount = 0;

    /**
    Meta-unit holding the initial instruction and unit calls
    */
    this.metaUnit = metaUnit;

    /**
    Pseudo-instruction holding initial definitions
    */
    this.initInstr = initInstr;

    /**
    Global object node
    */
    this.globalObj = this.newObject(
        initInstr,
        'global', 
        this.objProto, 
        undefined,
        undefined, 
        true
    );

    // TODO
    // TODO: handle type assertions
    // TODO

    /**
    Instruction iteration count
    */
    this.instrItrCount = 0;

    /**
    Block iteration count
    */
    this.blockItrCount = 0;

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
Get the SPSTFBlock instance for a given block stump
*/
SPSTF.prototype.getBlock = function (stump, func)
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
Queue an instruction for type flow analysis
*/
SPSTF.prototype.queueInstr = function (instr)
{
    assert (
        instr instanceof SPSTFInstr,
        'invalid instruction object'
    );

    // TODO: test if the work set helps the performance at all
    // in a relatively large benchmark
    if (this.instrWorkSet.has(instr) === true)
        return;

    this.instrWorkList.addLast(instr);
    this.instrWorkSet.add(instr);
}

/**
Queue a block for live value analysis
*/
SPSTF.prototype.queueBlock = function (block)
{
    assert (
        block instanceof SPSTFBlock,
        'invalid block object'
    );

    if (this.blockWorkSet.has(block) === true)
        return;

    this.blockWorkList.addLast(block);
    this.blockWorkSet.add(block);

    print('block queued: ' + block.getBlockName());
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
    var entry = this.getBlock(new SPSTFStump(irFunc.hirCFG.entry), func);

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

    // Add a call to the function in the meta-unit
    var callInstr = new SPSTFInstr(
        new CallFuncInstr(
            func.irFunc,
            IRConst.getConst(undefined),
            IRConst.getConst(undefined)
        ),
        0,
        this.metaUnit.entry
    );
    this.metaUnit.entry.instrs.push(callInstr);
}

/**
Run the analysis until fixed-point is reached
*/
SPSTF.prototype.run = function ()
{
    // Start timing the analysis
    var startTimeMs = (new Date()).getTime();

    // Until a fixed point is reached
    for (;;)
    {
        if (this.instrWorkList.isEmpty() === false)
        {
            // Run one type flow analysis iteration
            this.instrItr();
        }

        else if (this.blockWorkList.isEmpty() === false)
        {
            // Run one live value analysis iteration
            this.blockItr();
        }

        // Both work lists are empty
        else
        {
            break;
        }
    }

    // Stop the timing
    var endTimeMs = (new Date()).getTime();
    var time = (endTimeMs - startTimeMs) / 1000;

    // Update the total analysis time
    this.totalTime += time;
}

/**
Run one type flow analysis iteration
*/
SPSTF.prototype.instrItr = function ()
{
    assert (
        this.instrWorkList.isEmpty() === false,
            'empty work list'
    );

    // Remove an instruction from the work list
    var instr = this.instrWorkList.remFirst();
    this.instrWorkSet.rem(instr);

    // Call the flow function for ths instruction
    instr.flowFunc(this);

    // Increment the instruction iteration count
    this.instrItrCount++;
}

/**
Run one live value analysis iteration
*/
SPSTF.prototype.blockItr = function ()
{
    assert (
        this.blockWorkList.isEmpty() === false,
            'empty work list'
    );

    // Remove a block from the work list
    var block = this.blockWorkList.remFirst();
    this.blockWorkSet.rem(block);

    print('iterating block: ' + block.getBlockName());




    // TODO: merge live values coming from successors
    // ISSUE: last instr kills different values depending on target we came from

    // TODO: want field SPSTFBlock.liveMap, storing live values at the end


    var branchInstr = block.instrs[block.instrs.length-1];


    var targets = block.instrs[block.instrs.length-1].targets;



    // TODO: function call and return successors
    // Can we unify this somehow? ********************




    var liveMap = new SPSTFLiveMap();








    // For each instruction, in reverse order
    for (var i = block.instrs.length - 1; i >= 0; --i)
    {
        var instr = block.instrs[i];

        /* TODO

        Definitions kill live values

        Uses generate live values

        New live value uses reaching a definition creates a new edge and re-queueing

        Live value uses can also be removed, causing edges to be removed
        */









    }

    // If the live map at the beginning of the block changed
    if (liveMap.equal(block.liveMap) === false)
    {
        block.liveMap = liveMap;

        // TODO: queue predecessors






    }

    // Increment the block iteration count
    this.blockItrCount++;
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

    // Try to find the use for this value in the list
    var use = undefined;
    for (var i = 0; i < instr.inVals.length; ++i)
    {
        if (instr.inVals[i].value === value)
        {
            use = instr.inVals[i];
            break;
        }
    }

    // If this is a new use, create the use object
    if (use === undefined)
    {
        var use = {
            value: value,
            srcs: []
        };

        instr.inVals.push(use);

        // Queue this instruction's block for live value analysis
        this.queueBlock(instr.block);
    }

    // Value type
    var type = TypeSet.empty;

    // For each source
    for (var i = 0; i < use.srcs.length; ++i)
    {
        var src = use.srcs[i];

        if (src === null)
            continue;

        var outVal = src.instr.outVals[targetIdx][outIdx];

        type = type.union(outVal.type);
    }

    return type;
}

/**
Set the type set for a value
*/
SPSTF.prototype.setType = function (instr, value, type, targetIdx)
{
    if (targetIdx === undefined)
        targetIdx = 0;

    assert (
        instr instanceof SPSTFInstr,
        'invalid instruction object'
    );

    assert (
        value instanceof IRInstr ||
        value instanceof TGProperty ||
        value instanceof TGVariable
    );

    assert (
        targetIdx === 0 || targetIdx < instr.targets.length,
        'invalid target index'
    );

    // Get the list of definitions for this target
    var defList = instr.outVals[targetIdx];

    // Try to find the definition for this value in the list
    var def = undefined;
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

    // This is a new definition for this instruction
    else
    {
        // Create a new definition object
        var def = {
            value: value,
            type: type,
            dests: []
        }

        // Add the new definition to the list for this target
        defList.push(def);

        // Queue this instruction's block for live value analysis
        this.queueBlock(instr.block);
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

    this.setType(instr, instr.irInstr, normalType, 0);

    if (instr.targets.length > 0)
        this.setType(instr, instr.irInstr, exceptType, 1);
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

