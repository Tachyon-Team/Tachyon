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

/**
@class Set of live value uses
*/
function SPSTFUseSet()
{
    HashSet.call(this, undefined, undefined, 3);
}
SPSTFUseSet.prototype = Object.create(HashSet.prototype);

SPSTFUseSet.prototype.toString = function ()
{
    var str = '{';

    for (var itr = this.getItr(); itr.valid(); itr.next())
    {
        var use = itr.get();

        if (str !== '{')
            str += ',';

        str += use.instr.getValName();
    }

    return str + '}';
}

SPSTFUseSet.prototype.union = function (that)
{
    if (this === that)
        return this;
    if (this === SPSTFUseSet.empty)
        return that;
    if (that === SPSTFUseSet.empty)
        return this;

    var newSet = this.copy();

    // Add the elements from the second set
    for (var itr = that.getItr(); itr.valid(); itr.next())
        HashSet.prototype.add.call(newSet, itr.get());

    assert (
        newSet.length >= this.length && newSet.length >= that.length,
        'incorrect use set union result'
    );

    return newSet;
}

SPSTFUseSet.prototype.equal = function (that)
{
    if (this === that)
        return true;
    if (this.length !== that.length)
        return false;

    return HashSet.prototype.equal.call(this, that);
}

SPSTFUseSet.prototype.add = function (use)
{
    var newSet = this.copy();

    HashSet.prototype.add.call(newSet, use);

    return newSet;
}

SPSTFUseSet.prototype.rem = function (use)
{
    var newSet = this.copy();

    HashSet.prototype.rem.call(newSet, use);

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
SPSTFLiveMap.prototype = Object.create(HashMap.prototype);

SPSTFLiveMap.prototype.toString = function ()
{
    var str = '';

    for (var itr = this.getItr(); itr.valid(); itr.next())
    {
        var pair = itr.get();
        var value = pair.key;
        var useSet = pair.value;

        if (str !== '')
            str += '\n';

        str += (value.getValName? value.getValName():value) + ' => ' + useSet;
    }

    return str;
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
    assert (
        block instanceof BasicBlock,
        'invalid basic block'
    );

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

    // Get the number of named parameters
    var numParams = irFunc? (irFunc.argVars.length + 2):0;

    // Create the argument values
    var argVals = new Array(numParams);
    for (var argIndex = 0; argIndex < argVals.length; ++argIndex)
    {
        var argVal = new TGVariable('arg' + argIndex, this);
        argVal.argIndex = argIndex;
        argVals[argIndex] = argVal;
    }

    // Create the indexed argument value
    var idxArgVal = new TGVariable('idxArg', this);

    /**
    Original IR function
    */
    this.irFunc = irFunc;

    /**
    Entry block
    */
    this.entry = undefined;

    /**
    List of return blocks
    */
    this.retBlocks = [];

    /**
    List of call sites (SPSTFInstr instances)
    */
    this.callSites = [];

    /**
    List of argument values
    */
    this.argVals = argVals;

    /**
    Indexed argument value (for argument object)
    */
    this.idxArgVal = idxArgVal;

    /**
    List of global values defined in this function or callees
    */
    this.defSet = new HashSet();

    /**
    Flag indicating this function has been called in a normal call
    */
    this.normalCall = false;

    /**
    Flag indicating this function has been called as a constructor
    */
    this.ctorCall = false;
}

SPSTFFunc.prototype.getName = function ()
{
    if (this.irFunc instanceof IRFunction)
        return this.irFunc.funcName;

    return 'null function';
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
        func === null || func instanceof SPSTFFunc,
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

SPSTFBlock.prototype.getName = function ()
{
    var blockName = 
        (this.irBlock instanceof BasicBlock)?
        this.irBlock.getBlockName():
        'null block';

    return blockName + '(' + this.instrIdx + ')';
}

SPSTFBlock.prototype.toString = function ()
{
    var str = '';

    str += this.getName() + ':';

    for (var i = 0; i < this.instrs.length; ++i)
    {
        if (str !== '')
            str += '\n';

        str += this.instrs[i];
    }

    return str;
}

SPSTFBlock.prototype.getBranch = function()
{
    return this.instrs[this.instrs.length-1];
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
            type,
            srcs: [],
            numRems,
            instr
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

    // If this is a call instruction
    if (irInstr instanceof JSCallInstr || irInstr instanceof JSNewInstr)
    {
        // Add a field for the list of callees
        this.callees = [];
    }

    // If the instruction has targets
    if (irInstr.targets.length > 0)
    {
        this.targets = new Array(irInstr.targets.length);

        for (var i = 0; i < this.targets.length; ++i)
            this.targets[i] = new SPSTFStump(irInstr.targets[i]);
    }

    // If this is a call instruction with no explicit targets
    else if ((irInstr instanceof JSCallInstr || irInstr instanceof JSNewInstr) &&
             irInstr.parentBlock instanceof BasicBlock)
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
Get a string representation of this instruction
*/
SPSTFInstr.prototype.toString = function ()
{
    return this.irInstr.toString();
}

/**
Get the value name for this instruction
*/
SPSTFInstr.prototype.getValName = function ()
{
    return this.irInstr.getValName();
}

/**
@class Sparse Path-Sensitive Type Flow (SPSTF) Analysis
@extends TypeAnalysis
*/
function SPSTF()
{
    // Initialize the type analysis
    this.init({});
}
SPSTF.prototype = new TypeAnalysis();

/**
Maximum number of edge removals for a given use
*/
SPSTF.MAX_EDGE_REMS = 10;

/**
Initialize/reset the analysis
*/
SPSTF.prototype.init = function (options)
{
    /**
    Option to disable edge removal during analysis
    */
    this.noEdgeRem = Boolean(options['noedgerem']);

    // Clear the object map
    TGObject.objMap.clear();

    // Clear the closure cell map
    TGClosCell.cellMap.clear();

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
    Map of block stumps to basic block representations
    */
    this.blockMap = new HashMap(SPSTFStump.hashFn, SPSTFStump.equalFn);

    /**
    Map of IR instructions to instruction representations
    */
    this.instrMap = new HashMap();

    /**
    Map of IR functions to function representations
    */
    this.funcMap = new HashMap();

    /**
    Total number of type flow edges
    */
    this.numEdges = 0;

    // Create a meta-unit to hold initial definitions and unit calls
    var metaUnit = new SPSTFFunc(null);
    var metaEntry = new SPSTFBlock(null, 0, metaUnit)
    metaUnit.entry = metaEntry;

    // Create the global initialization instruction
    var initInstr = this.makeInstr(
        new SPSTFInitInstr(),
        metaEntry
    );

    // Create a jump to the next meta-block
    this.makeInstr(Object.create(JumpInstr.prototype), metaEntry);
    
    /**
    Meta-unit holding the initial instruction and unit calls
    */
    this.metaUnit = metaUnit;

    /**
    Pseudo-instruction holding initial definitions
    */
    this.initInstr = initInstr;

    /**
    Last meta-unit block added
    */
    this.lastMetaBlock = metaEntry;

    /**
    Object prototype object node
    */
    this.objProto = undefined;

    /**
    Array prototype object node
    */
    this.arrProto = undefined;

    /**
    Function prototype object node
    */
    this.funcProto = undefined;

    /**
    Boolean prototype object node
    */
    this.boolProto = undefined;

    /**
    Number prototype object node
    */
    this.numProto = undefined;

    /**
    String prototype object node
    */
    this.strProto = undefined;

    /**
    Global object node
    */
    this.globalObj = undefined;

    /**
    Map of hash sets for instruction uses/outputs, for gathering statistics
    */
    this.typeSets = new HashMap(
        function (use)
        {
            return defHashFunc(use.instr) + ((use.idx !== undefined)? use.idx:7);
        },
        function (use1, use2)
        {
            return use1.instr === use2.instr && use1.idx === use2.idx;
        }
    );

    /**
    Instruction iteration count
    */
    this.itrCount = 0;

    /**
    Block iteration count
    */
    this.blockItrCount = 0;

    /**
    Total analysis time
    */
    this.totalTime = 0;

    // Queue the global initialization instruction
    this.queueInstr(initInstr);
}

/**
Client API function to test if a basic block was visited by the analysis.
@returns a boolean value
*/
SPSTF.prototype.blockVisited = function (irBlock)
{
    assert (
        irBlock instanceof BasicBlock,
        'invalid basic block'
    );

    var block = this.blockMap.get(new SPSTFStump(irBlock, 0));
    var visited = (block !== HashMap.NOT_FOUND);

    return visited;
}

/**
Client API function to get the type set associated with an IR instruction or
one of its inputs.
@returns a type set object or null if unavailable
*/
SPSTF.prototype.getTypeSet = function (irInstr, useIdx)
{
    assert (
        irInstr instanceof IRInstr,
        'invalid IR instruction'
    );

    assert (
        useIdx === undefined || useIdx < irInstr.uses.length,
        'invalid use index'
    );

    // Find the instruction in the instruction map    
    var instr = this.instrMap.get(irInstr);

    // If the instruction wasn't analyzed, no info available
    if (instr === HashMap.NOT_FOUND)
        return null;

    if (useIdx !== undefined)
    {
        // Get the input type
        var typeSet = this.getInType(instr, useIdx);
    }
    else
    {
        // Store the type set of the output value   
        var outVals = instr.outVals[0];
        var typeSet = TypeSet.empty;
        for (var i = 0; i < outVals.length; ++i)
        {
            var def = outVals[i];

            if (def.value === irInstr)
            {
                //print('  output: ' + def.type);
                typeSet = def.type;
                break;
            }
        }
    }

    return typeSet;
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
Create an SPSTFInstr instance and add it to a block
*/
SPSTF.prototype.makeInstr = function (irInstr, block, instrIdx)
{
    if (instrIdx === undefined)
        instrIdx = block.instrs.length;

    var instr = new SPSTFInstr(irInstr, instrIdx, block);

    // Add the instruction to the block
    block.instrs.push(instr);

    // Store the object in the instruction map
    this.instrMap.set(irInstr, instr);

    // Queue the instruction for analysis
    this.queueInstr(instr);

    return instr;
}

/**
Get the SPSTFBlock instance for a given block stump
*/
SPSTF.prototype.getBlock = function (stump, func)
{
    assert (
        func === null || func instanceof SPSTFFunc,
        'invalid function object'
    );

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
            var instr = this.makeInstr(irInstr, block, i);

            // If this is a call/new instruction, stop. Remaining
            // instructions will be in a continuation block.
            if (irInstr instanceof JSCallInstr || irInstr instanceof JSNewInstr)
                break;

            // If this is a return instruction
            if (irInstr instanceof RetInstr)
            {
                // Add this block to the list of return blocks for the function
                func.retBlocks.push(block);
            }
        }

        // Add the block to the block map
        this.blockMap.set(stump, block);
    }

    // Return block representation
    return block;
}


var postOrderNo = 0;


/**
Get the SPSTFBlock instance for a given IR function
*/
SPSTF.prototype.getFunc = function (irFunc)
{

    function compPostOrder(irFunc)
    {
        var stack = [irFunc.hirCFG.entry];

        while (stack.length > 0)
        {
            var node = stack.pop();

            // If this node was never visited
            if (node.orderNo === undefined)
            {
                node.orderNo = 0;
                stack.push(node);
            }
            else
            {
                node.orderNo = ++postOrderNo;
                continue;
            }

            // Push the successors
            for (var i = 0; i < node.succs.length; ++i)
            {
                var succ = node.succs[i];
                if (succ.orderNo === undefined)
                    stack.push(succ);
            }
        }
    }




    assert (
        irFunc instanceof IRFunction,
        'expected IR function'
    );

    // Check if a representation has already been created for this function
    var func = this.funcMap.get(irFunc);

    // If no representation has yet been created
    if (func === HashMap.NOT_FOUND)
    {

        compPostOrder(irFunc);


        // Construct function representation
        var func = new SPSTFFunc(irFunc, 0, func);

        // Queue the function's entry block
        var entry = this.getBlock(new SPSTFStump(irFunc.hirCFG.entry), func);

        // Create the function entry pseudo-instruction and move it
        // at the start of the block
        var entryInstr = this.makeInstr(
            new SPSTFEntryInstr(),
            entry
        );
        entry.instrs.pop();
        entry.instrs.unshift(entryInstr);

        // Set the function entry block
        func.entry = entry;

        // Add the function to the function map
        this.funcMap.set(irFunc, func);
    }

    // Return the function representation
    return func;
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
Queue a block and value for live value analysis
*/
SPSTF.prototype.queueBlock = function (block, value)
{
    assert (
        block instanceof SPSTFBlock,
        'invalid block object'
    );

    assert (
        value !== undefined,
        'invalid value'
    );


    
 
    /*
    var newItem = { block:block, value:value };

    for (var itr = this.blockWorkList.getItr(); itr.valid(); itr.next())
    {
        var item = itr.get();

        if (item.block.irBlock === null)
            continue;

        var curNo = item.block.irBlock.orderNo;

        if (!block.irBlock || block.irBlock.orderNo < curNo)
        {
            //print('adding before');

            this.blockWorkList.addBefore(newItem, itr);
            return;
        }
    }

    this.blockWorkList.addLast(newItem);
    */

    
    this.blockWorkList.addLast({ block:block, value:value });
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
    var func = this.getFunc(ir);

    // Create a new block in the meta-unit to call into the new unit
    var callBlock = new SPSTFBlock(null, 0, this.metaUnit);

    // Create a closure for the function in the meta-unit
    var makeClosInstr = this.makeInstr(
        new CallFuncInstr(
            config.clientParams.staticEnv.getBinding('makeClos'),
            IRConst.getConst(undefined),
            IRConst.getConst(undefined),
            func.irFunc,
            IRConst.getConst(0, IRType.pint)
        ),
        callBlock
    );

    // Call the function in the meta-unit
    var callInstr = this.makeInstr(
        new JSCallInstr(
            makeClosInstr.irInstr,
            IRConst.getConst(undefined)
        ),
        callBlock
    );

    // Make the last meta-unit block branch to the new block
    var lastBlock = this.lastMetaBlock;
    var lastBranch = lastBlock.getBranch();
    lastBranch.targets = [callBlock];
    callBlock.preds = [lastBlock];

    // The new block is the last meta-unit block
    this.lastMetaBlock = callBlock;    
}

/**
Run the analysis until fixed-point is reached
*/
SPSTF.prototype.run = function ()
{
    //startV8Profile();

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

    assert (
        this.instrWorkList.isEmpty() === true &&
        this.blockWorkList.isEmpty() === true,
        'non-empty work list'
    );

    // Stop the timing
    var endTimeMs = (new Date()).getTime();
    var time = (endTimeMs - startTimeMs) / 1000;

    // Update the total analysis time
    this.totalTime += time;

    //stopV8Profile();
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

    /*    
    print(
        'Iterating instr: ' + 
        (instr.irInstr.parentBlock? instr.irInstr:null)
    );
    */

    /*
    if (instr.count === undefined)
        instr.count = 0;

    instr.count++;

    //print(instr.count);

    if (instr.count > 5)
        print(instr + ' : ' + instr.count);
    */    

    // Call the flow function for this instruction
    instr.flowFunc(this);

    /*
    var outType = this.getOutType(instr);
    if (outType.flags === TypeFlags.ANY)
    {
        print('instr produces any: ' + instr);

        //print('  ' + instr.block);

        for (var i = 0; i < instr.irInstr.uses.length; ++i)
            print('  ' + this.getInType(instr, i));
    }
    */

    // Increment the instruction iteration count
    this.itrCount++;

    //print('instr itr: ' + this.itrCount);
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
    var item = this.blockWorkList.remFirst();
    var block = item.block;
    var value = item.value;

    //print(
    //    'Iterating block: ' + block.getName() /*+
    //    ((block === block.func.entry)? (' (' + block.func.getName() + ')'):'')*/
    //);
    /*
    print(
        'Iterating block/value: ' + block.getName() + ' / ' + value
    );
    */

    var that = this;

    /**
    Process definitions for an instruction
    */
    function processDefs(instr, value, useSet, targetIdx)
    {
        // Get the definitions for this target
        var outVals = instr.outVals[targetIdx];

        // For each definition of the instruction
        for (var outIdx = 0; outIdx < outVals.length; ++outIdx)
        {
            var def = outVals[outIdx];

            // If this is not the value we want, skip it
            if (def.value !== value)
                continue;

            var dests = def.dests;

            /*
            // For each current dest of this definition
            for (var i = 0; i < dests.length; ++i)
            {
                var use = dests[i];

                // If the use is no longer present, remove the type flow edge
                if (useSet.has(use) === false)
                    that.remEdge(def, use);
            }
            */

            // For each use in the incoming use set
            for (var itr = useSet.getItr(); itr.valid(); itr.next())
            {
                var dest = itr.get();

                // If this is a new use, add a new type flow edge
                if (arraySetHas(dests, dest) === false)
                    that.addEdge(def, dest);
            }

            // Definitions kill live values
            useSet = SPSTFUseSet.empty;
        }

        return useSet;
    }

    /**
    Process uses for an instruction
    */
    function processUses(instr, value, useSet)
    {
        // For each use of the instruction
        for (var i = 0; i < instr.inVals.length; ++i)
        {
            var inVal = instr.inVals[i];

            // If this is not a use of our value, skip it
            if (inVal.value !== value)
                continue;

            // Uses generate live values
            useSet = useSet.add(inVal);

            // Stop, there is at most one use per value
            break;
        }

        return useSet;
    }

    /**
    Filter incoming phi uses not meant for this block
    */
    function filterPhis(useSet, pred, succ, value)
    {
        // For each use in the set
        for (var itr = useSet.getItr(); itr.valid(); itr.next())
        {
            var use = itr.get();
            var instr = use.instr;
            var irInstr = instr.irInstr;

            // If this is not a phi node from the next block, skip it
            if (instr.block !== succ || (irInstr instanceof PhiInstr) === false)
                continue;

            var predIdx = irInstr.preds.indexOf(pred.irBlock);

            // If the value is not meant for this block,
            // remove the use associated with this phi node
            if (predIdx === -1 || value !== irInstr.uses[predIdx])
                useSet = useSet.rem(use);
        }

        return useSet;
    }

    // Use set to be propagated through this block
    var useSet = SPSTFUseSet.empty;

    // Get the branch instruction for this block
    var branch = block.getBranch();

    // Test if the value is global
    var isGlobal = (value instanceof TGVariable);

    // If the branch is a return instruction
    if (branch.irInstr instanceof RetInstr)
    {
        var callSites = branch.block.func.callSites;

        // For each call site of this function
        for (var i = 0; i < callSites.length; ++i)
        {
            var callSite = callSites[i];
            var callCont = callSite.targets[0];

            if ((callCont instanceof SPSTFBlock) === false)
                continue;

            // If the value is global and defined in this function or
            // if it this call's return value
            if ((isGlobal === true && block.func.defSet.has(value) === true) ||
                value === callSite.irInstr)
            {
                var succSet = callCont.liveMap.get(value);
                var succSet = filterPhis(succSet, block, callCont, value);
                useSet = useSet.union(succSet);                
            }
        }

        // Process the definitions
        useSet = processDefs(branch, value, useSet, 0);
    }

    // If the branch is a call instruction
    else if (branch.irInstr instanceof JSCallInstr ||
             branch.irInstr instanceof JSNewInstr)
    {
        // For each callee entry block
        for (var i = 0; i < branch.callees.length; ++i)
        {
            var entry = branch.callees[i].entry;

            // If the value is global
            if (isGlobal === true)
            {
                var succSet = entry.liveMap.get(value);
                useSet = useSet.union(succSet);
            }
        }

        var callCont = branch.targets[0];

        // If the call continuation was visited
        if (callCont instanceof SPSTFBlock)
        {
            // Test if any callee defines this value
            var calleeDef = false;
            CALLEE_DEF_LOOP:
            for (var i = 0; i < branch.callees.length; ++i)
            {
                if (branch.callees[i].defSet.has(value) === true)
                {
                    calleeDef = true;
                    break CALLEE_DEF_LOOP;
                }
            }

            // Test if the value is this call's return value
            var isCallRet = (value === branch.irInstr);

            // If there are no callees
            // or the value is not global
            // or no callee defines this value
            // and this is not this call's return value
            if ((branch.callees.length === 0 ||
                 isGlobal === false ||
                 calleeDef === false) &&
                isCallRet === false)
            {
                var succSet = callCont.liveMap.get(value);
                succSet = filterPhis(succSet, block, callCont, value);
                useSet = useSet.union(succSet);
            }
        }

        // Process the definitions along the normal target (kills)
        useSet = processDefs(branch, value, useSet, 0);
    }

    // Other kinds of branch instructions
    else
    {
        var targets = branch.targets;

        for (var targetIdx = 0; targetIdx < targets.length; ++targetIdx)
        {
            var target = targets[targetIdx];

            if (target instanceof SPSTFStump)
                continue;

            var succSet = target.liveMap.get(value);

            succSet = filterPhis(succSet, block, target, value);

            // Process the definitions along the target (kills)
            succSet = processDefs(branch, value, succSet, targetIdx);

            useSet = useSet.union(succSet);
        }
    }

    // Process uses for branch instruction
    useSet = processUses(branch, value, useSet);

    // For each instruction except the branch, in reverse order
    for (var i = block.instrs.length - 2; i >= 0; --i)
    {
        var instr = block.instrs[i];

        // Process defs of the instruction
        useSet = processDefs(instr, value, useSet, 0);

        // Process uses of the instruction
        useSet = processUses(instr, value, useSet);
    }

    // Get the live set at the beginning of the block
    var outSet = block.liveMap.get(value);

    // If the set at the beginning of the block changed
    if (useSet.equal(outSet) === false)
    {
        //print('live map changed, queueing preds');

        // Update the live set for the value
        block.liveMap.set(value, useSet);

        // If this is a function entry block
        if (block === block.func.entry)
        {
            var func = block.func;

            // Queue the call site blocks
            for (var i = 0; i < func.callSites.length; ++i)
            {
                var callSite = func.callSites[i];
                this.queueBlock(callSite.block, value);
            }
        }
        else
        {
            // For each predecessor block
            for (var i = 0; i < block.preds.length; ++i)
            {
                var pred = block.preds[i];

                // Queue the predecessors
                this.queueBlock(pred, value);

                // Get the branch instruction of the predecessor block
                var branch = pred.getBranch();

                //print('queuing pred ending in: ' + branch);

                // If the predecessor is a call site
                if (branch.irInstr instanceof JSCallInstr || 
                    branch.irInstr instanceof JSNewInstr)
                {
                    // Queue all callee return sites
                    for (var j = 0; j < branch.callees.length; ++j)
                    {
                        var callee = branch.callees[j];
                        for (var k = 0; k < callee.retBlocks.length; ++k)
                            this.queueBlock(callee.retBlocks[k], value);
                    }
                }
            }
        }
    }

    // Increment the block iteration count
    this.blockItrCount++;

    //print('block itr: ' + this.blockItrCount);
}

/**
Create a new object abstraction
*/
SPSTF.prototype.newObject = function (
    instr,
    tag, 
    func,
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

    // Ensure that the prototype set is not empty
    assert (
        protoSet.flags !== TypeFlags.EMPTY,
        'invalid proto set flags'
    );

    /**
    Translate recent object references into summary object references
    */
    function translRefs(inType)
    {
        // If the value type doesn't contain the recent object
        if (inType.hasObj(recentObj) === false)
            return inType;

        var outType = new TypeSet(
            inType.flags,
            inType.rangeMin,
            inType.rangeMax,
            inType.strVal
        );

        // Replace occurrences of the recent object by the summary object
        for (var objItr = inType.getObjItr(); objItr.valid(); objItr.next())
        {
            var obj = objItr.get();

            if (obj === recentObj)
                obj = summaryObj;

            outType.addObj(obj);
        }

        return outType
    }

    // Create the recent object and mark it as a singleton
    var recentObj = new TGObject(
        instr,
        tag + '_r',
        func,
        flags,
        numClosVars,
        true
    );
    this.setType(instr, recentObj.proto, protoSet);

    // Initialize recent object properties to missing
    // This is so non-existent properties show as undefined
    for (propName in recentObj.props)
    {
        var rcntProp = recentObj.getPropNode(propName);
        if (this.hasOutDef(instr, rcntProp) === false)
            this.setType(instr, rcntProp, TypeSet.missing);
    }

    // If this is not a known singleton object
    if (singleton !== true)
    {
        // Create the summary object
        var summaryObj = new TGObject(
            instr,
            tag + '_s',
            func,
            flags,
            numClosVars,
            false
        );
        this.setType(instr, summaryObj.proto, protoSet);

        // For each named property of the recent object
        for (propName in recentObj.props)
        {
            var rcntProp = recentObj.getPropNode(propName);
            var summProp = summaryObj.getPropNode(propName);

            // Union recent type into summary prop type,
            // translating the recent type
            var rcntType = translRefs(this.getType(instr, rcntProp));
            var summType = this.getType(instr, summProp);
            this.setType(instr, summProp, summType.union(rcntType));
        }

        // Union the recent indexed property type into the
        // summary type, translating the recent type
        var rcntIdxType = translRefs(this.getType(instr, recentObj.idxProp));
        var summIdxType = this.getType(instr, summaryObj.idxProp);
        this.setType(instr, summaryObj.idxProp, summIdxType.union(rcntIdxType));

        // Create a set on the instruction for values possibly having
        // the recent object in their type set
        if (instr.recentVals === undefined)
            instr.recentVals = new HashSet();

        // For each value possibly using the recent object
        for (var itr = instr.recentVals.getItr(); itr.valid(); itr.next())
        {
            var value = itr.get();

            // Translate recent object references
            var inType = this.getType(instr, value);
            var outType = translRefs(inType);
            this.setType(instr, value, outType);
        }
    }

    // Return a type set containing only the recent object
    return new TypeSet(
        flags,
        undefined,
        undefined,
        undefined,
        recentObj
    );
}

/**
Add a value to a function's definition set. This may update
caller functions recursively.
*/
SPSTF.prototype.addFuncDef = function (func, value)
{
    // If this is not a global value, stop
    if ((value.parent instanceof TGObject) === false &&
        (value.parent instanceof TGClosCell) === false)
        return;

    var workList = [func];

    while (workList.length > 0)
    {
        var func = workList.pop();

        if (func.defSet.has(value) === true)
            continue;

        // Add the value to the function's definition set
        func.defSet.add(value);

        // Queue the return site blocks
        for (var i = 0; i < func.retBlocks.length; ++i)
            this.queueBlock(func.retBlocks[i], value);

        // Queue the call site blocks
        for (var i = 0; i < func.callSites.length; ++i)
            this.queueBlock(func.callSites[i].block, value);

        for (var i = 0; i < func.callSites.length; ++i)
            workList.push(func.callSites[i].block.func);
    }
}

/**
Kill all uses of a given value in predecessors, starting from a specified block
*/
SPSTF.prototype.killUses = function (block, value)
{
    var workList = new LinkedList();

    workList.addLast(block);

    // Until the work list is empty
    while (workList.isEmpty() === false)
    {
        var block = workList.remFirst();

        // If the value is not live in this block, skip it
        if (block.liveMap.get(value).length === 0)
            continue;

        // Kill all live uses for this value
        block.liveMap.killLive(value);

        // If this is a function entry block
        if (block === block.func.entry)
        {
            var func = block.func;

            // Queue the call site blocks
            for (var i = 0; i < func.callSites.length; ++i)
            {
                var callSite = func.callSites[i];
                workList.addLast(callSite.block);
            }
        }
        else
        {
            // For each predecessor block
            for (var i = 0; i < block.preds.length; ++i)
            {
                var pred = block.preds[i];

                // Queue the predecessor
                workList.addLast(pred);

                // Get the branch instruction of the predecessor block
                var branch = pred.getBranch();

                // If the predecessor is a call site
                if (branch.irInstr instanceof JSCallInstr || 
                    branch.irInstr instanceof JSNewInstr)
                {
                    // Queue all callee return sites
                    for (var j = 0; j < branch.callees.length; ++j)
                    {
                        var callee = branch.callees[j];
                        for (var k = 0; k < callee.retBlocks.length; ++k)
                            workList.addLast(callee.retBlocks[k]);
                    }
                }
            }
        }
    }
}

/**
Reset the outputs of an instruction and its subgraph of successors
*/
/*
SPSTF.prototype.resetInstr = function (use, visited)
{
    var workList = new LinkedList();

    visited = new HashSet();

    var ta = this;

    // Reset the definitions of an instruction
    function resetDefs(instr)
    {
        // For each target
        for (var targetIdx = 0; targetIdx < instr.outVals.length; ++targetIdx)
        {
            var defs = instr.outVals[targetIdx];

            // For each definition
            for (var defIdx = 0; defIdx < defs.length; ++defIdx)
            {
                var def = defs[defIdx];

                // If this is a property initialization, skip it
                if (def.value instanceof TGProperty &&
                    def.value.parent.origin === instr)
                    continue;

                // Reset the definition type
                def.type = TypeSet.empty;

                // Add all destinations to the work list
                for (var i = 0; i < def.dests.length; ++i)
                    workList.addLast(def.dests[i]);
            }
        }
    }

    // Reset a call instruction
    function resetCall(instr, use)
    {
        var irInstr = instr.irInstr;

        var useIndex;
        for (var i = 0; i < instr.inVals.length; ++i)
            if (instr.inVals[i].value === use.value)
                useIndex = i;

        if (useIndex === undefined)
            return;

        // Compute the call argument index
        var argIndex = (irInstr instanceof JSNewInstr)? (useIndex-1):useIndex;

        // For each target
        for (var targetIdx = 0; targetIdx < instr.outVals.length; ++targetIdx)
        {
            var defs = instr.outVals[targetIdx];

            // For each definition
            for (var defIdx = 0; defIdx < defs.length; ++defIdx)
            {
                var def = defs[defIdx];

                // If this is not the argument definition, skip it
                if (def.value.argIndex !== argIndex &&
                    def.value.name !== 'idxArg')
                    continue;

                // Reset the definition type
                def.type = TypeSet.empty;

                // Add all destinations to the work list
                for (var i = 0; i < def.dests.length; ++i)
                    workList.addLast(def.dests[i]);
            }
        }
    }

    // Add the use to the work list
    workList.addLast(use);

    // Until the work list is empty
    while (workList.isEmpty() === false)
    {
        var use = workList.remFirst();
        var instr = use.instr;
        var irInstr = instr.irInstr;

        // If this use was already visited, skip it
        if (visited.has(use) === true)
            continue;

        // Mark the use as visited
        visited.add(use);

        // If this is a call instruction
        if (irInstr instanceof JSCallInstr || 
            irInstr instanceof JSNewInstr)
        {
            resetCall(instr, use);
        }

        // Other kinds of instructions
        else
        {
            resetDefs(instr);
        }
    }

    // For each visited use
    for (var itr = visited.getItr(); itr.valid(); itr.next())
    {
        var use = itr.get();

        this.queueInstr(use.instr);

        // Don't remove objects from the set of objects touched by
        // property write instructions
        if (use.instr.irInstr instanceof PutPropInstr &&
            use.value === use.instr.irInstr.uses[0])
            continue;
        
        // Recompute the type for this use
        use.type = TypeSet.empty;
        for (var i = 0; i < use.srcs.length; ++i)
        {
            var src = use.srcs[i];
            use.type = use.type.union(src.type);
        }
    }
}
*/

/**
Add a def-use edge
*/
SPSTF.prototype.addEdge = function (
    def,
    use
)
{
    /*
    print('Adding edge');
    print('  val : ' + def.value);
    print('  from: ' + def.instr);
    print('  to  : ' + use.instr);
    print('  type: ' + def.type);
    */

    assert (
        def.value === use.value,
        'def-use value mismatch'
    );

    assert (
        use.instr instanceof SPSTFInstr,
        'invalid use object'
    );

    assert (
        arraySetHas(use.srcs, def) === false &&
        arraySetHas(def.dests, use) === false,
        'edges already present'
    );

    // Add mutual def-use edges
    def.dests.push(use);
    use.srcs.push(def);

    // Compute the updated use type
    var useType = use.type.union(def.type);

    // If the use type changed
    if (useType.equal(use.type) === false)
    {
        use.type = useType;

        this.queueInstr(use.instr);
    }

    this.numEdges++;
}

/**
Remove a def-use edge
*/
/*
SPSTF.prototype.remEdge = function (
    def,
    use
)
{
    // If edge removal is disabled, return
    if (this.noEdgeRem == true)
        return;

    //print('Removing edge');
    //print('  val : ' + def.value);
    //print('  from: ' + def.instr);
    //print('  to  : ' + use.instr);

    assert (
        def.value === use.value,
        'def-use value mismatch'
    );

    assert (
        use.instr instanceof SPSTFInstr,
        'invalid use object'
    );

    // If the maximum number of edge removals for the use was reached,
    // don't remove the edge
    if (use.numRems >= SPSTF.MAX_EDGE_REMS)
        return;

    // Increment the number of edge removals for the use
    use.numRems++;

    // If the maximum removal count is reached, print a warning
    if (use.numRems === SPSTF.MAX_EDGE_REMS)
        print('WARNING: max edge rem count reached');

    // Remove mutual def-use edges
    arraySetRem(def.dests, use);
    arraySetRem(use.srcs, def);

    // Compute the updated use type
    var useType = TypeSet.empty;
    for (var i = 0; i < use.srcs.length; ++i)
    {
        var srcType = use.srcs[i].type;
        useType = useType.union(srcType);
    }

    // If the use type changed
    if (useType.equal(use.type) === false)
    {
        //print('type changed');
        //print('  from: ' + use.type);
        //print('  to  : ' + useType);

        use.type = useType;

        // Reset the instruction's and its successors
        this.resetInstr(use);

        // Queue the instruction for analysis
        this.queueInstr(use.instr);
    }

    this.numEdges--;
}
*/

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

    // If this is an IR function (function pointer)
    else if (value instanceof IRFunction)
    {
        // Return the empty set type
        return TypeSet.empty;
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
            type: TypeSet.empty,
            srcs: [],
            numRems: 0,
            instr: instr
        };

        instr.inVals.push(use);

        // Queue this instruction's block for live value analysis
        this.queueBlock(instr.block, value);
    }

    // Return the current use type
    return use.type;
}

/**
Set the type set for a value
*/
SPSTF.prototype.setType = function (instr, value, type, targetIdx)
{
    //if (String(value) === 'k')
    //    print('setting type: ' + type);

    if (targetIdx === undefined)
        targetIdx = 0;

    assert (
        instr instanceof SPSTFInstr,
        'invalid instruction object'
    );

    assert (
        value instanceof IRInstr ||
        value instanceof TGProperty ||
        value instanceof TGVariable,
        'invalid value'
    );

    assert (
        type instanceof TypeSet,
        'invalid type'
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

    // If this is a new definition
    if (def === undefined)
    {
        // Create a new definition object
        var def = {
            value: value,
            type: TypeSet.empty,
            dests: [],
            instr: instr
        }

        // Add the new definition to the list for this target
        defList.push(def);

        // Add the definition to the function's definition set
        this.addFuncDef(instr.block.func, value);

        // Kill all uses of this value, starting from this block
        this.killUses(instr.block, value);

        // Queue this instruction's block for live value analysis
        this.queueBlock(instr.block, value);
    }

    // If the type hasn't changed, do nothing
    if (def.type.equal(type) === true)
        return;

    // Update the definition type
    def.type = type;

    // If the type set contains objects
    if ((type.flags & TypeFlags.EXTOBJ) !== 0 && type.getNumObjs() > 0)
    {
        // For each object
        for (var itr = type.getObjItr(); itr.valid(); itr.next())
        {
            var obj = itr.get();
            var origin = obj.origin;

            // If this is not a recent object, skip it
            if (obj.singleton === false)
                continue;

            // If the origin does not track recent values, skip it
            if (origin.recentVals === undefined)
                continue;
           
            // If this is a local variable from a different function
            // than the object's origin, skip it
            if (value instanceof IRValue && value.parentBlock instanceof BasicBlock &&
                value.parentBlock.parentCFG.ownerFunc !== origin.block.func.irFunc)
                continue;

            // If the origin already defines this value, skip it
            if (this.hasOutDef(origin, value) === true)
                continue;

            // Add the value to the list of recent values
            // and queue the object's origin instruction
            origin.recentVals.add(value);
            this.queueInstr(origin);
        }
    }

    // Queue all the destination instructions
    for (i = 0; i < def.dests.length; ++i)
    {
        var dest = def.dests[i];

        var newType = dest.type.union(type);

        // If the destination type changed
        if (newType.equal(dest.type) === false)
        {
            // Store the updated type
            dest.type = newType; 

            // Queue the destination instruction
            this.queueInstr(dest.instr);
        }
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

    if (instr.targets.length > 1)
        this.setType(instr, instr.irInstr, exceptType, 1);
}

/**
Set (restrict) an instruction input type
*/
SPSTF.prototype.setInType = function (instr, useIdx, normalType, exceptType)
{
    assert (
        useIdx < instr.irInstr.uses.length,
        'invalid use index'
    );

    if (exceptType === undefined)
        exceptType = normalType;

    var value = instr.irInstr.uses[useIdx];

    this.setType(instr, value, normalType, 0);

    if (instr.targets.length > 1)
        this.setType(instr, value, exceptType, 1);
}

/**
Test if an instruction defines an output value
*/
SPSTF.prototype.hasOutDef = function (instr, value, targetIdx)
{
    if (targetIdx === undefined)
        targetIdx = 0;

    var outVals = instr.outVals[targetIdx];
    for (var i = 0; i < outVals.length; ++i)
        if (outVals[i].value === value)
            return true;

    return false;
}

/**
Get the output type of an instruction
*/
SPSTF.prototype.getOutType = function (instr)
{
    var outVals = instr.outVals[0];

    for (var i = 0; i < outVals.length; ++i)
        if (outVals[i].value === instr.irInstr)
            return outVals[i].type;

    return TypeSet.empty;
}

/**
Mark a branch instruction's target block as reachable
*/
SPSTF.prototype.touchTarget = function (instr, targetIdx)
{
    assert (
        targetIdx < instr.targets.length,
        'invalid target'
    );
    
    var target = instr.targets[targetIdx];

    // If this target has not yet been visited
    if (target instanceof SPSTFStump)
    {
        // Get the predecessor block
        var pred = instr.block;

        // Create the basic block for this target
        var block = this.getBlock(target, pred.func);

        // Update the target to point to the block
        instr.targets[targetIdx] = block;

        // Add this the predecessor to the predecessors of the target
        arraySetAdd(block.preds, pred);

        // Queue the predecessor for all live values in the target
        for (var itr = block.liveMap.getItr(); itr.valid(); itr.next())
            this.queueBlock(pred, itr.get().key);

        // Get the branch instruction for this block
        var branch = block.getBranch();

        // If this is a return block
        if (branch.irInstr instanceof RetInstr)
        {
            var func = block.func;
            var callSites = func.callSites;

            // For each call site
            for (var i = 0; i < callSites.length; ++i)
            {
                var callCont = callSites[i].targets[0];

                if ((callCont instanceof SPSTFBlock) === false)
                    continue;

                // Queue the return block for all values live in the call continuation
                for (var liveItr = callCont.liveMap.getItr(); liveItr.valid(); liveItr.next())
                    this.queueBlock(block, liveItr.get().key);
            }
        }
    }
}

/**
Get the value node corresponding to a given object property
*/
SPSTF.prototype.getPropNode = function (obj, propName)
{
    // If the property node does not already exist
    if (obj.hasPropNode(propName) === false)
    {
        var propNode = obj.getPropNode(propName);

        var origInstr = obj.origin;

        assert (
            origInstr instanceof SPSTFInstr,
            'invalid origin instruction'
        );

        // Queue the creation site so that it will initialize
        // the new property to the missing type
        this.queueInstr(origInstr);

        //print('creating init def for: "' + propName + '"');
        //print('  orig instr: ' + origInstr);
    }

    return obj.getPropNode(propName);
}

/**
Perform a property lookup with recursive prototype chain search
*/
SPSTF.prototype.propLookup = function (instr, objType, nameType, visited)
{
    // Test if this is a bounded arguments object propery access
    function boundedArgsGet(instr)
    {
        function isArgObj(valDef)
        {
            return (
                valDef instanceof CallFuncInstr && 
                valDef.uses[0] instanceof IRFunction &&
                valDef.uses[0].funcName === 'makeArgObj'
            );
        }        

        function allDestsGet(valDef)
        {
            for (var i = 0; i < valDef.dests.length; ++i)
                if ((valDef.dests[i] instanceof GetPropInstr) === false)
                    return false;

            return true;
        }

        function idxBounded(valDef, idx, curBlock, depth)
        {
            if (depth >= 5)
                return false;

            if (curBlock.preds.length === 0)
                return false;

            // For each predecessor
            for (var i = 0; i < curBlock.preds.length; ++i)
            {
                var pred = curBlock.preds[i];

                var branch = pred.getLastInstr();

                if (branch instanceof IfInstr && 
                    branch.uses[1] === IRConst.getConst(true) &&
                    branch.uses[0] instanceof JSLtInstr &&
                    branch.uses[0].uses[0] === idx &&
                    branch.uses[0].uses[1] instanceof GetPropInstr &&
                    branch.uses[0].uses[1].uses[0] === valDef &&
                    branch.uses[0].uses[1].uses[1] === IRConst.getConst('length') &&
                    branch.targets[0] === curBlock)
                {
                    continue;
                }
                
                if (idxBounded(valDef, idx, pred, depth + 1) === false)
                    return false;
            }

            return true;
        }

        var valDef = instr.uses[0];
        var idx = instr.uses[1];
        var curBlock = instr.parentBlock;

        if (isArgObj(valDef) === true &&
            allDestsGet(valDef) === true && 
            idxBounded(valDef, idx, curBlock, 0) === true)
            return true;

        return false;
    }

    if (typeof nameType === 'string')
        nameType = TypeSet.constant(nameType);

    if (visited === undefined)
        visited = [];

    if (objType.flags === TypeFlags.ANY)
        throw '*WARNING: getProp base has any type';

    // Output type set
    var outType = TypeSet.empty;

    // If the object may be a number
    if (objType.flags & (TypeFlags.INT | TypeFlags.FLOAT))
    {
        // Lookup the property on the number prototype
        var protoProp = this.propLookup(instr, this.numProto, nameType, visited);
        outType = outType.union(protoProp);
    }

    // If the object may be a boolean
    if (objType.flags & (TypeFlags.TRUE | TypeFlags.FALSE))
    {
        // Lookup the property on the boolean prototype
        var protoProp = this.propLookup(instr, this.boolProto, nameType, visited);
        outType = outType.union(protoProp);
    }

    // If the object may be a string
    if (objType.flags & TypeFlags.STRING)
    {
        // If this may be the length property
        if (nameType.strVal === 'length')
        {
            outType = outType.union(TypeSet.posInt);
        }

        // If this may be any string property
        else if (nameType.flags & TypeFlags.STRING)
        {
            // Lookup the property on the string prototype
            var protoProp = this.propLookup(
                instr, 
                this.strProto, 
                nameType.restrict(TypeFlags.ANY & ~TypeFlags.INT & ~TypeFlags.FLOAT),
                visited
            );
            outType = outType.union(protoProp);
        }

        // If this may be an index property
        if (nameType.flags & (TypeFlags.INT | TypeFlags.FLOAT))
        {
            // This is a substring
            outType = outType.union(TypeSet.string);
        }
    }

    // For each possible object
    for (var objItr = objType.getObjItr(); objItr.valid(); objItr.next())
    {
        var obj = objItr.get();

        // If the object was already visited, skip it
        if (arraySetHas(visited, obj) === true)
            continue;

        // Mark the object as visited
        arraySetAdd(visited, obj);

        var propType = TypeSet.empty;

        // If the property name could be anything
        if ((nameType.flags & TypeFlags.STRING && !nameType.strVal) ||
            (nameType.flags & TypeFlags.OBJEXT) ||
            (nameType.flags & TypeFlags.NULL)   ||
            (nameType.flags & TypeFlags.UNDEF)  ||
            (nameType.flags & TypeFlags.TRUE)   ||
            (nameType.flags & TypeFlags.FALSE))
        {
            // Union all property types
            for (propName in obj.props)
            {
                var propNode = this.getPropNode(obj, propName);
                propType = propType.union(this.getType(instr, propNode));
            }
            propType = propType.union(this.getType(instr, obj.idxProp));
        }

        // The property type is not anything
        else
        {       
            // If this may the length property of an array
            if (obj.flags === TypeFlags.ARRAY && nameType.strVal === 'length')
            {
                propType = propType.union(TypeSet.posInt);
            }

            // If the property may be another string
            else if (nameType.flags & TypeFlags.STRING)
            {
                var propType = propType.union(
                    this.getType(
                        instr,
                        this.getPropNode(obj, nameType.strVal)
                    )
                );
            }

            // If the property may be a numeric
            if (nameType.flags & (TypeFlags.INT | TypeFlags.FLOAT))
            {
                var propType = propType.union(
                    this.getType(
                        instr,
                        obj.idxProp
                    )
                );

                // If this is an unbounded array access
                if (boundedArgsGet(instr.irInstr) === false)
                    propType = propType.union(TypeSet.missing);
            }
        }

        // If this property may be missing
        if (propType.flags & TypeFlags.MISSING)
        {
            // Get the type for the object's prototype
            var protoNode = obj.proto;
            var protoType = this.getType(instr, protoNode);

            // If the prototype is not necessarily null
            if (protoType.flags & ~TypeFlags.NULL)
            {
                // Do a recursive lookup on the prototype
                var protoProp = this.propLookup(instr, protoType, nameType, visited);

                // If we know for sure this property is missing
                if (propType.flags === TypeFlags.MISSING)
                {
                    // Take the prototype property type as-is
                    propType = protoProp;
                }
                else
                {
                    // Union the prototype property type
                    propType = propType.union(protoProp);
                }
            }

            // If the prototype may be null, add the undefined type
            if (protoType.flags & TypeFlags.NULL)
            {
                propType = propType.union(TypeSet.undef);
            }

            // Remove the missing flag from the property type
            propType = propType.restrict(propType.flags & (~TypeFlags.MISSING));
        }

        // Union the types for this property into the type set
        outType = outType.union(propType);
    }

    //print('depth: ' + depth);
    //print('out type: ' + outType);
    //print('');

    return outType;
}

/**
Implements function call semantics
*/
SPSTF.prototype.funcCall = function (
    callInstr,
    ctorCall,
    calleeType,
    thisType, 
    argTypes
)
{
    // If the callee could be any function
    if (calleeType.flags === TypeFlags.ANY)
    {
        //if (config.verbosity >= log.DEBUG)
            print('*WARNING: callee has type ' + calleeType);

        this.setOutType(callInstr, TypeSet.any);

        // No callees to analyze
        return;
    }

    // Get the call continuation block
    var callCont = callInstr.targets[0];

    // For each potential callee
    for (var itr = calleeType.getObjItr(); itr.valid(); itr.next())
    {
        var callee = itr.get();

        // If this is not a function, ignore it
        if ((callee.func instanceof IRFunction) === false)
            continue;

        // If we have a handler for this function
        if (ctorCall === false && callee.handler !== undefined)
        {
            // Call the handler function instead
            callee.handler.call(this, callInstr, callCont);
            continue;
        }

        // Get the SPSTFFunc instance for this value
        var irFunc = callee.func;
        var func = this.getFunc(irFunc);

        // If this function is a new callee
        if (arraySetHas(callInstr.callees, func) === false)
        {
            // Add the function to the callee set
            arraySetAdd(callInstr.callees, func);

            // Add this instruction to the set of callers of the function
            arraySetAdd(func.callSites, callInstr);

            // Queue the call site block for all live values at the function entry
            for (var liveItr = func.entry.liveMap.getItr(); liveItr.valid(); liveItr.next())
                this.queueBlock(callInstr.block, liveItr.get().key);

            // Queue the return blocks for all values live in the call continuation
            if (callCont instanceof SPSTFBlock)
            {
                for (var liveItr = callCont.liveMap.getItr(); liveItr.valid(); liveItr.next())
                {
                    var value = liveItr.get().key;
                    for (var i = 0; i < func.retBlocks.length; ++i)
                        this.queueBlock(func.retBlocks[i], value);
                }
            }

            // Queue the return instructions
            for (var i = 0; i < func.retBlocks.length; ++i)
            {
                var retInstr = func.retBlocks[i].getBranch();
                this.queueInstr(retInstr, value);
            }

            // Add the callee's definitions to the caller's definitions
            var caller = callInstr.block.func;
            for (var defItr = func.defSet.getItr(); defItr.valid(); defItr.next())
                this.addFuncDef(caller, defItr.get());

            // Set the call type flags
            if (ctorCall === true)
                func.ctorCall = true;
            else
                func.normalCall = true;
        }

        // For each argument used by the function
        for (var i = 0; i < func.argVals.length; ++i)
        {
            var argType;

            // Get the incoming type for this argument
            if (i === 0)
            {
                argType = new TypeSet(
                    TypeFlags.FUNCTION, 
                    undefined, 
                    undefined, 
                    undefined, 
                    callee
                );
            }
            else if (i === 1)
            {
                argType = thisType;
            }
            else
            {
                if (argTypes instanceof Array)
                    argType = (i-2 < argTypes.length)? argTypes[i-2]:TypeSet.undef;
                else
                    argType = argTypes;
            }

            // Set the type for this argument value
            this.setType(callInstr, func.argVals[i], argType);
        }

        // If the callee uses the arguments object
        if (irFunc.usesArguments === true)
        {
            // Indexed argument type
            var idxArgType = TypeSet.empty;

            // For each argument passed
            for (var i = 0; i < argTypes.length; ++i)
            {
                var argType = argTypes[i];
                idxArgType = idxArgType.union(argType);
            }

            // Set the indexed argument type
            this.setType(callInstr, func.idxArgVal, idxArgType)
        }
    }
}

//=============================================================================
//
// Library function handlers
//
//=============================================================================

/**
Map of library handler functions. Maps global object tags to maps from 
property names to handler functions.
*/
TypeProp.libHandlers = {
    func_proto_r: {}
};

/**
Handler for the Function.prototype.call function
*/
TypeProp.libHandlers.func_proto_r.call = function (callInstr, callCont)
{
    // JSCallInstr call func this arg*

    var calleeType = this.getInType(callInstr, 1);

    var thisType = this.getInType(callInstr, 2);

    // If "this" contains null or undef, replace by the global object
    if ((thisType.flags & (TypeFlags.UNDEF | TypeFlags.NULL)) !== 0)
    {
        thisType = thisType.restrict(
            TypeFlags.ANY & ~(TypeFlags.UNDEF | TypeFlags.NULL)
        );

        thisType = thisType.union(this.globalObj);
    }

    // Get the argument types
    var argTypes = [];
    for (var i = 3; i < callInstr.irInstr.uses.length; ++i)
    {
        var argType = this.getInType(callInstr, i);
        argTypes.push(argType);
    }

    // Perform the function call
    this.funcCall(
        callInstr,
        false,
        calleeType, 
        thisType, 
        argTypes
    );
}

/**
Handler for the Function.prototype.apply function
*/
TypeProp.libHandlers.func_proto_r.apply = function (callInstr, callCont)
{
    // JSCallInstr apply func this arg_array

    var calleeType = this.getInType(callInstr, 1);

    var thisType = this.getInType(callInstr, 2);

    // If "this" contains null or undef, replace by the global object
    if ((thisType.flags & (TypeFlags.UNDEF | TypeFlags.NULL)) !== 0)
    {
        thisType = thisType.restrict(
            TypeFlags.ANY & ~(TypeFlags.UNDEF | TypeFlags.NULL)
        );

        thisType = thisType.union(this.globalObj);
    }

    // Get the type for the argument array
    var arrayType = this.getInType(callInstr, 3);

    var argType = TypeSet.empty;

    // Union the array property types into the argument type
    for (var itr = arrayType.getObjItr(); itr.valid(); itr.next())
    {
        var obj = itr.get();

        if ((obj.flags & TypeFlags.ARRAY) !== 0)
        {
            var idxType = this.getType(callInstr, obj.idxProp);
            argType = argType.union(idxType);
        }
    }

    // If the array type may be null or undefined, add undefined
    if ((arrayType.flags & (TypeFlags.NULL | TypeFlags.UNDEF)) !== 0)
        argType = argType.union(TypeSet.undef);

    // Perform the function call
    this.funcCall(
        callInstr,
        false,
        calleeType, 
        thisType, 
        argType
    );
}

//=============================================================================
//
// Per-instruction flow/transfer functions
//
//=============================================================================

// Global initialization pseudo-instruction
function SPSTFInitInstr()
{
    this.mnemonic = 'init';
    this.type = IRType.none;
    this.uses = [];
    this.dests = [];
    this.targets = [];
}
SPSTFInitInstr.prototype = new IRInstr();

SPSTFInitInstr.prototype.spstfFlowFunc = function (ta)
{
    // Object prototype object node
    ta.objProto = ta.newObject(
        this,
        'obj_proto',
        undefined,
        undefined, 
        undefined, 
        undefined, 
        true
    );

    // Array prototype object node
    ta.arrProto = ta.newObject(
        this,
        'arr_proto',
        undefined,
        ta.objProto, 
        undefined, 
        undefined, 
        true
    );

    // Function prototype object node
    ta.funcProto = ta.newObject(
        this,
        'func_proto',
        undefined,
        ta.objProto,
        undefined, 
        undefined, 
        true
    );

    // Boolean prototype object node
    ta.boolProto = ta.newObject(
        this,
        'bool_proto',
        undefined,
        ta.objProto,
        undefined, 
        undefined, 
        true
    );

    // Number prototype object node
    ta.numProto = ta.newObject(
        this,
        'num_proto',
        undefined,
        ta.objProto,
        undefined, 
        undefined, 
        true
    );

    // String prototype object node
    ta.strProto = ta.newObject(
        this,
        'str_proto',
        undefined,
        ta.objProto,
        undefined, 
        undefined, 
        true
    );

    // Global object node
    ta.globalObj = ta.newObject(
        this,
        'global',
        undefined,
        ta.objProto, 
        undefined,
        undefined, 
        true
    );
}

// Default flow function
IRInstr.prototype.spstfFlowFunc = function (ta)
{
    // By default, do nothing
}

PhiInstr.prototype.spstfFlowFunc = function (ta)
{
    var outType = TypeSet.empty;

    // For each phi predecessor
    // Note: only reachable predecessors appear in this list
    for (var i = 0; i < this.irInstr.preds.length; ++i)
    {
        var pred = this.irInstr.preds[i];

        var val = this.irInstr.uses[i];
        var type = ta.getType(this, val);

        outType = outType.union(type);
    }

    ta.setOutType(this, outType);
}

GlobalObjInstr.prototype.spstfFlowFunc = function (ta)
{
    // This refers to the global object
    ta.setOutType(this, ta.globalObj);
}

InitGlobalInstr.prototype.spstfFlowFunc = function (ta)
{
    var propName = this.irInstr.uses[1].value;

    var globalObj = ta.globalObj.getObjItr().get();

    var propNode = ta.getPropNode(globalObj, propName);

    ta.setType(this, propNode, TypeSet.undef);
}

BlankObjInstr.prototype.spstfFlowFunc = function (ta)
{
    // Test if this is a global object (not in a loop)
    var isGlobal = false;
    var curBlock = this.irInstr.parentBlock;
    if (curBlock instanceof BasicBlock)
    {
        var curFunc = curBlock.parentCFG.ownerFunc;
        var curEntry = curFunc.hirCFG.entry;
        var isGlobal = curFunc.parentFunc === null && curBlock === curEntry;
    }

    // Create a new object from the object prototype
    var newObj = ta.newObject(
        this, 
        'obj', 
        undefined, 
        ta.objProto,
        undefined,
        undefined,
        isGlobal
    );

    // The result is the new object
    ta.setOutType(this, newObj);
}

BlankArrayInstr.prototype.spstfFlowFunc = function (ta)
{
    // Create a new array object from the array prototype
    var newObj = ta.newObject(this, 'array',  undefined, ta.arrProto, TypeFlags.ARRAY);

    // The result is the new object
    ta.setOutType(this, newObj);
}

HasPropInstr.prototype.spstfFlowFunc = function (ta)
{
    ta.setOutput(typeGraph, this, TypeSet.bool);

    ta.setOutType(this, TypeSet.boolj);
}

PutPropInstr.prototype.spstfFlowFunc = function (ta)
{
    var objType  = ta.getInType(this, 0);
    var nameType = ta.getInType(this, 1);
    var valType  = ta.getInType(this, 2);

    // Set the output type
    ta.setOutType(this, valType);

    // If either the base or name are undetermined, do nothing for now
    if (objType === TypeSet.empty || nameType === TypeSet.empty)
        return;

    // Test if there is a single, known object type
    var singleType = (
        objType.getNumObjs() === 1 && 
        (objType & ~TypeFlags.EXTOBJ) === 0
    );

    var instr = this;

    function updateProp(obj, propNode, valType)
    {
        // Test if we can overwrite the current property type
        var canAssignType = (
            singleType === true && 
            obj.singleton === true &&
            propNode !== obj.idxProp
        );

        // If we can do a strong update
        if (canAssignType === true)
        {
            //print('strong update');

            // Do a strong update on the property type
            ta.setType(instr, propNode, valType);
        }
        else
        {
            //print('weak update');

            // Union with the current property type
            var propType = ta.getType(instr, propNode);
            var newType = propType.union(valType);
            ta.setType(instr, propNode, newType);
        }
    }

    try
    {
        if (objType.flags === TypeFlags.ANY)
            throw '*WARNING: putProp on any type';

        if (nameType.flags === TypeFlags.ANY)
            throw '*WARNING: putProp with any name';

        // If the name type cannot be fully handled
        if (nameType.flags & TypeFlags.TRUE ||
            nameType.flags & TypeFlags.FALSE ||
            nameType.flags & TypeFlags.EXTOBJ ||
            ((nameType.flags & TypeFlags.STRING) && nameType.strVal === undefined))
            throw '*WARNING: putProp with unhandled property type: ' + nameType;

        // For each possible object
        for (var objItr = objType.getObjItr(); objItr.valid(); objItr.next())
        {
            var obj = objItr.get();

            // If this is a library function for which we have a handler
            if ((valType.flags & TypeFlags.FUNCTION) !== 0 &&
                valType.getNumObjs() === 1 &&
                nameType.flags === TypeFlags.STRING &&
                nameType.strVal !== undefined &&
                TypeProp.libHandlers.hasOwnProperty(obj.tag) === true &&
                TypeProp.libHandlers[obj.tag].hasOwnProperty(nameType.strVal) === true)
            {
                // Set the handler function for the value object
                var propName = nameType.strVal;
                var valObj = valType.getObjItr().get();
                valObj.handler = TypeProp.libHandlers[obj.tag][propName];
            }

            // If the name may be a specific string
            if ((nameType.flags & TypeFlags.STRING) && nameType.strVal !== undefined)
            {
                updateProp(
                    obj,
                    ta.getPropNode(obj, nameType.strVal),
                    valType
                );
            }

            // If the property may be numeric (indexed)
            if (nameType.flags & (TypeFlags.INT | TypeFlags.FLOAT))
            {
                // If writing to an array property, add the undefined type
                updateProp(
                    obj,
                    obj.idxProp,
                    valType.union(TypeSet.undef)
                );
            }

            // If the name may be the undefined value
            if (nameType.flags & TypeFlags.UNDEF)
            {
                updateProp(
                    obj,
                    ta.getPropNode(obj, "undefined"),
                    valType
                );
            }

            // If the name may be the null value
            if (nameType.flags & TypeFlags.NULL)
            {
                updateProp(
                    obj,
                    ta.getPropNode(obj, "null"),
                    valType
                );
            }
        }
    }

    // If an inference problem occurs
    catch (e)
    {
        if (e instanceof Error)
            throw e;

        print(e);
        //print(this);
    }

    // The object cannot be undefined or null along the normal branch
    var newObjType = objType.restrict(TypeFlags.ANY & ~(TypeFlags.UNDEF | TypeFlags.NULL));
    ta.setInType(this, 0, newObjType, objType);
}

GetPropInstr.prototype.spstfFlowFunc = function (ta)
{
    var objType = ta.getInType(this, 0);
    var nameType = ta.getInType(this, 1);

    // If either argument is undetermined, do nothing for now
    if (objType === TypeSet.empty || nameType === TypeSet.empty)
    {
        ta.setOutType(this, TypeSet.empty);
        return;
    }

    try
    {
        // Perform the property lookup
        var outType = ta.propLookup(this, objType, nameType);
        ta.setOutType(this, outType);
    }

    // If an inference problem occurs
    catch (e)
    {
        if (e instanceof Error)
            throw e;

        //if (config.verbosity >= log.DEBUG)
        {
            print(e);
            //print(this);
        }

        ta.setOutType(this, TypeSet.any);
    }
}

GetGlobalInstr.prototype.spstfFlowFunc = GetPropInstr.prototype.spstfFlowFunc;

TypeOfInstr.prototype.spstfFlowFunc = function (ta)
{
    var t0 = ta.getInType(this, 0);

    ta.setOutType(this, TypeSet.string);
}

JSAddInstr.prototype.spstfFlowFunc = function (ta)
{
    var t0 = ta.getInType(this, 0);
    var t1 = ta.getInType(this, 1);
 
    //print('JSAddInstr');
    //print('t0: ' + t0);
    //print('t1: ' + t1);

    // If either type sets are undetermined, do nothing
    if (t0 === TypeSet.empty || t1 === TypeSet.empty)
    {
        ta.setOutType(this, TypeSet.empty);
        return;
    }
   
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
        outType = TypeSet.number;
    }

    // By default
    else
    {
        // The result can be int or float or string
        outType = new TypeSet(TypeFlags.INT | TypeFlags.FLOAT | TypeFlags.STRING);
    }

    ta.setOutType(this, outType);
}

JSSubInstr.prototype.spstfFlowFunc = function (ta)
{
    var t0 = ta.getInType(this, 0);
    var t1 = ta.getInType(this, 1);
    
    /*
    print('JSSubInstr');
    print('  t0: ' + t0);
    print('  t1: ' + t1);
    */

    // If either type sets are undetermined, do nothing
    if (t0 === TypeSet.empty || t1 === TypeSet.empty)
    {
        ta.setOutType(this, TypeSet.empty);
        return;
    }

    // Output type
    var outType;

    if (t0.flags === TypeFlags.INT && t1.flags === TypeFlags.INT)
    {
        var minVal = t0.rangeMin - t1.rangeMax;
        var maxVal = t0.rangeMax - t1.rangeMin;

        outType = new TypeSet(
            TypeFlags.INT,
            minVal,
            maxVal
        );
    }

    // By default
    else
    {
        outType = TypeSet.number;
    }

    ta.setOutType(this, outType);
}

JSMulInstr.prototype.spstfFlowFunc = function (ta)
{
    var t0 = ta.getInType(this, 0);
    var t1 = ta.getInType(this, 1);
    
    // If either type sets are undetermined, do nothing
    if (t0 === TypeSet.empty || t1 === TypeSet.empty)
    {
        ta.setOutType(this, TypeSet.empty);
        return;
    }

    // Output type
    var outType;

    if (t0.flags === TypeFlags.INT && t1.flags === TypeFlags.INT)
    {
        var minVal;
        minVal = t0.rangeMin * t1.rangeMin;
        minVal = Math.min(minVal, t0.rangeMin * t1.rangeMax);
        minVal = Math.min(minVal, t0.rangeMax * t1.rangeMin);
        if (isNaN(minVal))
            minVal = -Infinity;

        var maxVal;
        maxVal = t0.rangeMax * t1.rangeMax;
        maxVal = Math.max(maxVal, t0.rangeMin * t1.rangeMin);
        if (isNaN(maxVal))
            maxVal = Infinity;

        outType = new TypeSet(
            TypeFlags.INT,
            minVal,
            maxVal
        );
    }

    // By default
    else
    {
        outType = TypeSet.number;
    }

    ta.setOutType(this, outType);
}

JSDivInstr.prototype.spstfFlowFunc = function (ta)
{
    var t0 = ta.getInType(this, 0);
    var t1 = ta.getInType(this, 1);

    // If either type sets are undetermined, do nothing
    if (t0 === TypeSet.empty || t1 === TypeSet.empty)
    {
        ta.setOutType(this, TypeSet.empty);
        return;
    }

    // Output type
    var outType;

    if (t0.flags === TypeFlags.INT && t1.flags === TypeFlags.INT)
    {
        var minVal = Infinity;
        minVal = Math.min(minVal, t0.rangeMin / t1.rangeMin);
        minVal = Math.min(minVal, t0.rangeMin / t1.rangeMax);
        minVal = Math.min(minVal, t0.rangeMax / t1.rangeMin);
        minVal = Math.min(minVal, t0.rangeMax / t1.rangeMax);
        if (minVal === Infinity || isNaN(minVal) === true)
            minVal = -Infinity;

        var maxVal = -Infinity;
        maxVal = Math.max(maxVal, t0.rangeMin / t1.rangeMin);
        maxVal = Math.max(maxVal, t0.rangeMin / t1.rangeMax);
        maxVal = Math.max(maxVal, t0.rangeMax / t1.rangeMin);
        maxVal = Math.max(maxVal, t0.rangeMax / t1.rangeMax);
        if (maxVal === -Infinity || isNaN(maxVal) === true)
            maxVal = Infinity;

        var flags;
        if (t0.rangeMin === t0.rangeMax && 
            t1.rangeMin === t1.rangeMax &&
            t0.rangeMin % t1.rangeMax === 0)
            flags = TypeFlags.INT;
        else
            flags = TypeFlags.INT | TypeFlags.FLOAT;

        outType = new TypeSet(
            flags,
            minVal,
            maxVal
        );
    }

    // By default
    else
    {
        outType = TypeSet.number;
    }

    ta.setOutType(this, outType);
}

// Modulo (remainder) instruction
JSModInstr.prototype.spstfFlowFunc = function (ta)
{
    var v0 = ta.getInType(this, 0);
    var v1 = ta.getInType(this, 1);

    ta.setOutType(this, TypeSet.integer);
}

// Bitwise operations
JSBitOpInstr.prototype.spstfFlowFunc = function (ta)
{
    ta.setOutType(this, TypeSet.integer);
}

// Comparison operator base class
JSCompInstr.prototype.spstfFlowFunc = function (ta)
{
    var v0 = ta.getInType(this, 0);
    var v1 = ta.getInType(this, 1);

    ta.setOutType(this, TypeSet.bool);
}

// Operator ==
JSEqInstr.prototype.spstfFlowFunc = function (ta)
{
    var v0 = ta.getInType(this, 0);
    var v1 = ta.getInType(this, 1);

    // If either type sets are undetermined, do nothing
    if (v0 === TypeSet.empty || v1 === TypeSet.empty)
    {
        ta.setOutType(this, TypeSet.empty);
        return;
    }

    // Output type
    var outType;

    // If both values are known integer constants
    if (v0.flags === TypeFlags.INT &&
        v1.flags === TypeFlags.INT &&
        v0.rangeMin === v0.rangeMax &&
        v1.rangeMin === v1.rangeMax)
    {
        outType = (v0.rangeMin === v1.rangeMin)? TypeSet.true:TypeSet.false;
    }

    // If both values are known strings
    else if (
        v0.flags === TypeFlags.STRING && v1.flags === TypeFlags.STRING &&
        v0.strVal !== undefined && v1.strVal !== undefined)
    {
        outType = (v0.strVal === v1.strVal)? TypeSet.true:TypeSet.false;
    }

    // If both values are known booleans
    else if (
        (v0.flags === TypeFlags.TRUE || v0.flags === TypeFlags.FALSE) &&
        (v1.flags === TypeFlags.TRUE || v1.flags === TypeFlags.FALSE))
    {
        outType = (v0.flags === v1.flags)? TypeSet.true:TypeSet.false;
    }

    // Otherwise, we know the output is boolean
    else
    {
        outType = TypeSet.bool;
    }

    ta.setOutType(this, outType);
}

// Operator ===
JSSeInstr.prototype.spstfFlowFunc = function (ta)
{
    var v0 = ta.getInType(this, 0);
    var v1 = ta.getInType(this, 1);

    // If either type sets are undetermined, do nothing
    if (v0 === TypeSet.empty || v1 === TypeSet.empty)
    {
        ta.setOutType(this, TypeSet.empty);
        return;
    }

    // Output type
    var outType;

    // If both values are known integer constants
    if (v0.flags === TypeFlags.INT &&
        v1.flags === TypeFlags.INT &&
        v0.rangeMin === v0.rangeMax &&
        v1.rangeMin === v1.rangeMax)
    {
        outType = (v0.rangeMin === v1.rangeMin)? TypeSet.true:TypeSet.false;
    }

    // If both values are known strings
    else if (
        v0.flags === TypeFlags.STRING && v1.flags === TypeFlags.STRING &&
        v0.strVal !== undefined && v1.strVal !== undefined)
    {
        outType = (v0.strVal === v1.strVal)? TypeSet.true:TypeSet.false;
    }

    // If both values are known booleans
    else if (
        (v0.flags === TypeFlags.TRUE || v0.flags === TypeFlags.FALSE) &&
        (v1.flags === TypeFlags.TRUE || v1.flags === TypeFlags.FALSE))
    {
        outType = (v0.flags === v1.flags)? TypeSet.true:TypeSet.false;
    }

    // Otherwise, we know the output is boolean
    else
    {
        outType = TypeSet.bool;
    }

    ta.setOutType(this, outType);
}

// Operator !=
JSNeInstr.prototype.spstfFlowFunc = function (ta)
{
    var v0 = ta.getInType(this, 0);
    var v1 = ta.getInType(this, 1);

    // If either type sets are undetermined, do nothing
    if (v0 === TypeSet.empty || v1 === TypeSet.empty)
    {
        ta.setOutType(this, TypeSet.empty);
        return;
    }

    // Output type
    var outType = TypeSet.bool;

    // If the type flags are mutually exclusive,
    // the values cannot be equal
    if (v0.flags & v1.flags === 0)
    {
        outType = TypeSet.false;
    }

    // If both values are numbers and their ranges are mutually exclusive
    else if (
        (v0.flags === TypeFlags.INT || v0.flags === TypeFlags.FLOAT) &&
        (v1.flags === TypeFlags.INT || v1.flags === TypeFlags.FLOAT))
    {
        // If the ranges are mutually exclusive
        if (v0.rangeMax < v1.rangeMin || v1.rangeMax < v0.rangeMin)
            outType = TypeSet.true;

        // If the values are equal
        else if (v0.rangeMin === v0.rangeMax && 
                 v1.rangeMin === v1.rangeMax &&
                 v0.rangeMin === v1.rangeMin)
            outType = TypeSet.false;
    }

    // If both values are known strings
    else if (
        v0.flags === TypeFlags.STRING && v1.flags === TypeFlags.STRING &&
        v0.strVal !== undefined && v1.strVal !== undefined)
    {
        outType = (v0.strVal !== v1.strVal)? TypeSet.true:TypeSet.false;
    }

    ta.setOutType(this, outType);
}

// Operator !==
JSNsInstr.prototype.spstfFlowFunc = function (ta)
{
    var v0 = ta.getInType(this, 0);
    var v1 = ta.getInType(this, 1);

    // If either type sets are undetermined, do nothing
    if (v0 === TypeSet.empty || v1 === TypeSet.empty)
    {
        ta.setOutType(this, TypeSet.empty);
        return;
    }

    // Output type
    var outType = TypeSet.bool;

    // If the type flags are mutually exclusive,
    // the values cannot be equal
    if (v0.flags & v1.flags === 0)
    {
        outType = TypeSet.false;
    }

    // If both values are numbers and their ranges are mutually exclusive
    else if (
        (v0.flags === TypeFlags.INT || v0.flags === TypeFlags.FLOAT) &&
        (v1.flags === TypeFlags.INT || v1.flags === TypeFlags.FLOAT))
    {
        // If the ranges are mutually exclusive
        if (v0.rangeMax < v1.rangeMin || v1.rangeMax < v0.rangeMin)
            outType = TypeSet.true;

        // If the values are equal
        else if (v0.rangeMin === v0.rangeMax && 
                 v1.rangeMin === v1.rangeMax &&
                 v0.rangeMin === v1.rangeMin)
            outType = TypeSet.false;
    }

    // If both values are known strings
    else if (
        v0.flags === TypeFlags.STRING && v1.flags === TypeFlags.STRING &&
        v0.strVal !== undefined && v1.strVal !== undefined)
    {
        outType = (v0.strVal !== v1.strVal)? TypeSet.true:TypeSet.false;
    }

    // If both values are equal constants
    else if (
        (v0.flags === TypeFlags.UNDEF && v1.flags == TypeFlags.UNDEF) ||
        (v0.flags === TypeFlags.NULL && v1.flags == TypeFlags.NULL))
    {
        outType = TypeSet.false;
    }

    ta.setOutType(this, outType);
}

JumpInstr.prototype.spstfFlowFunc = function (ta)
{
    // Make the successor reachable
    ta.touchTarget(this, 0);
}

// TODO
// TODO: throw, must merge with all possible catch points
// TODO

// If branching instruction
IfInstr.prototype.spstfFlowFunc = function (ta)
{
    var instr = this;
    var irInstr = this.irInstr;

    var v0 = ta.getInType(this, 0);
    var v1 = ta.getInType(this, 1);
    var v2 = (irInstr.uses.length > 2)? ta.getInType(this, 2):undefined;

    // Function to handle the successor queuing for a given branch
    function mergeSuccs(boolVal)
    {
        /*
        print(irInstr);
        print('  from: ' + instr.block.func.getName());
        print('  if branch bool: ' + boolVal);
        */

        // If we can potentially narrow the comparison input types
        if (irInstr.testOp === 'EQ' && 
            v1.flags === TypeFlags.TRUE &&
            irInstr.uses[0] instanceof JSCompInstr)
        {
            var compInstr = irInstr.uses[0];

            var lVal = compInstr.uses[0];
            var rVal = compInstr.uses[1];

            var lType = ta.getType(instr, lVal);
            var rType = ta.getType(instr, rVal);

            var trueLType = lType;
            var falseLType = lType;
            
            // Less-than comparison
            if (compInstr instanceof JSLtInstr)
            {
                //print(lType + ' < ' + rType);

                // If both values are integer
                if (lType.flags === TypeFlags.INT && rType.flags === TypeFlags.INT)
                {
                    // lVal < rVal
                    var rangeMax = Math.min(lType.rangeMax, rType.rangeMax - 1);
                    var rangeMin = Math.min(lType.rangeMin, rangeMax);
                    var trueLType = new TypeSet(
                        TypeFlags.INT,
                        rangeMin,
                        rangeMax
                    );

                    // lVal >= rVal
                    var rangeMin = Math.max(lType.rangeMin, rType.rangeMin);
                    var rangeMax = Math.max(lType.rangeMax, rangeMin);
                    var falseLType = new TypeSet(
                        TypeFlags.INT,
                        rangeMin,
                        rangeMax
                    );
                }
            }

            // If the compared value is not a constant
            if ((lVal instanceof IRConst) === false)
            {
                /*
                print('lType     : ' + lType);
                print('trueLType : ' + trueLType);
                print('falseLType: ' + falseLType);
                */

                if (boolVal === true || boolVal === undefined)
                    ta.setType(instr, lVal, trueLType, 0);
                if (boolVal === false || boolVal === undefined)
                    ta.setType(instr, lVal, falseLType, 1);
            }
        }

        // Touch the reachable successors
        if (boolVal === true || boolVal === undefined)
            ta.touchTarget(instr, 0);
        if (boolVal === false || boolVal === undefined)
            ta.touchTarget(instr, 1);
    }

    // If either type sets are undetermined, do nothing
    if (v0 === TypeSet.empty || v1 === TypeSet.empty)
        return;

    // If this is an equality comparison
    if (irInstr.testOp === 'EQ')
    {
        if ((v0.flags === TypeFlags.TRUE && v1.flags === TypeFlags.TRUE) ||
            (v0.flags === TypeFlags.FALSE && v1.flags === TypeFlags.FALSE))
            return mergeSuccs(true);

        if ((v0.flags === TypeFlags.FALSE && v1.flags === TypeFlags.TRUE) ||
            (v0.flags === TypeFlags.TRUE && v1.flags === TypeFlags.FALSE))
            return mergeSuccs(false);
    }

    // Merge with both possible branch targets
    mergeSuccs();
}

JSCallInstr.prototype.spstfFlowFunc = function (ta)
{
    // Get the type set for the callee
    var calleeType = ta.getInType(this, 0);

    // Test if this is a new/constructor call
    var isNew = (this.irInstr instanceof JSNewInstr);

    // Get the this argument call
    var thisType = (isNew === false)? ta.getInType(this, 1):TypeSet.empty;

    // If there are no callees, get all argument types,
    // this prevents type assertions from failing
    // TODO: create pseudo TypeAssert function?
    if (calleeType.getNumObjs() === 0)
    {
        for (var i = 0; i < this.irInstr.uses.length; ++i)
            ta.getInType(this, i);
    }

    // Mark the call successors as reachable
    for (var i = 0; i < this.targets.length; ++i)
        ta.touchTarget(this, i);

    // Get the argument types
    var argTypes = [];
    for (var i = isNew? 1:2; i < this.irInstr.uses.length; ++i)
    {
        var argType = ta.getInType(this, i);
        argTypes.push(argType);
    }

    // Perform the function call
    ta.funcCall(
        this,
        isNew,
        calleeType, 
        thisType,
        argTypes
    );

    // Restrict the callee type in the continuation to functions
    var newCalleeType = calleeType.restrict(TypeFlags.FUNCTION);        
    ta.setInType(this, 0, newCalleeType, calleeType);
}

// New/constructor call instruction
// Handled by the same function as the regular call instruction
JSNewInstr.prototype.spstfFlowFunc = JSCallInstr.prototype.spstfFlowFunc;

// Function entry pseudo-instruction
function SPSTFEntryInstr()
{
    this.mnemonic = 'entry';
    this.type = IRType.none;
    this.uses = [];
    this.dests = [];
    this.targets = [];
}
SPSTFEntryInstr.prototype = new IRInstr();

SPSTFEntryInstr.prototype.spstfFlowFunc = function (ta)
{
    var func = this.block.func;

    // If this function is called as a constructor
    if (func.ctorCall === true)
    {
        // Get the callee function type
        var calleeType = ta.getType(this, func.argVals[0]);

        // Get the "this" argument type
        var thisType = ta.getType(this, func.argVals[1]);

        // Lookup the "prototype" property of the callee
        var protoType = ta.propLookup(this, calleeType, 'prototype');

        //print('calleeType : ' + calleeType);
        //print('  protoType: ' + protoType);

        // If the prototype type is not yet resolved
        if (protoType === TypeSet.empty)
        {
            var objType = TypeSet.empty;
        }
        else
        {
            // If the prototype may not be an object
            if (protoType.flags & (~TypeFlags.EXTOBJ))
            {
                // Exclude non-objects and include the object prototype object
                protoType = protoType.restrict(protoType.flags & (~TypeFlags.EXTOBJ));
                protoType = protoType.union(ta.objProto);
            }

            // Create a new object to use as the this argument
            var objType = ta.newObject(this, 'new_obj', undefined, protoType);
        }

        // Union the new object argument type with the this argument type
        thisType = thisType.union(objType);

        // Update the "this" argument type
        ta.setType(this, func.argVals[1], thisType);
    }
}

ArgValInstr.prototype.spstfFlowFunc = function (ta)
{
    var func = this.block.func;
    var argIndex = this.irInstr.argIndex;

    // Get the type of this argument
    var argType = ta.getType(this, func.argVals[argIndex]);

    ta.setOutType(this, argType);
}

RetInstr.prototype.spstfFlowFunc = function (ta)
{
    //print('RetInstr');

    // Get the return value type
    var retType = ta.getInType(this, 0);

    // Get the function this belongs to
    var func = this.block.func;

    // For each call site
    for (var i = 0; i < func.callSites.length; ++i)
    {
        var callSite = func.callSites[i];

        // If this is a constructor call
        if (callSite.irInstr instanceof JSNewInstr)
        {
            var callRet = TypeSet.empty;

            // If the return type may be undefined
            if (retType.flags & TypeFlags.UNDEF)
            {
                // Get the type of the function's "this" value
                var thisType = ta.getType(this, func.argVals[1]);

                // Union the "this" argument type
                callRet = callRet.union(thisType);
            }

            // If the return type may be not undefined
            if (retType.flags !== TypeFlags.UNDEF)
            {
                // Union all but undefined
                callRet = callRet.union(retType.restrict(
                    retType.flags & ~TypeFlags.UNDEF
                ));
            }
        }
        else
        {
            var callRet = retType;
        }

        // Define the return type for this call site
        ta.setType(this, callSite.irInstr, callRet);
    }
}

// LIR call instruction
CallFuncInstr.prototype.spstfFlowFunc = function (ta)
{
    var callee = this.irInstr.uses[0];

    // Return type (by default, any type)
    var retType = TypeSet.any;

    // If we cannot determine the callee
    if ((callee instanceof IRFunction) === false)
    {
        // Do nothing
    }

    // Creates the 'arguments' object
    else if (callee.funcName === 'makeArgObj')
    {
        var func = this.block.func;

        // Create the arguments object
        var argObjType = ta.newObject(
            this,
            'arg_obj',
            undefined,
            ta.objProto
        );
        var argObj = argObjType.getObjItr().get();

        // Set the arguments length value
        var lengthNode = ta.getPropNode(argObj, 'length');
        ta.setType(this, lengthNode, TypeSet.posInt);

        // TODO: callee property

        // Get the type of the indexed function argument value
        var idxArgType = ta.getType(this, func.idxArgVal);

        // Set the indexed property type of the argument object
        ta.setType(this, argObj.idxProp, idxArgType);

        retType = argObjType;
    }

    // Closure object creation
    else if (callee.funcName === 'makeClos')
    {
        //print('makeClos');

        var func = this.irInstr.uses[3];
        var numClosCells = this.irInstr.uses[4].value;

        //print(this);

        assert (
            func instanceof IRFunction,
            'closure of unknown function'
        );

        assert (
            isNonNegInt(numClosCells),
            'invalid num clos cells'
        );

        // Test if this is a global function declaration (not in a loop)
        var globalFunc = false;
        var curBlock = this.irInstr.parentBlock;
        if (curBlock instanceof BasicBlock)
        {
            var curFunc = curBlock.parentCFG.ownerFunc;
            var curEntry = curFunc.hirCFG.entry;
            var globalFunc = curFunc.parentFunc === null && curBlock === curEntry;
        }

        // Create an object node for this function
        var funcObj = ta.newObject(
            this,
            func.funcName,
            func,
            ta.funcProto, 
            TypeFlags.FUNCTION, 
            numClosCells,
            globalFunc
        );

        //var protoNode = ta.getPropNode(funcObj.getObjItr().get(), 'prototype');
        //ta.setType(this, protoNode, TypeSet.empty);

        retType = funcObj;
    }

    // Closure prototype object creation
    else if (callee.funcName === 'makeClosProto')
    {
        var funcObj = ta.getInType(this, 3);

        if (funcObj === TypeSet.empty)
        {
            retType = TypeSet.empty;
        }
        else
        {
            assert (
                funcObj.flags === TypeFlags.FUNCTION && funcObj.getNumObjs() === 1,
                'invalid function type'
            );

            // Test if this is a global function declaration (not in a loop)
            var globalFunc = false;
            var curBlock = this.irInstr.parentBlock;
            if (curBlock instanceof BasicBlock)
            {
                var curFunc = curBlock.parentCFG.ownerFunc;
                var curEntry = curFunc.hirCFG.entry;
                var globalFunc = curFunc.parentFunc === null && curBlock === curEntry;
            }

            // Create a Function.prototype object for the function
            var protoObj = ta.newObject(
                this,
                'proto',
                undefined,
                ta.objProto,
                undefined,
                undefined,
                globalFunc
            );

            // Assign the prototype object to the Function.prototype property
            var protoNode = ta.getPropNode(funcObj.getObjItr().get(), 'prototype');
            ta.setType(this, protoNode, protoObj);

            retType = TypeSet.empty;
        }
    }

    // Closure cell creation
    else if (callee.funcName === 'makeCell')
    {
        //print('makeCell');
        //print(this);

        var newCell = new TGClosCell(this.irInstr);

        var cellType = new TypeSet(
            TypeFlags.CELL,
            undefined,
            undefined,
            undefined,
            newCell
        );

        retType = cellType;
    }

    // Get object prototype
    else if (callee.funcName === 'get_ctx_objproto')
    {
        retType = ta.objProto;
    }

    // Get array prototype
    else if (callee.funcName === 'get_ctx_arrproto')
    {
        retType = ta.arrProto;
    }

    // Get function prototype
    else if (callee.funcName === 'get_ctx_funcproto')
    {
        retType = ta.funcProto;
    }

    // Get boolean prototype
    else if (callee.funcName === 'get_ctx_boolproto')
    {
        retType = ta.boolProto;
    }

    // Get number prototype
    else if (callee.funcName === 'get_ctx_numproto')
    {
        retType = ta.numProto;
    }

    // Get string prototype
    else if (callee.funcName === 'get_ctx_strproto')
    {
        retType = ta.strProto;
    }

    // Set closure cell variable
    else if (callee.funcName === 'set_clos_cells')
    {
        var closType = ta.getInType(this, 3);
        var cellIdx = this.irInstr.uses[4].value;
        var valType = ta.getInType(this, 5);

        if (closType === TypeSet.any)
            print('*WARNING: set_clos_cells on any type');

        // For each possible closure
        for (var itr = closType.getObjItr(); itr.valid(); itr.next())
        {
            var clos = itr.get();

            assert (
                cellIdx < clos.closVars.length,
                'invalid clos var index: ' + cellIdx + 
                ' (' + clos.closVars.length + ')'
            );

            var varNode = clos.closVars[cellIdx];

            var curType = ta.getType(this, varNode);
            curType = curType.union(valType);
            ta.setType(this, varNode, curType);
        }

        retType = TypeSet.empty;
    }

    // Get closure cell variable
    else if (callee.funcName === 'get_clos_cells')
    {
        var closType = ta.getInType(this, 3);
        var cellIdx = this.irInstr.uses[4].value;

        var outType = (closType === TypeSet.any)? TypeSet.any:TypeSet.empty;

        // For each possible closure
        for (var itr = closType.getObjItr(); itr.valid(); itr.next())
        {
            var clos = itr.get();

            assert (
                cellIdx < clos.closVars.length,
                'invalid clos var index: ' + cellIdx + 
                ' (' + clos.closVars.length + ')'
            );

            var varNode = clos.closVars[cellIdx];
            var varType = ta.getType(this, varNode);

            outType = outType.union(varType);
        }

        retType = outType;
    }

    // Set closure cell value
    else if (callee.funcName === 'set_cell_val')
    {
        var cellType = ta.getInType(this, 3);
        var valType = ta.getInType(this, 4);

        //print('set_cell_val');
        //print(this);

        assert (
            (cellType.flags & ~TypeFlags.CELL) === 0,
            'invalid closure cell type: ' + cellType
        );

        // For each possible cell
        for (var itr = cellType.getObjItr(); itr.valid(); itr.next())
        {
            var cell = itr.get();
            var varNode = cell.value;

            var curType = ta.getType(this, varNode);
            curType = curType.union(valType);
            ta.setType(this, varNode, curType);
        }

        retType = TypeSet.empty;
    }

    // Get closure cell value
    else if (callee.funcName === 'get_cell_val')
    {
        var cellType = ta.getInType(this, 3);

        //print('get_cell_val');
        //print(this);

        // If the cell type is not unknown
        if (cellType.flags !== TypeFlags.ANY)
        {
            assert (
                (cellType.flags & ~TypeFlags.CELL) === 0,
                'invalid closure cell type: ' + cellType
            );

            var outType = TypeSet.empty;

            // For each possible cell
            for (var itr = cellType.getObjItr(); itr.valid(); itr.next())
            {
                var cell = itr.get();
                var varType = ta.getType(this, cell.value);

                outType = outType.union(varType);
            }

            retType = outType;
        }
    }

    // Box an integer
    else if (callee.funcName === 'boxInt')
    {
        retType = TypeSet.integer;
    }

    // Allocate a string
    else if (callee.funcName === 'alloc_str')
    {
        retType = TypeSet.string;
    }

    // Retrieve a string from the string table
    else if (callee.funcName === 'getTableStr')
    {
        retType = TypeSet.string;
    }

    // Box value to boolean conversion
    else if (callee.funcName === 'boxToBool')
    {
        var val = ta.getInType(this, 3);

        if (val.flags === TypeFlags.EMPTY)
            retType = TypeSet.empty;
        else if (val.flags === TypeFlags.TRUE || val.flags === TypeFlags.FALSE)
            retType = val;
        else
            retType = TypeSet.bool;
    }

    // Box value to string conversion
    else if (callee.funcName === 'boxToString')
    {
        var val = ta.getInType(this, 3);

        if (val.flags === TypeFlags.EMPTY)
            retType = TypeSet.empty;
        else if (val.flags === TypeFlags.STRING)
            retType = val;
        else
            retType = TypeSet.string;
    }

    else if (callee.funcName === 'boxIsInt')
    {
        var val = ta.getInType(this, 3);

        if (val.flags === TypeFlags.EMPTY)
            retType = TypeSet.empty;
        else if (val.flags === TypeFlags.INT)
            retType = TypeSet.true;
        else if ((val.flags & TypeFlags.INT) == 0)
            retType = TypeSet.false;
        else
            retType = TypeSet.bool;
    }

    else if (callee.funcName === 'boxIsObj')
    {
        var val = ta.getInType(this, 3);

        if (val.flags === TypeFlags.EMPTY)
            retType = TypeSet.empty;
        else if (val.flags === TypeFlags.OBJECT)
            retType = TypeSet.true;
        else if ((val.flags & TypeFlags.OBJECT) == 0)
            retType = TypeSet.false;
        else
            retType = TypeSet.bool;
    }

    // Test if a value is the global object
    else if (callee.funcName === 'isGlobalObj')
    {
        var valType = ta.getInType(this, 3);

        if (valType.getNumObjs() === 1)
        {
            if (valType.getObjItr().get() === ta.globalObj)
                retType = TypeSet.true;
            else
                retType = TypeSet.false;
        }
        else
        {
            retType = TypeSet.bool;
        }
    }

    // Throw an exception
    else if (callee.funcName === 'throwExc')
    {
        // The output of this function is never used
        retType = TypeSet.empty;
    }

    // Floating-point operation placeholder
    else if (callee.funcName === 'noFPSupport')
    {
        // Assume any number can be returned
        retType = TypeSet.number;
    }

    // Unknown primitive
    else
    {
        //print('unknown primitive: ' + callee.funcName);
    }

    // Set our own output type
    ta.setOutType(this, retType);

    // Mark the successors as reachable
    for (var i = 0; i < this.targets.length; ++i)
        ta.touchTarget(this, i);
}

