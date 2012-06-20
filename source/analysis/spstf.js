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

        str += use.getValName();
    }

    return str + '}';
}

SPSTFUseSet.prototype.union = function (that)
{
    if (this === that)
        return this;

    var newSet = this.copy();

    // Add the elements from the second set
    for (var itr = that.getItr(); itr.valid(); itr.next())
        HashSet.prototype.add.call(newSet, itr.get());

    return newSet;
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
    List of argument value instructions
    */
    this.argInstrs = new Array(numParams);

    /**
    Argument object instruction
    */
    this.argObjInstr = undefined;

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
            srcs: []
            {
                instr,
                targetIdx
                outIdx
            },
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

        // Add a field for the argument types
        var numArgs = irInstr.uses.length + ((irInstr instanceof JSNewInstr)? 1:0);
        this.argTypes = new Array(numArgs);
        for (var i = 0; i < numArgs; ++i)
            this.argTypes[i] = TypeSet.empty;
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

    // Create the instruction holding the initial definitions
    var initIRInstr = Object.create(IRInstr.prototype);
    initIRInstr.mnemonic = 'init';
    initIRInstr.type = IRType.none;
    initIRInstr.uses = [];
    initIRInstr.dests = [];
    initIRInstr.targets = [];
    var initInstr = this.makeInstr(initIRInstr, metaEntry);

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
    this.objProto = this.newObject(
        initInstr,
        'obj_proto',
        undefined,
        undefined, 
        undefined, 
        undefined, 
        true
    );

    /**
    Array prototype object node
    */
    this.arrProto = this.newObject(
        initInstr,
        'arr_proto',
        undefined,
        this.objProto, 
        undefined, 
        undefined, 
        true
    );

    /**
    Function prototype object node
    */
    this.funcProto = this.newObject(
        initInstr,
        'func_proto',
        undefined,
        this.objProto,
        undefined, 
        undefined, 
        true
    );

    /**
    Boolean prototype object node
    */
    this.boolProto = this.newObject(
        initInstr,
        'bool_proto',
        undefined,
        this.objProto,
        undefined, 
        undefined, 
        true
    );

    /**
    Number prototype object node
    */
    this.numProto = this.newObject(
        initInstr,
        'num_proto',
        undefined,
        this.objProto,
        undefined, 
        undefined, 
        true
    );

    /**
    String prototype object node
    */
    this.strProto = this.newObject(
        initInstr,
        'str_proto',
        undefined,
        this.objProto,
        undefined, 
        undefined, 
        true
    );

    /**
    Global object node
    */
    this.globalObj = this.newObject(
        initInstr,
        'global',
        undefined,
        this.objProto, 
        undefined,
        undefined, 
        true
    );

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

    var typeSet = this.typeSets.get({ instr:irInstr, idx:useIdx });

    if (typeSet === HashMap.NOT_FOUND)
        return null;

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

    block.instrs.push(instr);

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

            // If this is an argument value instruction
            else if (irInstr instanceof ArgValInstr)
            {
                var argIndex = irInstr.argIndex;

                assert (
                    func.argInstrs[argIndex] === undefined,
                    'already have ArgValInstr for index'
                );

                // Store the instruction on the function object
                func.argInstrs[argIndex] = instr;
            }

            // If this is the argument object creation instruction
            else if (
                irInstr instanceof CallFuncInstr && 
                irInstr.uses[0] instanceof IRFunction &&
                irInstr.uses[0].funcName === 'makeArgObj')
            {
                func.argObjInstr = instr;
            }
        }

        // Add the block to the block map
        this.blockMap.set(stump, block);
    }

    // Return block representation
    return block;
}

/**
Get the SPSTFBlock instance for a given IR function
*/
SPSTF.prototype.getFunc = function (irFunc)
{
    assert (
        irFunc instanceof IRFunction,
        'expected IR function'
    );

    // Check if a representation has already been created for this function
    var func = this.funcMap.get(irFunc);

    // If no representation has yet been created
    if (func === HashMap.NOT_FOUND)
    {
        // Construct function representation
        var func = new SPSTFFunc(irFunc, 0, func);

        // Queue the function's entry block
        var entry = this.getBlock(new SPSTFStump(irFunc.hirCFG.entry), func);

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
    var lastBranch = lastBlock.instrs[lastBlock.instrs.length-1];
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

    /*    
    print(
        'Iterating instr: ' + 
        (instr.irInstr.parentBlock? instr.irInstr:null)
    );
    */

    // Call the flow function for ths instruction
    instr.flowFunc(this);

    // Store the type set for all uses of the instruction
    for (var i = 0; i < instr.irInstr.uses.length; ++i)
    {
        var useType = this.getInType(instr, i);
        this.typeSets.set({ instr:instr.irInstr, idx:i }, useType);
    }

    // Store the type set of the output value   
    var irInstr = instr.irInstr;
    var outVals = instr.outVals[0];
    var outType = TypeSet.empty;
    for (var i = 0; i < outVals.length; ++i)
    {
        var def = outVals[i];

        if (def.value === irInstr)
        {
            //print('  output: ' + def.type);
            outType = def.type;
            break;
        }
    }
    this.typeSets.set({ instr: irInstr }, outType);

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

            // For each current dest of this definition
            for (var i = 0; i < dests.length; ++i)
            {
                var use = dests[i];

                // If the use is no longer present
                if (useSet.has(use) === false)
                {
                    // Remove the type flow edge
                    that.remEdge(
                        def,
                        use
                    );
                }
            }

            // For each use in the incoming use set
            for (var itr = useSet.getItr(); itr.valid(); itr.next())
            {
                var dest = itr.get();

                // If this is a new use
                if (arraySetHas(dests, dest) === false)
                {
                    // Add a new type flow edge
                    that.addEdge(
                        def,
                        dest
                    );
                }
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
    var branch = block.instrs[block.instrs.length-1];

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

            // Test if the value is global
            var isGlobal = (
                value.parent instanceof TGObject || 
                value.parent instanceof TGClosCell
            );

            // If the value is global and defined in this function
            if (isGlobal === true && block.func.defSet.has(value) === true)
            {
                var succSet = callCont.liveMap.get(value);
                var succSet = filterPhis(succSet, block, callCont, value);
                useSet = useSet.union(succSet);
            }
        }
    }

    // If the branch is a call instruction
    else if (branch.irInstr instanceof JSCallInstr ||
             branch.irInstr instanceof JSNewInstr)
    {
        // Test if the value is global
        var isGlobal = (
            value.parent instanceof TGObject || 
            value.parent instanceof TGClosCell
        );

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
            // If any callee defines this value, remove it
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

            // If there are no callees
            // or the value is not global
            // or no callee defines this value
            if (branch.callees.length === 0 ||
                isGlobal === false ||
                calleeDef === false)
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
            //print('num preds: ' + block.preds.length)
            //print(block);

            // For each predecessor block
            for (var i = 0; i < block.preds.length; ++i)
            {
                var pred = block.preds[i];

                // Queue the predecessors
                this.queueBlock(pred, value);

                // Get the branch instruction of the predecessor block
                var branch = pred.instrs[pred.instrs.length-1];

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

    assert (
        protoSet.flags !== TypeFlags.EMPTY,
        'invalid proto set flags'
    );

    var obj = new TGObject(
        instr,
        tag,
        func,
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
                var branch = pred.instrs[pred.instrs.length-1];

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
Add a def-use edge
*/
SPSTF.prototype.addEdge = function (
    def,
    use
)
{
    /*
    print('Adding edge');
    print('  val : ' + value);
    print('  from: ' + defInstr);
    print('  to  : ' + useInstr);
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
SPSTF.prototype.remEdge = function (
    def,
    use
)
{
    /*
    print('Removing edge');
    print('  val : ' + def.value);
    print('  from: ' + def.instr);
    print('  to  : ' + use.instr);
    */

    assert (
        def.value === use.value,
        'def-use value mismatch'
    );

    assert (
        use.instr instanceof SPSTFInstr,
        'invalid use object'
    );

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
        // TODO
        // TODO: warn if changed?
        // TODO

        /*
        print('changed');
        print('  pre : ' + use.type);
        print('  post: ' + useType);
        print('  ' + use.srcs.length);
        */

        use.type = useType;

        this.queueInstr(use.instr);
    }

    this.numEdges--;
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

    // This is a new definition for this instruction
    else
    {
        // Create a new definition object
        var def = {
            value: value,
            type: type,
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

        // Assign the missing type to the property at the object
        // creation site. This is because object properties do not
        // exist at object creation time
        this.setType(origInstr, propNode, TypeSet.missing);

        //print('creating init def for: "' + propName + '"');
        //print('  orig instr: ' + origInstr);
    }

    return obj.getPropNode(propName);
}

/**
Perform a property lookup with recursive prototype chain search
*/
SPSTF.prototype.propLookup = function (instr, objType, propName, depth)
{
    if (objType.flags === TypeFlags.ANY)
        throw '*WARNING: getProp on any type';

    // If there are non-object bases
    if (objType.flags & 
        ~(TypeFlags.EXTOBJ  | 
          TypeFlags.UNDEF   | 
          TypeFlags.NULL    |
          TypeFlags.STRING)
        )
        throw '*WARNING: getProp with invalid base';

    // If we have exceeded the maximum lookup depth
    if (depth > 8)
        throw '*WARNING: maximum prototype chain lookup depth exceeded';

    // Output type set
    var outType = TypeSet.empty;

    //print('depth ' + depth);
    //print('obj type : ' + objType);
    //print('prop name: ' + propName + ' ' + (typeof propName));

    // If the object may be a string
    if (objType.flags & TypeFlags.STRING)
    {
        // If this is the length property
        if (propName === 'length')
        {
            outType = outType.union(TypeSet.posInt);
        }

        // If this is a named property
        else if (typeof propName === 'string')
        {
            // Lookup the property on the string prototype
            var protoProp = this.propLookup(instr, this.strProto, propName, depth + 1);
            outType = outType.union(protoProp);
        }

        // Otherwise, this is an index property
        else
        {
            // This is a substring
            outType = outType.union(TypeSet.string);
        }
    }

    // For each possible object
    for (var objItr = objType.getObjItr(); objItr.valid(); objItr.next())
    {
        var obj = objItr.get();

        // If this is the length property of an array
        if (obj.flags === TypeFlags.ARRAY && propName === 'length')
        {
            outType = outType.union(TypeSet.posInt)
        }

        // Otherwise, for normal properties
        else
        {
            // Get the node for this property
            if (typeof propName === 'string')
                var propNode = this.getPropNode(obj, propName);
            else
                var propNode = obj.idxProp;

            // Get the type for this property node
            var propType = this.getType(instr, propNode)
        
            // If this property may be missing or this is an unbounded array access
            if (propType.flags & TypeFlags.MISSING || propName === false)
            {
                // Get the type for the object's prototype
                var protoNode = obj.proto;
                var protoType = this.getType(instr, protoNode);

                // If the prototype is not necessarily null
                if (protoType.flags & ~TypeFlags.NULL)
                {
                    // Do a recursive lookup on the prototype
                    var protoProp = this.propLookup(instr, protoType, propName, depth + 1);

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
    }

    //print('depth: ' + depth);
    //print('out type: ' + outType);
    //print('');

    return outType;
}

//=============================================================================
//
// Per-instruction flow/transfer functions
//
//=============================================================================

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
    // Create a new object from the object prototype
    var newObj = ta.newObject(this, 'obj', undefined, ta.objProto);

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

    try
    {
        if (objType.flags === TypeFlags.ANY)
            throw '*WARNING: putProp on any type';

        if ((objType.flags & TypeFlags.EXTOBJ) === 0)
            throw '*WARNING: putProp on non-object';

        if (nameType.flags === TypeFlags.ANY)
            throw '*WARNING: putProp with any name';

        // Test if there is a single, known object type
        var singleType = (
            objType.getNumObjs() === 1 && 
            (objType & ~TypeFlags.EXTOBJ) === 0
        );

        // Get a reference to this function
        var func = this.block.func;

        // Test if the object is the this argument of the function and
        // the function is only ever called as a constructor
        var isCtorThis = (
            this.irInstr.uses[0] instanceof ArgValInstr &&
            this.irInstr.uses[0].argIndex === 1 &&
            func.normalCall === false
        );

        // If this is not a string constant or an integer
        if ((nameType.flags !== TypeFlags.STRING || nameType.strVal === undefined) &&
            nameType.flags !== TypeFlags.INT)
            throw '*WARNING: putProp with unknown property name: ' + nameType;

        // Get the property name string, if any
        var propName = (nameType.flags === TypeFlags.STRING)? nameType.strVal:undefined;

        // If writing to an array property, add the undefined type
        if (nameType.flags === TypeFlags.INT)
            valType = valType.union(TypeSet.undef);

        // For each possible object
        for (var objItr = objType.getObjItr(); objItr.valid(); objItr.next())
        {
            var obj = objItr.get();

            // Get the node for this property
            if (propName !== undefined)
                var propNode = ta.getPropNode(obj, propName);
            else
                var propNode = obj.idxProp;

            /*
            // Test if the object was created in this function
            var isLocalObj = (
                obj.origin.parentBlock &&
                obj.origin.parentBlock.parentCFG &&
                obj.origin.parentBlock.parentCFG.ownerFunc === func.irFunc &&
                (this.irInstr.uses[0] === obj.origin ||
                 this.irInstr.parentBlock === obj.origin.parentBlock)
            );
            */

            // Test if the object was created in this function
            var isLocalObj = (
                obj.origin.block.func === func &&
                (this.irInstr.uses[0] === obj.origin.irInstr ||
                 this.irInstr.parentBlock === obj.origin.irInstr.parentBlock)
            );

            // Test if we can overwrite the current property type
            var canAssignType = (
                propNode !== obj.idxProp &&
                (
                    isCtorThis === true ||
                    (singleType === true && isLocalObj === true) ||
                    (singleType === true && obj.singleton === true)
                )
            );

            //print(this);

            // If we can do a strong update
            if (canAssignType === true)
            {
                //print('strong update');

                // Do a strong update on the property type
                ta.setType(this, propNode, valType);
            }
            else
            {
                //print('weak update');

                // Union with the current property type
                var propType = ta.getType(this, propNode);
                var newType = propType.union(valType);
                ta.setType(this, propNode, newType);
            }
        }
    }

    // If an inference problem occurs
    catch (e)
    {
        if (e instanceof Error)
            throw e;

        if (config.verbosity >= log.DEBUG)
        {
            print(e);
            print(this);
        }
    }

    // The object cannot be undefined or null along the normal branch
    var newObjType = objType.restrict(TypeFlags.ANY & ~(TypeFlags.UNDEF | TypeFlags.NULL));
    ta.setInType(this, 0, newObjType, objType);
}

GetPropInstr.prototype.spstfFlowFunc = function (ta)
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
        // If the property name could be anything
        if (nameType.flags === TypeFlags.ANY)
            throw '*WARNING: getProp with any name';

        // If this is not a string constant or an integer
        if ((nameType.flags !== TypeFlags.STRING || nameType.strVal === undefined) &&
            nameType.flags !== TypeFlags.INT)
            throw '*WARNING: getProp with unknown property name: ' + nameType;

        // If the property name is a string
        var propName;
        if (nameType.flags === TypeFlags.STRING)
        {
            propName = nameType.strVal;
        }
        else
        {
            // TODO: more generic test for pos int, int < arr.length

            // If this is a bounded arguments access
            if (boundedArgsGet(this.irInstr) === true)
            {
                // The array access is bounded
                propName = true;
            }
            else
            {
                // For now, assume unbounded array access
                propName = false;
            }
        }

        // Perform the property lookup
        var outType = ta.propLookup(this, objType, propName, 0);

        ta.setOutType(this, outType);
    }

    // If an inference problem occurs
    catch (e)
    {
        if (e instanceof Error)
            throw e;

        if (config.verbosity >= log.DEBUG)
        {
            print(e);
            print(this);
        }

        ta.setOutType(this, TypeSet.any);
    }
}

GetGlobalInstr.prototype.spstfFlowFunc = GetPropInstr.prototype.spstfFlowFunc;

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
        var minVal = 0;      
        if (Math.abs(t1.rangeMin) !== Infinity)
            minVal = Math.min(t0.rangeMin / t1.rangeMin);
        if (Math.abs(t1.rangeMax) !== Infinity)
            minVal = Math.min(t0.rangeMin / t1.rangeMax);
        if (Math.abs(t1.rangeMin) !== Infinity)
            minVal = Math.min(t0.rangeMax / t1.rangeMin);
        if (Math.abs(t1.rangeMax) !== Infinity)
            minVal = Math.min(t0.rangeMax / t1.rangeMax);

        var maxVal;
        if (Math.abs(t1.rangeMin) !== Infinity)
            maxVal = Math.max(t0.rangeMin / t1.rangeMin);
        if (Math.abs(t1.rangeMax) !== Infinity)
            maxVal = Math.max(t0.rangeMin / t1.rangeMax);
        if (Math.abs(t1.rangeMin) !== Infinity)
            maxVal = Math.max(t0.rangeMax / t1.rangeMin);
        if (Math.abs(t1.rangeMax) !== Infinity)
            maxVal = Math.max(t0.rangeMax / t1.rangeMax);

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

    //print('call: ' + this);

    // If the callee could be any function
    if (calleeType.flags === TypeFlags.ANY)
    {
        if (config.verbosity >= log.DEBUG)
            print('*WARNING: callee has type ' + calleeType);

        ta.setOutType(this, TypeSet.any);

        // Mark the call successors as reachable
        for (var i = 0; i < this.targets.length; ++i)
            ta.touchTarget(this, i);

        // No callees to analyze
        return;
    }

    // Test if this is a new/constructor call
    var isNew = (this.irInstr instanceof JSNewInstr);

    // If this is a regular function call
    if (isNew === false)
    {
        // Get the this argument call
        var thisType = ta.getInType(this, 1);
    }
    else
    {
        // Lookup the "prototype" property of the callee
        var protoType = ta.propLookup(this, calleeType, 'prototype', 0);

        // If the prototype type is not yet resolved
        if (protoType === TypeSet.empty)
        {
            var thisType = TypeSet.empty;
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
            var thisType = ta.newObject(this, 'new_obj', undefined, protoType);
        }
    }

    // For each argument
    for (var i = 0; i < this.argTypes.length; ++i)
    {
        var argType;

        // Get the incoming type for this argument
        if (i === 0)
        {
            argType = ta.getInType(this, 0);
        }
        else if (i === 1)
        {
            argType = thisType;
        }
        else
        {
            var useIdx = (isNew === true)? (i-1):i;
            argType = ta.getInType(this, useIdx);
        }

        // If the argument type changed
        if (this.argTypes[i].equal(argType) === false)
        {
            // Update the argument type
            this.argTypes[i] = argType;

            // For each potential callee
            for (var itr = calleeType.getObjItr(); itr.valid(); itr.next())
            {
                var callee = itr.get();

                // If this is not a function, ignore it
                if ((callee.func instanceof IRFunction) === false)
                    continue;

                // Get the SPSTFFunc instance for this value
                var irFunc = callee.func;
                var func = ta.getFunc(irFunc);

                // Queue the argument value instruction for this argument
                var argInstr = func.argInstrs[i];
                if (argInstr !== undefined)
                    ta.queueInstr(func.argInstrs[i]);

                // If the function uses the arguments object, queue the
                // argument object creation instruction
                if (func.argObjInstr !== undefined)
                    ta.queueInstr(func.argObjInstr);
            }
        }
    }

    // Mark the call successors as reachable
    for (var i = 0; i < this.targets.length; ++i)
        ta.touchTarget(this, i);

    // Get the call continuation block
    var callCont = this.targets[0];

    // Union of the return type of all potential callees
    var retType = TypeSet.empty;

    // For each potential callee
    for (var itr = calleeType.getObjItr(); itr.valid(); itr.next())
    {
        var callee = itr.get();

        // If this is not a function, ignore it
        if ((callee.func instanceof IRFunction) === false)
            continue;

        var irFunc = callee.func;

        //print('callee: ' + irFunc.funcName);

        // Get the SPSTFFunc instance for this value
        var func = ta.getFunc(irFunc);

        // If this function is a new callee
        if (arraySetHas(this.callees, func) === false)
        {
            // Add the function to the callee set
            arraySetAdd(this.callees, func);

            // Add this instruction to the set of callers of the function
            arraySetAdd(func.callSites, this);

            // Queue the call site block for all live values at the function entry
            for (var liveItr = func.entry.liveMap.getItr(); liveItr.valid(); liveItr.next())
                ta.queueBlock(this.block, liveItr.get().key);

            // Queue the return blocks for all values live in the call continuation
            if (callCont instanceof SPSTFBlock)
            {
                for (var liveItr = callCont.liveMap.getItr(); liveItr.valid(); liveItr.next())
                {
                    var value = liveItr.get().key;
                    for (var i = 0; i < func.retBlocks.length; ++i)
                        ta.queueBlock(func.retBlocks[i], value);
                }
            }

            // Queue the argument value instructions
            for (var i = 0; i < func.argInstrs.length; ++i)
                if (func.argInstrs[i] !== undefined)
                    ta.queueInstr(func.argInstrs[i]);

            // Add the callee's definitions to the caller's definitions
            var caller = this.block.func;
            for (var defItr = func.defSet.getItr(); defItr.valid(); defItr.next())
                ta.addFuncDef(caller, defItr.get());
        }

        // Set the call type flags
        if (isNew === true)
            func.ctorCall = true;
        else
            func.normalCall = true;

        // Compute the return type for this call
        var calleeRet = TypeSet.empty;
        for (var i = 0; i < func.retBlocks.length; ++i)
        {
            var retBlock = func.retBlocks[i];
            var retInstr = retBlock.instrs[retBlock.instrs.length-1];
            var retRet = retInstr.retType;

            if ((retRet instanceof TypeSet) === false)
                continue;

            calleeRet = calleeRet.union(retRet);
        }

        // If this is a constructor call
        if (isNew === true)
        {
            var newCalleeRet = TypeSet.empty;

            // If the return type may be undefined
            if (calleeRet.flags & TypeFlags.UNDEF)
            {
                // Union the "this" argument type
                newCalleeRet = newCalleeRet.union(thisType);
            }

            // If the return type may be not undefined
            if (calleeRet.flags !== TypeFlags.UNDEF)
            {
                // Union all but undefined
                newCalleeRet = newCalleeRet.union(calleeRet.restrict(
                    calleeRet.flags & ~TypeFlags.UNDEF
                ));
            }

            calleeRet = newCalleeRet;
        }

        // Update the return type
        retType = retType.union(calleeRet);
    }

    // Restrict the callee type in the continuation to functions
    var newCalleeType = calleeType.restrict(TypeFlags.FUNCTION);        
    ta.setInType(this, 0, newCalleeType, calleeType);

    //print('setting return type: ' + retType);

    // Set the call return type
    ta.setOutType(this, retType);
}

// New/constructor call instruction
// Handled by the same function as the regular call instruction
JSNewInstr.prototype.spstfFlowFunc = JSCallInstr.prototype.spstfFlowFunc;

// Call with apply instruction
// TODO
//CallApplyInstr.prototype.spstfFlowFunc = function (ta)
//{
//}

ArgValInstr.prototype.spstfFlowFunc = function (ta)
{
    var func = this.block.func;
    var argIndex = this.irInstr.argIndex;

    var argType = TypeSet.empty;

    //print('ArgValInstr');
    //print('argIndex: ' + argIndex);

    // For each call site
    for (var i = 0; i < func.callSites.length; ++i)
    {
        // Get the type of the argument from this caller
        var callerArg = func.callSites[i].argTypes[argIndex];
        if (callerArg === undefined)
            callerArg = TypeSet.undef;

        //print(callerArg);

        argType = argType.union(callerArg);
    }

    //print('out: ' + argType);

    ta.setOutType(this, argType);
}

RetInstr.prototype.spstfFlowFunc = function (ta)
{
    //print('RetInstr');

    // Get the return value type
    var retType = ta.getInType(this, 0);

    // If the return type changed
    if (this.retType === undefined || this.retType.equal(retType) === false)
    {
        // Store the return value type
        this.retType = retType;

        // Get the function this belongs to
        var func = this.block.func;

        // Queue the call instructions at the call sites
        for (var i = 0; i < func.callSites.length; ++i)
            ta.queueInstr(func.callSites[i]);
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

        // Compute the union of all visible argument types
        var idxArgType = TypeSet.empty;
        for (var i = 0; i < func.callSites.length; ++i)
        {
            var callSite = func.callSites[i];
            for (var j = 2; j < callSite.argTypes.length; ++j)
            {
                var callerArg = callSite.argTypes[j];
                idxArgType = idxArgType.union(callerArg);
            }
        }

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
            'closure',
            func,
            ta.funcProto, 
            TypeFlags.FUNCTION, 
            numClosCells,
            globalFunc
        );

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

        retType = funcObj;
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
        retType = TypeSet.posInt;
    }

    // Box value to boolean conversion
    else if (callee.funcName === 'boxToBool')
    {
        var val = ta.getInType(this, 2);

        if (val.flags === TypeFlags.TRUE || val.flags === TypeFlags.FALSE)
            retType = val;
        else
            retType = TypeSet.bool;
    }

    // Box value to string conversion
    else if (callee.funcName === 'boxToString')
    {
        var valType = ta.getInType(this, 2);

        if (valType.flags === TypeFlags.STRING)
            retType = valType;
        else
            retType = TypeSet.string;
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

