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
Interprocedural type analysis implementation.

@author
Maxime Chevalier-Boisvert
*/









/*
TODO: treat global object type as a type that's passed to functions???

Associate global object type with function entry?

Should ideally have it flow through in the type map?


When we analyze unit-level functions, pass empty global type to first unit...
We need the global type merge at the end of the unit to pass it to the
next unit to analyze.

The order of the unit analysis matters.


For a function, set of call sites, or set of next blocks to merge into?
- Function calls can occur in the middle of blocks!
  - Re-queue that block, it will be analyzed with the new return site info


Range expansion issue: count range expansions and string concats in union function? ***
*/






//=============================================================================
//
// Type propagation analysis core
//
//=============================================================================

/**
@class Type propagation interprocedural analysis
*/
function TypeProp(params)
{
    assert (
        params instanceof CompParams,
        'expected compilation parameters'
    );

    /**
    Compilation parameters this analysis is associated with
    */
    this.params = params;

    /**
    Worklist of basic blocks queued to be analyzed
    */
    this.workList = new LinkedList();

    /**
    Set of blocks in the work list
    This is to avoid adding blocks to the work list twice
    */
    this.workSet = new HashSet();

    /**
    Map of type maps at block entries
    */
    this.blockTypes = new HashMap();

    /**
    Global object class
    This is distinct from specific global object type instances
    */
    this.globalClass = new ClassDesc('global');

    /**
    Map of IR functions to function-specific information
    */
    this.funcInfo = new HashMap();

    /**
    Ordered list of unit-level functions to be analyzed
    */
    this.unitList = [];

    /**
    Total analysis iteration count
    */
    this.itrCount = 0;
}

/**
Reinitialize/reset the analysis
*/
TypeProp.prototype.reset = function ()
{
    MapDesc.mapSet.clear();

    ClassDesc.classMap.clear();
    ClassDesc.nextClassIdx = 0;

    this.workList.clear();

    this.workSet.clear();

    this.blockTypes.clear();

    this.globalClass = new ClassDesc('global');

    this.funcInfo = new HashMap();

    this.unitList = [];

    this.itrCount = 0;
}

/**
Get the function info object for an IR function
*/
TypeProp.prototype.getFuncInfo = function (irFunc)
{
    // If an info object already exists, return it
    var info = this.funcInfo.get(irFunc);
    if (info !== HashMap.NOT_FOUND)
        return info;

    // Initialize the argument types
    var argTypes = new Array(irFunc.argVars.length + 2);
    for (var i = 0; i < argTypes.length; ++i)
        argTypes[i] = TypeDesc.noinf;

    // Create a new info object
    var info = {

        // Function entry block
        entry: irFunc.hirCFG.entry,

        // Global object type
        globalType: TypeDesc.noinf,

        // Types of formal arguments
        argTypes : argTypes,

        // Return type
        retType : TypeDesc.noinf,

        // Set of callers
        callerSet: new HashSet(),

        // List of callers
        callerList: [],

        // Next unit in the chain, for unit-level functions
        nextUnit: undefined
    };

    // Store the new function info object
    this.funcInfo.set(irFunc, info);

    return info;
}

/**
Queue a block to be (re)analyzed
*/
TypeProp.prototype.queueBlock = function (block)
{
    // If the block is already queued, do nothing
    if (this.workSet.has(block) === true)
        return;

    this.workList.addLast(block);

    this.workSet.add(block);
}

/**
Queue a function to be analyzed
*/
TypeProp.prototype.queueFunc = function (irFunc)
{
    assert (
        irFunc instanceof IRFunction,
        'expected IR function'
    );

    // Get the function's entry block
    var entry = irFunc.hirCFG.entry;

    // If the function has no entry type info
    if (this.blockTypes.has(entry) === false)
    {
        // Get the info object for this function
        var funcInfo = this.getFuncInfo(irFunc);

        // Initialize the entry type info
        var typeMap = new TypeMap();
        typeMap.setGlobalType(funcInfo.globalType);
        this.blockTypes.set(entry, typeMap);
    }

    // Queue the function's entry block
    this.queueBlock(entry);
}

/**
Queue a unit-level function to be analyzed
*/
TypeProp.prototype.queueUnit = function (ir)
{
    assert (
        ir.astNode instanceof Program,
        'IR object is not unit-level function'
    );

    // Get the info object for this function
    var funcInfo = this.getFuncInfo(ir);

    // FIXME: for not, initialize a new global object for the unit
    funcInfo.globalType = new TypeDesc(
        TypeFlags.OBJECT,
        undefined,
        undefined,
        undefined,
        [new MapDesc(this.globalClass)]
    );

    // Queue the unit function to be analyzed
    this.queueFunc(ir);

    // Add the unit to the list of units to be analyzed
    this.unitList.push(ir);

    // TODO: handle next unit
}

/**
Run the analysis for some number of iterations
*/
TypeProp.prototype.run = function (maxItrs)
{
    // Until the max iteration count is reached
    for (var numItrs = 0; maxItrs === undefined || numItrs < maxItrs; ++numItrs)
    {
        // If the work list is empty, stop
        if (this.workList.isEmpty() === true)
            break;

        // Run one analysis iteration
        this.iterate();
    }

    // Update the total iteration count
    this.itrCount += numItrs;

    // Return the number of iterations performed
    return numItrs;
}

/**
Run one type analysis iteration
*/
TypeProp.prototype.iterate = function ()
{
    assert (
        this.workList.isEmpty() === false,
            'empty work list'
    );

    // Remove a block from the work list
    var block = this.workList.remFirst();
    this.workSet.rem(block);

    print('------')
    print('Block: ' + block.getBlockName());
    print('------')
    print('');

    // Get a copy of the type set at the block entry
    var typeMap = this.blockTypes.get(block).copy();

    assert (
        typeMap !== HashMap.NOT_FOUND,
        'type set not found'
    );



    //
    // TODO: update list of blocks touching classes
    //
    // Do this when iterating over instr uses. Should eliminate many
    // blocks***



    // For each instruction
    for (var i = 0; i < block.instrs.length; ++i)
    {
        var instr = block.instrs[i];

        print(instr);

        // For each use of the instruction
        for (var j = 0; j < instr.uses.length; ++j)
        {
            var use = instr.uses[j];

            // If this is an IR instruction and this is its only use,
            // remove it from the type map
            if (use instanceof IRInstr && use.uses.length === 1)
                typeMap.rem(use);

            var useType = typeMap.getType(use);
            print(use.getValName() + ' : ' + useType);
        }

        // Process the phi node or instruction
        var outType =
            (instr instanceof PhiInstr)?
            this.phiFunc(instr, typeMap):
            this.instrFunc(instr, typeMap);

        if (instr.dests.length > 0)
            print(instr.getValName() + ' => ' + outType);
        print('');

        // If the output is uninferred, stop analyzing this block for not,
        // wait until better information is available
        if (outType === TypeDesc.noinf)
        {
            print('noinf output, stopping block analysis');
            print('');
            return;
        }

        // If the instruction has dests, add its type to the type set
        if (instr.dests.length > 0)
            typeMap.setType(instr, outType);
    }
}

/**
Merge incoming types for a successor block
*/
TypeProp.prototype.succMerge = function (succ, predMap)
{
    // Get the type map for the successor
    var succMap = this.blockTypes.get(succ);

    // If the successor has no type map yet
    if (succMap === HashMap.NOT_FOUND)
    {
        // Pass a copy of the predecessor map to the successor
        this.blockTypes.set(succ, predMap.copy());

        // Queue the successor for analysis
        this.queueBlock(succ);
    }
    else
    {
        // Merge the predecessor type map into the successor's
        var changed = succMap.merge(predMap);

        // If the successor's type map was changed,
        // queue the successor for analysis
        if (changed == true)
            this.queueBlock(succ);
    }
}

/**
Flow function applied to phi nodes
*/
TypeProp.prototype.phiFunc = function (phi, typeMap)
{
    var outType = TypeDesc.noinf;

    // For each phi predecessor
    for (var i = 0; i < phi.preds.length; ++i)
    {
        var pred = phi.preds[i];

        // If this predecessor hasn't been visited, skip it
        if (this.blockTypes.has(pred) === false)
            continue;

        // Merge the type of this incoming value
        var incType = typeMap.getType(phi.uses[i]);
        outType = outType.union(incType);
    }

    assert (
        outType !== TypeDesc.noinf,
        'phi output type is noinf'
    );

    // Return the phi node output type
    return outType;
}

/**
Flow function applied to instruction
*/
TypeProp.prototype.instrFunc = function (instr, typeMap)
{
    // If no type flow function is defined
    if (instr.typeProp === undefined)
    {
        // If this is a branch instruction
        if (instr.targets.length > 0)
        {
            // Set the output type in the type map
            if (instr.dests.length > 0)
                typeMap.setType(instr, TypeDesc.any);

            // Merge with all possible branch targets
            for (var i = 0; i < instr.targets.length; ++i)
                this.succMerge(instr.targets[i], typeMap);
        }

        // Return the any type
        return TypeDesc.any;
    }

    // Call the instruction's type flow function
    var outType = instr.typeProp(this, typeMap);

    assert (
        outType instanceof TypeDesc,
        'instruction flow function returned invalid type descriptor'
    );

    return outType;
}

//=============================================================================
//
// Flow functions for HIR instructions
//
//=============================================================================

GlobalObjInstr.prototype.typeProp = function (ta, typeMap)
{
    // Return the global object type
    return typeMap.globalType;    
}

InitGlobalInstr.prototype.typeProp = function (ta, typeMap)
{
    // Do nothing, the global property is treated as not being
    // guaranteed to exist
    return TypeDesc.any;
}

BlankObjInstr.prototype.typeProp = function (ta, typeMap)
{
    // Create a class descriptor for this instruction
    var classDesc = new ClassDesc(this);

    // Create a type using the class
    var objType = new TypeDesc(
        TypeFlags.OBJECT,
        undefined,
        undefined,
        undefined,
        [new MapDesc(classDesc)]
    );

    return objType;
}

HasPropInstr.prototype.typeProp = function (ta, typeMap)
{
    // TODO:
    // If type in map, return true
    // If type not in map but in class, return bool
    // If type not in class, return false

    // TODO: examine type map
    return TypeDesc.bool;
}

PutPropInstr.prototype.typeProp = function (ta, typeMap)
{
    var objType = typeMap.getType(this.uses[0]);
    var propName = typeMap.getType(this.uses[1]).stringVal();
    var valType = typeMap.getType(this.uses[2]);

    // TODO: issue, here, the property name could be unknown
    // A run-time check is needed
    if (propName === undefined)
    {
        print('*WARNING: putProp with unknown property name');
    }
    else
    {
        // Update the object property type
        var newObjType = objType.putProp(propName, valType);
        typeMap.setType(this.uses[0], newObjType);

        // If this is the global object, update the global object type as well
        var globalType = typeMap.getGlobalType();
        if (objType.equal(globalType) === true)
            typeMap.setGlobalType(newObjType);            
    }

    return valType;
}

GetPropInstr.prototype.typeProp = function (ta, typeMap)
{
    var objType = typeMap.getType(this.uses[0]);
    var propName = typeMap.getType(this.uses[1]).stringVal();

    var propType = TypeDesc.noinf;

    // Union the possible property types
    for (var i = 0; i < objType.mapSet.length; ++i)
    {
        var map = objType.mapSet[i];
        propType = propType.union(map.getPropType(propName));
    }

    return propType;
}

GetGlobalInstr.prototype.typeProp = GetPropInstr.prototype.typeProp;

JSAddInstr.prototype.typeProp = function (ta, typeMap)
{
    var t0 = typeMap.getType(this.uses[0]);
    var t1 = typeMap.getType(this.uses[1]);

    if (t0.flags === TypeFlags.INT && t1.flags === TypeFlags.INT)
    {
        var minVal = t0.minVal + t1.minVal;

        var maxVal = t0.maxVal + t1.maxVal;

        return new TypeDesc(
            TypeFlags.INT,
            minVal,
            maxVal
        );
    }

    if (t0.flags === TypeFlags.STRING || t1.flags === TypeFlags.STRING)
    {
        var t0Str = t0.stringVal();
        var t1Str = t1.stringVal();

        var newStr = (t0Str && t1Str)? (t0Str + t1Str):undefined;

        return new TypeDesc(
            TypeFlags.STRING,
            undefined,
            undefined,
            newStr
        );
    }

    if ((t0.flags & ~(TypeFlags.STRING | TypeFlags.INT)) === 0 &&
        (t1.flags & ~(TypeFlags.STRING | TypeFlags.INT)) === 0)
    {
        return new TypeDesc(TypeFlags.INT | TypeFlags.STRING)
    }

    return new TypeDesc(TypeFlags.INT | TypeFlags.FLOAT | TypeFlags.STRING);
}

JSSubInstr.prototype.typeProp = function (ta, typeMap)
{
    var t0 = typeMap.getType(this.uses[0]);
    var t1 = typeMap.getType(this.uses[1]);

    if (t0.flags === TypeFlags.INT && t1.flags === TypeFlags.INT)
    {
        var minVal = t0.minVal - t1.maxVal;

        var maxVal = t0.maxVal - t1.minVal;

        return new TypeDesc(
            TypeFlags.INT,
            minVal,
            maxVal
        );
    }

    if ((t0.flags & ~(TypeFlags.STRING | TypeFlags.INT)) === 0 &&
        (t1.flags & ~(TypeFlags.STRING | TypeFlags.INT)) === 0)
    {
        return new TypeDesc(TypeFlags.INT | TypeFlags.STRING)
    }

    return new TypeDesc(TypeFlags.INT | TypeFlags.FLOAT);
}

JSLtInstr.prototype.typeProp = function (ta, typeMap)
{
    // TODO:
    return TypeDesc.bool;
}

JSEqInstr.prototype.typeProp = function (ta, typeMap)
{
    var v0 = typeMap.getType(this.uses[0]);
    var v1 = typeMap.getType(this.uses[1]);

    // If both values are known integer constants
    if (v0.flags === TypeFlags.INT &&
        v1.flags === TypeFlags.INT &&
        v0.minVal === v0.maxVal &&
        v1.minVal === v1.maxVal)
    {
        return new TypeDesc(
            (v0.minVal === v1.minVal)?
            TypeFlags.TRUE:TypeFlags.FALSE
        );
    }

    // If both values are known booleans
    if (v0.flags === TypeFlags.TRUE && v1.flags === TypeFlags.TRUE)
        return TypeDesc.true;
    if (v0.flags === TypeFlags.TRUE && v1.flags === TypeFlags.FALSE)
        return TypeDesc.false;
    if (v0.flags === TypeFlags.FALSE && v1.flags === TypeFlags.TRUE)
        return TypeDesc.false;
    if (v0.flags === TypeFlags.FALSE && v1.flags === TypeFlags.FALSE)
        return TypeDesc.true;

    // Otherwise, we know the output is boolean
    return TypeDesc.bool;
}

JSNsInstr.prototype.typeProp = function (ta, typeMap)
{
    // TODO:
    return TypeDesc.bool;
}

JSCallInstr.prototype.typeProp = function (ta, typeMap)
{
    var callee = typeMap.getType(this.uses[0]);
    var thisArg = typeMap.getType(this.uses[1]);

    // Function return type
    var retType = TypeDesc.noinf;

    // For each potential callee
    for (var i = 0; i < callee.mapSet.length; ++i)
    {
        // Get this class descriptor
        var classDesc = callee.mapSet[i].classDesc;

        // Get the function for this class
        var func = classDesc.origin;

        // If this is not a function, ignore it
        if ((func instanceof IRFunction) === false)
            continue;

        //print('potential callee: ' + func.funcName);

        // Get the info object for this function
        var funcInfo = ta.getFuncInfo(func);

        // Argument types changed flag
        var argChanged = false;

        // For each argument
        for (var j = 0; j < funcInfo.argTypes.length; ++j)
        {
            // Get the incoming type for this argument
            var argType = 
                (j < this.uses.length)?
                typeMap.getType(this.uses[j]):
                TypeDesc.undef;

            // Merge the argument type
            var newType = argType.union(funcInfo.argTypes[j]);
            if (newType.equal(funcInfo.argTypes[j]) === false)
            {
                //print('arg type changed');
                //print('old type: ' + funcInfo.argTypes[j]);
                //print('new type: ' + newType);

                funcInfo.argTypes[j] = newType;
                argChanged = true;
            }
        }

        // Get the global type from the type map
        var globalType = typeMap.getGlobalType();

        // Merge the function's global type
        var newGlobal = funcInfo.globalType.union(globalType);
        if (newGlobal.equal(funcInfo.globalType) === false)
        {
            //print('global type changed');

            funcInfo.globalType = newGlobal;
            argChanged = true;
        }

        // If any argument types changed, queue the function for analysis
        if (argChanged === true)
            ta.queueFunc(func);

        // Add this instruction to the set of callers
        if (funcInfo.callerSet.has(this) === false)
        {
            funcInfo.callerSet.add(this);
            funcInfo.callerList.push(this);
        }
       
        // Merge the return type
        retType = retType.union(funcInfo.retType);
    }

    // Set our own output type in the type map
    if (this.dests.length > 0)
        typeMap.setType(this, retType);

    // Merge with all possible branch targets
    for (var i = 0; i < this.targets.length; ++i)
        ta.succMerge(this.targets[i], typeMap);

    // Return the function return type
    return retType;
}

// LIR call instruction
CallFuncInstr.prototype.typeProp = function (ta, typeMap)
{
    var callee = this.uses[0];

    // Return type
    var retType;

    // If we cannot determine the callee
    if ((callee instanceof IRFunction) === false)
    {
        // Do nothing
        retType = TypeDesc.any;
    }

    // If this is a call to makeClos (closure creation)
    else if (callee.funcName === 'makeClos')
    {
        var func = this.uses[3];

        assert (
            func instanceof IRFunction,
            'closure of unknown function'
        );

        // Create a class descriptor for this function
        var classDesc = new ClassDesc(func);

        // Create a type using the function's class
        var closType = new TypeDesc(
            TypeFlags.FUNCTION,
            undefined,
            undefined,
            undefined,
            [new MapDesc(classDesc)]
        );

        retType = closType;
    }

    // For other primitive functions
    else
    {
        // Do nothing
        retType = TypeDesc.any;
    }

    // Set our own output type in the type map
    if (this.dests.length > 0)
        typeMap.setType(this, retType);

    // Merge with all possible branch targets
    for (var i = 0; i < this.targets.length; ++i)
        ta.succMerge(this.targets[i], typeMap);

    // Return the return type
    return retType;
}

ArgValInstr.prototype.typeProp = function (ta, typeMap)
{
    // Get the info object for this function
    var func = this.parentBlock.parentCFG.ownerFunc;
    var funcInfo = ta.getFuncInfo(func);
    
    // Return the type for this argument
    return funcInfo.argTypes[this.argIndex];
}

RetInstr.prototype.typeProp = function (ta, typeMap)
{
    // Get the info object for this function
    var func = this.parentBlock.parentCFG.ownerFunc;
    var funcInfo = ta.getFuncInfo(func);
 
    var retType = typeMap.getType(this.uses[0]);

    // Merge the return type
    var newType = retType.union(funcInfo.retType);

    // If the return type changed
    if (newType.equal(funcInfo.retType) === false)
    {
        // Update the function return type
        funcInfo.retType = newType;

        // Queue the call site blocks for analysis
        for (var i = 0; i < funcInfo.callerList.length; ++i)
        {
            var callInstr = funcInfo.callerList[i];
            ta.queueBlock(callInstr.parentBlock);
        }
    }

    // This instruction has no output
    return TypeDesc.any;
}

// If branching instruction
IfInstr.prototype.typeProp = function (ta, typeMap)
{
    var v0 = typeMap.getType(this.uses[0]);
    var v1 = typeMap.getType(this.uses[1]);
    var v2 = (this.uses.length > 2)? typeMap.getType(this.uses[1]):undefined;


    // TODO: known branch function?


    if (this.testOp === 'EQ')
    {
        if (v0.flags === TypeFlags.TRUE && v1.flags === TypeFlags.TRUE)
        {
            ta.succMerge(this.targets[0], typeMap);
            return TypeDesc.any;
        }
    }

    // Merge with all possible branch targets
    for (var i = 0; i < this.targets.length; ++i)
        ta.succMerge(this.targets[i], typeMap);

    // This instruction returns no value
    return TypeDesc.any;
}

