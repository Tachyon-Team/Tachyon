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


/*
How do we manage the global undef initialization case?
- Init to undefined because variables may be used before they are defined

- Differentiate field creation from assignment of undefined?
  - There is only one assignment to undefined, before the code of the unit is run
  - You know that no function call can cause the global field init to occur

js_global_init?
- Marks field as existing in class, gives it the noinf type?
  - Can't just do that? Any assignment to the variable will change the class type
- But we maintain a map of must-be-defined fields on the global object
  - Could this map store some initialized to undefined flag?

Key idea: we know the flow will never come back to the global init statement again!***
- js_global_init adds noinf type to class
- If we have the undefined value in the map, know the field could have class type or
  undefined
- If no undefined in map, we are fine :D
- No need for special instruction? Knowing it happens at global scope is enough
  - Could store type descriptor in type map? Or just undefined flag.

PROBLEM: if we store a whole type descriptor at the map level, the types are not
accounted for in the class?
- This is fine for the "init with undefined", because the global descriptor gets
  modified before any functions can use it. Everybody after that must see this
  change.
- Isn't this true for all global prop sets? Flow must pass through, everybody will
  see the changes made at the map level!

- Not quite! Previous units and previous code must be aware that these types can
exist on the global object?
  - Normally, previous code will get a global object descriptor at the time it
    is called, giving it access to the updated map descriptor.
  - What about a closure capturing the global object?

- If I store a reference to the global object, I store the old type descriptor/map
  - This indirect reference is unaware of types that have been added to the class

- Cannot store general type descriptor or account for all possible modifications


Special js_global_init instruction:
- Don't add field in the class
- To old unit code, the field isn't guaranteed to exist, it is not in their maps
  - Could provoke an exception if accessed
  - The field could technically exist, however.
- Once field is initialized to undefined, marked in type descriptor, still no mention
  at the class level
  - New type descriptors know the field would register as undefined
- Once a proper value is set through a put_prop, the class is actually changed

- Merge???
  - Merging type descriptor with proper class field with type descriptor with
    undefined value? Could retract back to "field not guaranteed to exist"
    - It's an intersection we're computing

- Couldn't we simply handle this global undefined init case by saying the field
isn't guaranteed to exist???
  - The analysis will output undefined as a possible type
  - Problem? If we merge with something that has the field? Still not guaranteed to exist

- If we flow through a global block that assigns undefined? Field goes back to not guaranteed to exist.
  - What if we end up calling some old code that thought this field was guaranteed to exist???****




- Could have js_global_init instruction that does the whole conditional shebang at once?
  - We know the field doesn't initially exist, gets set to undefined???
- Field set to not guaranteed to exist at output of js_global_init

- What if some old code assigned the global object somewhere, thinks that some
  field is guaranteed to exist?
  - That code is right, because it just created the field

The js_global_init code will not go store undefined if the field exists.

On the other hand, it could be that that field contains undefined?
- Passing through js_global_init will make that be taken into account?
- Only way to undo that is to pass by an assignment to the global object





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
        callers: new HashSet(),

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

        // If the instruction has dests, add its type to the type set
        if (instr.dests.length > 0)
            typeMap.setType(instr, outType);
    }

    // For each successor
    for (var i = 0; i < block.succs.length; ++i)
    {
        var succ = block.succs[i];

        // Get the type map for the successor
        var succMap = this.blockTypes.get(succ);

        // If the successor has no type map yet
        if (succMap === HashMap.NOT_FOUND)
        {
            // Pass a copy of the predecessor map to the successor
            this.blockTypes.set(succ, typeMap.copy());

            // Queue the successor for analysis
            this.queueBlock(succ);
        }
        else
        {
            // Merge the local type map into the successor's
            var changed = succMap.merge(typeMap);

            // If the successor's type map was changed,
            // queue the successor for analysis
            if (changed == true)
                this.queueBlock(succ);
        }
    }
}

/**
Flow function applied to phi nodes
*/
TypeProp.prototype.phiFunc = function (phi, typeMap)
{
    // TODO
}

/**
Flow function applied to instruction
*/
TypeProp.prototype.instrFunc = function (instr, typeMap)
{
    // If no type flow function is defined, return the any type
    if (instr.typeProp === undefined)
        return TypeDesc.any;


    // TODO: call site handling

    // TODO: return handling


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

JSNsInstr.prototype.typeProp = function (ta, typeMap)
{
    // TODO
    return TypeDesc.bool;
}

GlobalObjInstr.prototype.typeProp = function (ta, typeMap)
{
    // Return the global object type
    return typeMap.globalType;    
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
    var globalType = typeMap.getGlobalType();
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
        // Update the global type with the new property
        var globalType = globalType.putProp(propName, valType);
        typeMap.setGlobalType(globalType);
    }

    return valType;
}

InitGlobalInstr.prototype.typeProp = function (ta, typeMap)
{
    // Do nothing, the global property is treated as not being
    // guaranteed to exist
    return TypeDesc.any;
}

GetGlobalInstr.prototype.typeProp = function (ta, typeMap)
{
    var globalType = typeMap.getGlobalType();
    var propName = typeMap.getType(this.uses[1]).stringVal();

    var propType = TypeDesc.noinf;

    // Union the map types
    for (var i = 0; i < globalType.mapSet.length; ++i)
    {
        var map = globalType.mapSet[i];

        propType = propType.union(map.getPropType(propName));
    }

    return propType;
}

JSAddInstr.prototype.typeProp = function (ta, typeMap)
{
    var t0 = typeMap.getType(this.uses[0]);
    var t1 = typeMap.getType(this.uses[1]);

    if (t0.flags === TypeFlags.INT && t1.flags === TypeFlags.INT)
    {
        // TODO: double range each time

        return new TypeDesc(TypeFlags.INT);
    }

    if (t0.flags === TypeFlags.STRING || t1.flags === TypeFlags.STRING)
    {
        var t0Str = t0.stringVal();
        var t1Str = t1.stringVal();

        // TODO: max length

        return new TypeDesc(
            TypeFlags.STRING,
            undefined,
            undefined,
            (t0Str && t1Str)? (t0Str + t1Str):undefined
        );
    }

    return new TypeDesc(TypeFlags.INT | TypeFlags.FLOAT | TypeFlags.STRING);
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
        funcInfo.callers.add(this);
       
        // Merge the return type
        retType = retType.union(funcInfo.retType);
    }

    // Return the function return type
    return retType;
}

// LIR call instruction
CallFuncInstr.prototype.typeProp = function (ta, typeMap)
{
    var callee = this.uses[0];

    // If we cannot determine the callee, do nothing
    if ((callee instanceof IRFunction) === false)
        return TypeDesc.any;

    // If this is a call to makeClos (closure creation)
    if (callee.funcName === 'makeClos')
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

        return closType;
    }

    return TypeDesc.any;
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
        for (var itr = funcInfo.callers.getItr(); itr.valid(); itr.next())
        {
            var callInstr = itr.get();
            ta.queueBlock(callInstr.parentBlock);
        }
    }

    // This instruction has no output
    return TypeDesc.any;
}




// TODO: handle more instructions!





