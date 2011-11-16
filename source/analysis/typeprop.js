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
    this.globalClass = new ClassDesc();
    this.globalClass.globalClass = true;






    // TODO
    // TODO: Need to map functions to type analysis information
    // TODO
    this.funcTypes = undefined;





    /**
    Ordered list of unit-level functions to be analyzed
    */
    this.unitList = [];

    /**
    Map of functions to lists of call sites
    In the case of unit-level functions, this contains the next unit
    To be processed.
    */
    this.callerLists = new HashMap();






    /**
    Total analysis iteration count
    */
    this.itrCount = 0;
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

    var entry = irFunc.hirCFG.entry;



    // TODO: handle the function's argument types
    // Merge them into existing type set if existing


    // FIXME: for now, create an empty type set for the entry
    if (this.blockTypes.has(entry) === false)
    {
        var globalType = new TypeDesc(
            TypeFlags.OBJECT,
            undefined,
            undefined,
            undefined,
            [new MapDesc(this.globalClass)]
        );

        var typeMap = new TypeMap;
        typeMap.setGlobalType(globalType);
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

    // Queue the unit function to be analyzed
    this.queueFunc(ir);

    // Add the unit to the list of units to be analyzed
    this.unitList.push(ir);


    // TODO: handle caller list


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

            var useType = typeMap.getValType(use);
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
            typeMap.setValType(instr, outType);
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
    return instr.typeProp(this, typeMap);
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
    var propName = typeMap.getValType(this.uses[1]).stringVal();
    var valType = typeMap.getValType(this.uses[2]);

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

GetGlobalInstr.prototype.typeProp = function (ta, typeMap)
{
    var globalType = typeMap.getGlobalType();
    var propName = typeMap.getValType(this.uses[1]).stringVal();

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
    var t0 = typeMap.getValType(this.uses[0]);
    var t1 = typeMap.getValType(this.uses[1]);

    if (t0.flags === TypeFlags.STRING || t1.flags === TypeFlags.STRING)
    {
        print('string output');
        print(t0);
        print(t1);
        return TypeDesc.string;
    }

    return new TypeDesc(TypeFlags.INT | TypeFlags.FLOAT | TypeFlags.STRING);
}


// TODO: handle more instructions!










