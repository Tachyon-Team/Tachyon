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
Register allocation for x86 code generation.

@author
Maxime Chevalier-Boisvert
*/

/**
@class Stack frame map. Maps temporaries to stack frame locations.
*/
x86.StackFrameMap = function (slotSize)
{
    assert (
        slotSize === 4 || slotSize === 8,
        'invalid slot size'
    );

    /**
    @field Stack frame slot size, in bytes
    */
    this.slotSize = slotSize;

    /**
    @field Map of arguments to slot indices
    */
    this.argMap = [];

    /**
    @field Slot index of the return address
    */
    this.retAddrSlot = undefined;

    /**
    @field Map of callee-save registers to slot indices
    */
    this.saveRegMap = {};

    /**
    @field Map of spill slots to stack slot indices
    */
    this.spillMap = [];

    /**
    @field Total number of stack frame slots
    */
    this.numSlots = 0;

    /**
    @field Number of callee-save register slots
    */
    this.numSaveSlots = 0;

    /**
    @field Offsets for stack slots
    */
    this.slotOffsets = [];
}

/**
Produce a string representation of the stack frame map
*/
x86.StackFrameMap.prototype.toString = function ()
{
    var str = '';

    //
    // TODO
    //

    return str;
}

/**
Compute the offsets for stack locations
*/
x86.StackFrameMap.prototype.compOffsets = function ()
{
    this.slotOffsets.length = this.numSlots;

    var curOffset = 0;

    // For each stack frame location, in reverse order
    for (var i = this.numSlots - 1; i >= 0; --i)
    {
        this.slotOffsets[i] = curOffset;

        curOffset += this.slotSize;
    }
}

/**
Get the size of portion of the stack frame used for spills
*/
x86.StackFrameMap.prototype.getSpillSize = function ()
{
    //print('ret addr offset: ' + this.getSlotOffset(this.retAddrSlot));

    return (this.spillMap.length + this.numSaveSlots) * this.slotSize;
}

/**
Allocate a stack location for the return address
*/
x86.StackFrameMap.prototype.allocArg = function (argIdx)
{
    assert (
        isNonNegInt(argIdx) && this.argMap[argIdx] === undefined,
        'invalid argument index: ' + argIdx
    );

    var locIdx = this.numSlots;

    this.argMap[argIdx] = locIdx;

    this.numSlots += 1;
}

/**
Allocate a stack location for the return address
*/
x86.StackFrameMap.prototype.allocRetAddr = function ()
{
    assert (
        this.retAddrSlot === undefined,
        'return address mapped on stack'
    );

    var locIdx = this.numSlots;

    this.retAddrSlot = locIdx;

    this.numSlots += 1;
}

/**
Allocate a stack location for a callee-save register
*/
x86.StackFrameMap.prototype.allocSaveReg = function (reg)
{
    assert (
        reg instanceof x86.Register,
        'invalid register'
    );

    var locIdx = this.numSlots;

    this.saveRegMap[reg.name] = locIdx;

    this.numSlots += 1;
    this.numSaveSlots += 1;
}

/**
Get the stack slot for a given spill.
*/
x86.StackFrameMap.prototype.getSpillSlot = function (spillIdx)
{
    assert (
        spillIdx <= this.spillMap.length,
        'invalid spill slot index'
    );

    if (spillIdx < this.spillMap.length)
        return this.spillMap[spillIdx];

    var locIdx = this.numSlots;

    this.spillMap[this.spillMap.length] = locIdx;

    this.numSlots += 1;

    return locIdx;
}

/**
Get the stack slot for a given argument
*/
x86.StackFrameMap.prototype.getArgSlot = function (argIdx)
{
    assert (
        argIdx < this.argMap.length,
        'invalid argument index'
    );

    return this.argMap[argIdx];
}

/**
Get the stack slot for callee-save register
*/
x86.StackFrameMap.prototype.getRegSlot = function (reg)
{
    assert (
        reg.name in this.saveRegMap,
        'invalid callee-save register: ' + reg
    );

    return this.saveRegMap[reg.name];
}

/**
Get the offset for a stack slot
*/
x86.StackFrameMap.prototype.getSlotOffset = function (slotIdx)
{
    assert (
        slotIdx < this.numSlots,
        'invalid stack slot idx: ' + slotIdx
    );

    return this.slotOffsets[slotIdx]
}

/**
@class Register allocation map. Maps temporaries and constants to
registers and stack locations at a given point within a function.
*/
x86.RegAllocMap = function ()
{
    /**
    @field Map of register numbers to values
    */
    this.regMap = [];

    /**
    @field Map of stack slots to values
    */
    this.stackMap = new HashMap();

    /**
    @field Number of spill slots used
    */
    this.numSpills = 0;

    /**
    @field Map of values to list of current allocations
    */
    this.allocMap = new HashMap();
}

/**
Copy an allocation map
*/
x86.RegAllocMap.prototype.copy = function ()
{
    // Create a new reg alloc map
    var newMap = new x86.RegAllocMap();

    // Copy the GP register map
    newMap.regMap.length = this.regMap.length;
    for (var i = 0; i < this.regMap.length; ++i)
        newMap.regMap[i] = this.regMap[i];

    // Copy the stack slot map
    newMap.stackMap = this.stackMap.copy();

    // Copy the number of spills
    newMap.numSpills = this.numSpills;

    // Copy the alloc map
    for (var itr = this.allocMap.getItr(); itr.valid(); itr.next())
    {
        var item = itr.get();
        var val = item.key;
        var list = item.value;

        var newList = new Array(list.length);
        for (var i = 0; i < list.length; ++i)
            newList[i] = list[i];

        newMap.allocMap.setItem(val, newList);
    }

    // Return the copy
    return newMap;
}

/**
Produce a string representation of an allocation map
*/
x86.RegAllocMap.prototype.toString = function ()
{
    var str = '';

    str += 'GP registers:';

    for (var regNo = 0; regNo < this.regMap.length; ++regNo)
    {        
        var value = this.regMap[regNo];

        if (value === undefined)
            continue;

        str += '\nreg' + regNo + ' => ' + value.getValName();
    }

    return str;
}

/**
Allocate a value to a register or spill slot
*/
x86.RegAllocMap.prototype.makeAlloc = function (value, alloc)
{
    assert (
        value === undefined || value instanceof IRValue,
        'invalid value in allocReg: ' + value
    );

    assert (
        isNonNegInt(alloc) ||
        (alloc instanceof x86.Register && alloc.type === 'gp'),
        'invalid allocation: ' + alloc
    );

    log.debug(alloc + ' -> ' + (value? value.getValName():undefined));

    // If the allocation is to a register
    if (alloc instanceof x86.Register)
    {
        // Get the value the register previously mapped to
        var prevRegVal = this.regMap[alloc.regNo];

        // Remove the allocation of the previous value to the register
        if (prevRegVal !== undefined)
            this.remAlloc(prevRegVal, alloc);

        // Update the register's value
        this.regMap[alloc.regNo] = value;
    }
    else
    {
        // Get the value the stack slot previously mapped to
        var prevSlotVal = this.stackMap[alloc];

        // Remove the allocation of the previous value to the spill slot
        if (prevSlotVal !== undefined)
            this.remAlloc(prevSlotVal, alloc);

        // Update the stack slot's value
        this.stackMap.setItem(alloc, value);
    }

    // Get the alloc set for this value
    var allocSet;
    if (this.allocMap.hasItem(value) === false)
    {
        var allocSet = [];
        this.allocMap.addItem(value, allocSet);
    }
    else
    {
        allocSet = this.allocMap.getItem(value);
    }

    // Add the new allocation to the set
    arraySetAdd(allocSet, alloc);
}

/**
Remove a value from the allocation map
*/
x86.RegAllocMap.prototype.remAlloc = function (value, alloc)
{
    assert (
        value instanceof IRValue,
        'invalid value in remAlloc: ' + value
    );

    if (this.allocMap.hasItem(value) === false)
        return;

    var allocSet = this.allocMap.getItem(value);

    assert (
        arraySetHas(allocSet, alloc),
        'allocation not in set: ' + alloc
    );

    arraySetRem(allocSet, alloc);

    if (alloc instanceof x86.Register)
        this.regMap[alloc.regNo] = undefined;
    else
        this.stackMap.setItem(alloc, undefined);
}

/**
Remove all allocations for a value from the allocation map
*/
x86.RegAllocMap.prototype.remAllocs = function (value)
{
    assert (
        value instanceof IRValue,
        'invalid value in remAllocs: ' + value
    );

    if (this.allocMap.hasItem(value) === false)
        return;

    var allocSet = this.allocMap.getItem(value);

    for (var i = 0; i < allocSet.length; ++i)
    {
        var alloc = allocSet[i];

        if (alloc instanceof x86.Register)
            this.regMap[alloc.regNo] = undefined;
        else
            this.stackMap.setItem(alloc, undefined);
    }

    allocSet.length = 0;
}

/**
Find a free spill slot and allocate a value to it. Returns
the corresponding stack slot index.
*/
x86.RegAllocMap.prototype.spillValue = function (value, stackMap, liveSet)
{
    var stackSlot = undefined;

    // For each stack slot
    for (var itr = this.stackMap.getItr(); itr.valid(); itr.next())
    {
        var itrVal = itr.get();
        var alloc = itr.key;
        var curVal = itr.value;

        // If this spill slot is free, or assigned to a dead value
        if (curVal === undefined || liveSet.hasItem(curVal) === false)
        {
            stackSlot = alloc;
            break;
        }
    }

    // If no free stack slot was found
    if (stackSlot === undefined)
    {
        // Get a new spill slot from the stack frame map
        stackSlot = stackMap.getSpillSlot(this.numSpills);
        this.numSpills += 1;
    }

    // Allocate the value to the stack slot
    this.makeAlloc(value, stackSlot);

    return stackSlot;
}

/**
Get the value a register maps to
*/
x86.RegAllocMap.prototype.getRegVal = function (reg)
{
    assert (
        reg instanceof x86.Register && reg.type === 'gp',
        'invalid register'
    );

    return this.regMap[reg.regNo];
}

/**
Get the allocation set for a value.
*/
x86.RegAllocMap.prototype.getAllocs = function (value)
{
    assert (
        value instanceof IRValue,
        'invalid value in getAlloc: ' + value
    );

    if (this.allocMap.hasItem(value) === true)
        return this.allocMap.getItem(value);
    else
        return [];
}

/**
Compute the size of a constant value if it were to be
used as an immediate.
*/
x86.getImmSize = function (value, params)
{
    // If the value is not a constant, stop
    if ((value instanceof ConstValue) === false)
        return undefined;

    // If the value is not an immediate integer, stop
    if (value.isInt() === false &&
        value.value !== undefined &&
        value.value !== true &&
        value.value !== false &&
        value.value !== null)
        return undefined;

    // Get the immediate bits for the value
    var immVal = value.getImmValue(params);

    // Compute the smallest size this immediate fits in
    var size;
    if (num_ge(immVal, getIntMin(8)) && num_le(immVal, getIntMax(8)))
        size = 8;
    else if (num_ge(immVal, getIntMin(16)) && num_le(immVal, getIntMax(16)))
        size = 16;
    else if (num_ge(immVal, getIntMin(32)) && num_le(immVal, getIntMax(32)))
        size = 32;
    else
        size = 64;

    // Return the size
    return size;
}

/**
Test if an IR value must be handled at link-time
*/
x86.isLinkValue = function (value)
{
    return (
        value instanceof IRFunction ||
        (value instanceof ConstValue && value.isString())
    );
}







