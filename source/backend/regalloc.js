/**
@fileOverview

Data structures holding information about register allocation used by
the backend. A specific register allocation algorithm might extend
those data structures for specific needs.

@copyright
Copyright (c) 2010 Tachyon Javascript Engine, All Rights Reserved
*/

/** @namespace */
var regAlloc = {};

/**
@class Register allocation information for a function
*/
regAlloc.fct = function ()
{
    var that = Object.create(regAlloc.fct.prototype);

    /**
    Maximum number of spills made by this function
    @field 
    */
    that.spillNb = 0;

    /**
    Temporary slot 
    */
    that.temp = null;

    return that;
};

/**
@class Register allocation information for a control-flow graph object
*/
regAlloc.cfg = function ()
{
    return Object.create(regAlloc.cfg.prototype);
};

/**
@class Register allocation information for a block object
*/
regAlloc.block = function ()
{
    var that = Object.create(regAlloc.block.prototype);

    /**
    Position at the beginning of the block
    @field
    */
    that.from = -1;

    return that;
};

/**
@class Register allocation information for an instruction object
*/
regAlloc.instr = function ()
{
    var that = Object.create(regAlloc.instr.prototype);

    /**
    Position of the instruction
    @field
    */
    that.id = null;

    /**
    Operand list for this instruction.  Should be either a register,
    a memory location or an IRValue.
    @field
    */
    that.opnds = null;

    /**
    Destination for this instruction. Should be a register, a memory
    location or null.
    @field
    */
    that.dest = null;

    return that;
};

/********* Interface to the register allocation algorithm  ********************/

/*
*   Register indexes are indexes into the physical register list containing 
*   the explicit platform specific registers.
*/

/**
Returns a register index preference for a given operand position
*/
regAlloc.instr.prototype.opndsRegHint = function (instr, params, position)
{
    return null;
};

/** Returns a register index preference for the output value */
regAlloc.instr.prototype.outRegHint = function (instr, config) 
{ 
    return null; 
};

/** Tells if operands of an instruction must be in registers */
regAlloc.instr.prototype.opndsRegRequired = false;

/** List all register indices used by a given instruction */
regAlloc.instr.prototype.usedRegisters = function (instr, config) 
{ 
    return null; 
};

/********* End of Interface to the register allocation algorithm  *************/
