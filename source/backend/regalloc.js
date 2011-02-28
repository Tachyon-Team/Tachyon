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

    return that;
};

/**
@class Register allocation information for an instruction object
*/
regAlloc.instr = function ()
{
    var that = Object.create(regAlloc.instr.prototype);

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
regAlloc.instr.prototype.outRegHint = function (instr, params) 
{ 
    return null; 
};

/** Tells if operands of an instruction must be in registers */
regAlloc.instr.prototype.opndsRegRequired = false;

/** List all register indices used by a given instruction */
regAlloc.instr.prototype.usedRegisters = function (instr, params) 
{ 
    return null; 
};

/********* End of Interface to the register allocation algorithm  *************/
