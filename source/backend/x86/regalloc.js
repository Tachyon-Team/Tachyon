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
@class Per-instruction register allocation hints/constraints
*/
x86.RegAllocCfg = function ()
{
}

/**
Maximum number of memory operands to use
*/
x86.RegAllocCfg.prototype.maxMemOpnds = function (instr)
{
    return 1;
}

/**
Number of scratch registers wanted
*/
x86.RegAllocCfg.prototype.numScratchRegs = function (instr)
{
    return 0;
}

/**
Registers this instruction will have to write to, excluding
scratch registers, operands and the destination.
*/
x86.RegAllocCfg.prototype.writeRegSet = function (opIdx)
{
    return null;
}

/**
Indicates that an operand must be placed in a register
*/
x86.RegAllocCfg.prototype.opndMustBeReg = function (opIdx)
{
    return false;
}

/**
Set of registers an operand can be assigned to. This
may be a single register.
*/
x86.RegAllocCfg.prototype.opndRegSet = function (opIdx)
{
    return null;
}

/**
Dest is operand 0.
*/
x86.RegAllocCfg.prototype.destIsOpnd0 = function (instr)
{
    return true;
}

/**
Indicates that the destination must be placed in a register
*/
x86.RegAllocCfg.prototype.destMustBeReg = function (opIdx)
{
    return false;
}

/**
Set of registers the destination can be assigned to.
This may be a single register.
*/
x86.RegAllocCfg.prototype.destRegSet = function (opIdx)
{
    return null;
}

/**
Perform register allocation on an IR function
*/
x86.allocRegs = function ()
{
    // TODO

    // TODO: should we generate the code as the register allocation
    // is being performed?
    // Can always store operands in a big array. The instructions are
    // ordered at this point. Otherwise, simply map them by instr id. ***









}

