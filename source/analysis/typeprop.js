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

/*
TODO: IR refactorings

- Remove instrid, blockid?
- Try using field on instr, blocks for analysis data, time result
  - Need to nullify field after analysis...

- Eliminate unique instruction naming scheme
  - Name on printout only
  - Eliminates complex and slow naming logic

- Hash based on serial number if ever needed

- Store instructions and blocks into linked lists
  - Time before & after... Especially peephole part of frontend

*/

/*
TODO: basic interprocedural type analysis

Need type descriptor:
- Describe types of globals
- Describe field types
- Describe array types
- Describe SSA temp types
- Fn input and return types

Need way to init initial type desc
- Call fn to initialize?
- Initialize to what? Could create some basic initial type descs for
  common types.

Need way to compute intersection of type descs
- Produces a new type desc
*/

/**
@namespace Type descriptor flags namespace
*/
TypeDesc.Flags = {};

// Possible type descriptor flags
TypeDesc.Flags.UNDEF    = 1 << 0; // May be undefined
TypeDesc.Flags.NULL     = 1 << 1; // May be null
TypeDesc.Flags.TRUE     = 1 << 2; // May be true
TypeDesc.Flags.FALSE    = 1 << 3; // May be false
TypeDesc.Flags.FLOAT    = 1 << 4; // May be floating-point
TypeDesc.Flags.INT      = 1 << 5; // May be integer
TypeDesc.Flags.STRING   = 1 << 6; // May be string
TypeDesc.Flags.OBJECT   = 1 << 7; // May be string
TypeDesc.Flags.ARRAY    = 1 << 8; // May be string
TypeDesc.Flags.FUNCTION = 1 << 9; // May be string

/**
@class Describes variable or temporary types in the type propagation analysis.
*/
function TypeDesc(
    flags,
    classIdx
)
{
    /**
    Descriptor flags bit field
    */
    this.flags = flags;

    /**
    Numerical range minimum. Undefined if unknown.
    */
    this.minVal = minVal;

    /**
    Numerical range maximum. Undefined if unknown.
    */
    this.maxVal = maxVal;

    /**
    Pseudo-class index. Undefined if unknown.
    */
    this.classIdx = classIdx;
}

// TODO: unknown/any type

// TODO: uninferred type (NoInf)

// TODO: basic int, str, obj types

// TODO: union function (bitwise OR?)






