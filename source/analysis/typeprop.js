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

// Unknown/any type flag
TypeDesc.Flags.ANY =
    TypeDesc.Flags.UNDEF    |
    TypeDesc.Flags.NULL     |
    TypeDesc.Flags.TRUE     |
    TypeDesc.Flags.FALSE    |
    TypeDesc.Flags.INT      |
    TypeDesc.Flags.FLOAT    |
    TypeDesc.Flags.STRING   |
    TypeDesc.Flags.OBJECT   |
    TypeDesc.Flags.ARRAY    |
    TypeDesc.Flags.FUNCTION;

// Uninferred type flag (before analysis)
TypeDesc.Flags.NOINF = 0;

/**
@class Describes variable or temporary types in the type propagation analysis.
*/
function TypeDesc(
    flags,
    minVal,
    maxVal,
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

/**
Generate a type descriptor for a constant value
*/
TypeDesc.constant = function (value)
{
    if (value instanceof IRConst)
        value = IRConst.value;

    if (isInt(value) === true)
    {
        return new TypeDesc(TypeDesc.Flags.INT, value, value);
    }

    else if (typeof value === 'number')
    {
        return new TypeDesc(TypeDesc.Flags.FLOAT, value, value);
    }

    else if (typeof value === 'string')
    {
        return TypeDesc.String;
    }

    // TODO: handle other types
    return TypeDesc.Any;
}

/**
Produce a string representation of a type descriptor
*/
TypeDesc.prototype.toString = function ()
{
    if (this.flags === TypeDesc.Flags.NOINF)
        return "noinf";

    if (this.flags === TypeDesc.Flags.ANY)
        return "any";

    var str = "";

    // Add the flags
    for (flagName in TypeDesc.Flags)
    {
        var flagVal = TypeDesc.Flags[flagName];

        if (this.flags & flagVal)
        {
            if (str != "")
                str += ",";

            str += flagName.toLowerCase();
        }
    }

    // If range information is present
    if (this.minVal !== undefined || this.maxVal !== undefined)
    {
        if (this.minVal !== undefined && this.minVal === this.maxVal)
            str += " " + this.minVal;
        else if (this.minVal !== undefined && this.maxVal !== undefined)
            str += " [" + this.minVal + ", " + this.maxVal + "]";
        else if (this.minVal === undefined)
            str += " ]-inf, " + this.maxVal + "]";
        else
            str += " [" + this.minVal + ", +inf[";
    }

    // If class information is present
    if (this.classIdx !== undefined)
    {
        str += " class:" + this.classIdx;
    }

    return str;
}

//
// TODO: isFixnum? with params/target
//

/**
Type descriptor union (OR) function
*/
TypeDesc.prototype.union = function (that)
{
    var flags = this.flags | that.flags;

    var minVal =
        (this.minVal !== undefined && that.minVal !== undefined)?
        Math.min(this.minVal, that.minVal):undefined;

    var maxVal =
        (this.maxVal !== undefined && that.maxVal !== undefined)?
        Math.max(this.maxVal, that.maxVal):undefined;

    var classIdx =
        (this.classIdx === that.classIdx)?
        this.classIdx:undefined;

    return new TypeDesc(
        flags,
        minVal,
        maxVal,
        classIdx
    );
}

// Any type descriptor
TypeDesc.Any = new TypeDesc(TypeDesc.Flags.ANY);

// Uninferred type descriptor
TypeDesc.Noinf = new TypeDesc(TypeDesc.Flags.NOINF);

// Generic number type descriptor
TypeDesc.Number = new TypeDesc(TypeDesc.Flags.INT | TypeDesc.Flags.FLOAT);

// Generic string type descriptor
TypeDesc.String = new TypeDesc(TypeDesc.Flags.STRING);

// Generic object type descriptor
TypeDesc.Object = new TypeDesc(TypeDesc.Flags.OBJECT);




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


/*
TODO: Analysis code, SCCP-based
- Ignore object classes for now?

Need info about globals... The global obj is an object...
- Special handling or just store its current map/class descriptor??
- Need some way of describing its fields



*/










