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
Definition of Intermediate Representation (IR) instructions types.

@author
Maxime Chevalier-Boisvert
*/

/**
@class IR type representation object
*/
function IRType(name)
{
    /**
    Name of this type
    @field
    */
    this.name = name;
}
IRType.prototype = {};

/**
Obtain a string representation of an IR type
*/
IRType.prototype.toString = function ()
{
    return this.name;
};

/**
Test if the type is a pointer type
*/
IRType.prototype.isPtr = function ()
{
    return this === IRType.rptr ||
           this === IRType.ref  ||
           this === IRType.box;
};

/**
Test if the type is an integer type
*/
IRType.prototype.isInt = function ()
{
    return this.isUnsigned() ||
           this.isSigned();
};

/**
Test if the type is an unsigned integer type
*/
IRType.prototype.isUnsigned = function ()
{
    return (
        this === IRType.u8  ||
        this === IRType.u16 ||
        this === IRType.u32 ||
        this === IRType.u64 ||
        this === IRType.puint
    );
};

/**
Test if the type is a signed integer type
*/
IRType.prototype.isSigned = function ()
{
    return (
        this === IRType.i8  ||
        this === IRType.i16 ||
        this === IRType.i32 ||
        this === IRType.i64 ||
        this === IRType.pint
    );
};

/**
Test if the type is a floating-point type
*/
IRType.prototype.isFP = function ()
{
    return this === IRType.f64;
};

/**
Test if the type is an integer or floating-point type
*/
IRType.prototype.isNumber = function ()
{
    return this.isInt() || this.isFP();
};

/**
Get the type size in bytes
*/
IRType.prototype.getSizeBytes = function (params)
{
    assert (
        params instanceof CompParams,
        'expected compilation parameters'
    );

    switch (this)
    {
        case IRType.none:
        return 0;

        case IRType.i8:
        case IRType.u8:
        return 1;

        case IRType.i16:
        case IRType.u16:
        return 2;

        case IRType.i32:
        case IRType.u32:
        return 4;

        case IRType.i64:
        case IRType.u64:
        case IRType.f64:
        return 8;

        // These types take the size of the pointer on the target architecture
        case IRType.rptr:
        case IRType.ref:
        case IRType.box:
        case IRType.pint:
        case IRType.puint:
        return params.target.ptrSizeBytes;
    }
};

/**
Get the type size in bits
*/
IRType.prototype.getSizeBits = function (params)
{
    return this.getSizeBytes(params) * 8;
};

/**
Get the minimum value this type can represent
*/
IRType.prototype.getMinVal = function (params)
{
    const width  = params.staticEnv.getBinding("BOX_NUM_BITS_INT").value;

    // If this is an integer type
    if (this.isInt())
    {
        // Compute the minimum value
        return getIntMin(this.getSizeBits(params), this.isUnsigned());
    }

    // Otherwise, if this is the boxed type
    else if (this === IRType.box)
    {
        // TODO: make this infinity when supported, boxed
        // values can be floats
        return getIntMin(width, false);
    }
};

/**
Get the maximum value this type can represent
*/
IRType.prototype.getMaxVal = function (params)
{
    const width  = params.staticEnv.getBinding("BOX_NUM_BITS_INT").value;

    // If this is an integer type
    if (this.isInt())
    {
        // Compute the maximum value
        return getIntMax(this.getSizeBits(params), this.isUnsigned());
    }

    // Otherwise, if this is the boxed type
    else if (this === IRType.box)
    {
        // TODO: make this infinity when supported, boxed
        // values can be floats
        return getIntMax(width, false);
    }

};

/**
Test if an integer value is in the range supported by this type.
*/
IRType.prototype.valInRange = function (val, params)
{
    assert (
        this.isInt() || this === IRType.box,
        'valInRange only applies to integer and boxed types'
    );

    return (
        num_ge(val, this.getMinVal(params)) && 
        num_le(val, this.getMaxVal(params))
    );
};

// Type given when there is no output value
IRType.none = new IRType('none');

// Boxed value type
// Contains an immediate integer or an object pointer, and a tag
IRType.box  = new IRType('box');

// Untagged reference to a garbage collected object
IRType.ref = new IRType('ref');

// Raw pointer to any memory address
IRType.rptr = new IRType('rptr');

// Integer type of width specific to the platform
// (same width as the pointer size)
IRType.pint = new IRType('pint');

// Unsigned integer type of width specific to the platform
// (same width as the pointer size)
IRType.puint = new IRType('puint');

// Unboxed unsigned integer types
IRType.u8   = new IRType('u8');
IRType.u16  = new IRType('u16');
IRType.u32  = new IRType('u32');
IRType.u64  = new IRType('u64');

// Unboxed signed integer types
IRType.i8   = new IRType('i8');
IRType.i16  = new IRType('i16');
IRType.i32  = new IRType('i32');
IRType.i64  = new IRType('i64');

// Floating-point types
IRType.f64  = new IRType('f64');

