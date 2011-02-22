/**
@fileOverview
Definition of Intermediate Representation (IR) instructions types.

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010-2011 Maxime Chevalier-Boisvert, All Rights Reserved
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
    return this === IRType.u8  ||
           this === IRType.u16 ||
           this === IRType.u32 ||
           this === IRType.u64;
};

/**
Test if the type is a signed integer type
*/
IRType.prototype.isSigned = function ()
{
    return this === IRType.i8  ||
           this === IRType.i16 ||
           this === IRType.i32 ||
           this === IRType.i64 ||
           this === IRType.pint;
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
IRType.prototype.getSizeBytes = function (target)
{
    assert (
        target instanceof Target,
        'expected compilation target'
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
        case IRType.bool:
        case IRType.pint:
        return target.ptrSizeBytes;
    }
};

/**
Get the type size in bits
*/
IRType.prototype.getSizeBits = function (target)
{
    return this.getSizeBytes(target) * 8;
};

/**
Get the minimum value this type can represent
*/
IRType.prototype.getMinVal = function (target)
{
    // If this is an integer type
    if (this.isInt())
    {
        // Compute the minimum value
        return getIntMin(this.getSizeBits(target), this.isUnsigned());
    }

    // Otherwise, if this is the boxed type
    else if (this === IRType.box)
    {
        // TODO: make this infinity when supported, boxed
        // values can be floats
        return getIntMin(30, false);
    }
};

/**
Get the maximum value this type can represent
*/
IRType.prototype.getMaxVal = function (target)
{
    // If this is an integer type
    if (this.isInt())
    {
        // Compute the maximum value
        return getIntMax(this.getSizeBits(target), this.isUnsigned());
    }

    // Otherwise, if this is the boxed type
    else if (this === IRType.box)
    {
        // TODO: make this infinity when supported, boxed
        // values can be floats
        return getIntMax(30, false);
    }

};

/**
Test if an integer value is in the range supported by this type.
*/
IRType.prototype.valInRange = function (val, target)
{
    assert (
        this.isInt() || this === IRType.box,
        'valInRange only applies to integer and boxed types'
    );

    return (
        num_ge(val, this.getMinVal(target, this.isUnsigned())) && 
        num_le(val, this.getMaxVal(target, this.isUnsigned()))
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

// Boolean type
IRType.bool = new IRType('bool');

// Integer type of width specific to the platform
// (same width as the pointer size)
IRType.pint = new IRType('pint');

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

