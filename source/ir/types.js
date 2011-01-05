/**
@fileOverview
Definition of Intermediate Representation (IR) instructions types.

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010 Maxime Chevalier-Boisvert, All Rights Reserved
*/

/**
@class IR type representation object
*/
function IRType(name, size)
{
    /**
    Name of this type
    @field
    */
    this.name = name;

    /**
    Type size in bytes
    @field
    */
    this.size = size;

    /**
    Type size in bits
    @field
    */
    this.numBits = size * 8;

    // If this is an integer type
    if (name.charAt(0) == 'i' || name.charAt(0) == 'u')
    {
        // Compute the available range
        this.minVal = getIntMin(this.numBits, name.charAt(0) == 'u');
        this.maxVal = getIntMax(this.numBits, name.charAt(0) == 'u');
    }

    // Otherwise, if this is the boxed type
    else if (name == 'box')
    {
        // TODO: make this infinity when supported, boxed
        // values can be floats
        this.minVal = getIntMin(30, false);
        this.maxVal = getIntMax(30, false);
    }
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
           this === IRType.i64;
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
    return this.isInt() ||
           this.isFP();
};

// TODO: boxed and pointer type sizes are actually platform-dependent
// Need code get appropriate size for the platform


// FIXME: the pointer size depends on the target, but the target may be
// different depending on the platform. Can't just define pint, box,
// rptr globally. They depend on the target...
//
//


// Type given when there is no output value
IRType.none = new IRType('none', 0),

// Boxed value type
// Contains an immediate integer or an object pointer, and a tag
IRType.box  = new IRType('box' , config.target.ptrSizeBytes),

// Raw pointer to any memory address
IRType.rptr = new IRType('rptr', config.target.ptrSizeBytes),

// Boolean type
IRType.bool = new IRType('bool', config.target.ptrSizeBytes),

// Unboxed unsigned integer types
IRType.u8   = new IRType('u8'  , 1),
IRType.u16  = new IRType('u16' , 2),
IRType.u32  = new IRType('u32' , 4),

// Unboxed signed integer types
IRType.i8   = new IRType('i8'  , 1),
IRType.i16  = new IRType('i16' , 2),
IRType.i32  = new IRType('i32' , 4),

// Floating-point types
IRType.f64  = new IRType('f64' , 8);

// If we are on a 32-bit platform
if (config.target.ptrSizeBits == 32)
{
    // Int type of width corresponding a pointer on this platform
    IRType.pint = IRType.i32;
}

// Otherwise, we are on a 64-bit platform
else
{
    IRType.u64  = new IRType('u64' , 8),
    IRType.i64  = new IRType('i64' , 8),

    // Int type of width corresponding a pointer on this platform
    IRType.pint = IRType.i64;
}

