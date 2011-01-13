/**
@fileOverview
Implementation of high-level IR instructions through handler functions

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010-2011 Maxime Chevalier-Boisvert, All Rights Reserved
*/

/**
@class Represents constant values in the IR
@augments IRValue
*/
function ConstValue(value, type)
{
    // Ensure that the specified value is valid
    assert (
        !(type.isInt() && 
        (typeof value != 'number' || Math.floor(value) != value)),
        'integer constants require integer values'
    );
    assert (
        !(type.isFP() && typeof value != 'number'),
        'floating-point constants require number values'
    );
    assert (
        !(typeof value == 'string' && type !== IRType.box),
        'string-valued constants must have box type'
    );
    assert (
        !(type === IRType.bool && value != 0 && value != 1),
        'boolean constants must be 0 or 1'
    );        

    /**
    Value of the constant
    @field
    */
    this.value = value;

    /**
    Type of the constant
    @field
    */
    this.type = type;
}
ConstValue.prototype = new IRValue();

/**
Get a string representation of a constant instruction
*/
ConstValue.prototype.toString = function ()
{
    if (typeof this.value == 'string')
    {
       return '"' + escapeJSString(this.value) + '"';
    }
    else if (typeof this.value == 'number')
    {
        return this.type + ':' + String(this.value);
    }
    else if (this.value instanceof Function)
    {
        if (this.value.hasOwnProperty('name'))
            return this.value.name;
        else
            return 'function';
    }
    else
    {
        return String(this.value);
    }
};

/**
Get a string representation of an instruction's value/name.
Returns the constant's string representation directly.
*/
ConstValue.prototype.getValName = ConstValue.prototype.toString;

/**
Test if a constant is a number
*/
ConstValue.prototype.isNumber = function ()
{
    return (typeof this.value == 'number');
};

/**
Test if a constant is an integer
*/
ConstValue.prototype.isInt = function ()
{
    return (this.isNumber() && this.value == Math.floor(this.value));
};

/**
Test if a constant is a boxed integer
*/
ConstValue.prototype.isBoxInt = function (params)
{
    assert (params instanceof CompParams);

    var BOX_NUM_BITS_INT = params.staticEnv.getBinding('BOX_NUM_BITS_INT').value;

    return (
        this.type === IRType.box &&
        this.isInt() && 
        this.value >= getIntMin(BOX_NUM_BITS_INT, false) && 
        this.value <= getIntMax(BOX_NUM_BITS_INT, false)
    );
};

/**
Test if a constant is a string
*/
ConstValue.prototype.isString = function ()
{
    return (typeof this.value == 'string');
};

/**
Test if a constant is the undefined constant
*/
ConstValue.prototype.isUndef = function ()
{
    return this.value === undefined;
};

/**
Get the tag bits associated with a constant
*/
ConstValue.prototype.getTagBits = function (params)
{
    assert (
        params instanceof CompParams,
        'compilation parameters expected'
    );

    assert (
        this.type === IRType.box,
        'tag bits only applicable to boxed values'
    );

    if (this.value instanceof IRFunction)
    {
        return params.staticEnv.getBinding('TAG_FUNCTION').value;
    }

    if (this.isBoxInt(params))
    {
        return params.staticEnv.getBinding('TAG_INT').value;
    }

    if (this.isNumber())
    {
        return params.staticEnv.getBinding('TAG_FLOAT').value;
    }

    if (typeof this.value == 'string')
    {
        return params.staticEnv.getBinding('TAG_STRING').value;
    }

    if (this.value === true || 
        this.value === false ||
        this.value === null || 
        this.value === undefined)
    {
        return params.staticEnv.getBinding('TAG_OTHER').value;
    }

    assert (
        false,
        'cannot get tag bits for: ' + this
    );
};

/**
Get the immediate value (bit pattern) of a constant
*/
ConstValue.prototype.getImmValue = function (params)
{
    assert (params instanceof CompParams);

    if (this.isBoxInt(params))
    {
        return (this.value << params.staticEnv.getBinding('TAG_NUM_BITS_INT').value);
    }

    if (this.type.isInt() && this.type !== IRType.box)
    {
        return this.value;
    }

    if (this.type === IRType.rptr)
    {
        return this.value;
    }

    if (this.type === IRType.bool)
    {
        return this.value;
    }

    if (this.value === true)
    {
        return params.staticEnv.getBinding('BIT_PATTERN_TRUE').value;
    }

    if (this.value === false)
    {
        return params.staticEnv.getBinding('BIT_PATTERN_FALSE').value;
    }

    if (this.value === undefined)
    {
        return params.staticEnv.getBinding('BIT_PATTERN_UNDEF').value;
    }

    if (this.value === null)
    {
        return params.staticEnv.getBinding('BIT_PATTERN_NULL').value;
    }

    assert (
        false,
        'cannot get immediate bits for: ' + this
    );
};

/**
Map of values to maps of types to IR constants
*/
ConstValue.constMap = new HashMap();

/**
Get the unique constant instance for a given value
*/
ConstValue.getConst = function (value, type)
{
    // The default type is boxed
    if (type === undefined)
        type = IRType.box;

    // If there is no type map for this value
    if (!ConstValue.constMap.hasItem(value))
    {
        // Create a new hash map to map types to constants
        var typeMap = new HashMap();
        ConstValue.constMap.addItem(value, typeMap);
    }
    else
    {
        var typeMap = ConstValue.constMap.getItem(value);
    }

    // If there is no constant for this type
    if (!typeMap.hasItem(type))
    {
        // Create a new constant with the specified type
        var constant = new ConstValue(value, type);
        typeMap.addItem(type, constant);
    }
    else
    {
        var constant = typeMap.getItem(type);
    }

    // Return the constant
    return constant;
};

