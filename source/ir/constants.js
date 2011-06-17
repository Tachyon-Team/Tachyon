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
Implementation of high-level IR instructions through handler functions

@author
Maxime Chevalier-Boisvert
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
        (num_instance(value) !== true || num_integer(value) !== true)),
        'integer constants require integer values'
    );
    assert (
        !(type.isFP() && typeof value !== 'number'),
        'floating-point constants require number values'
    );
    assert (
        !(typeof value === 'string' && type !== IRType.box),
        'string-valued constants must have box type'
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
    if (typeof this.value === 'string')
    {
       return '"' + escapeJSString(this.value) + '"';
    }
    else if (num_instance(this.value))
    {
        return this.type + ':' + num_to_string(this.value);
    }
    else if (this.value instanceof Function)
    {
        if (this.value.hasOwnProperty('name'))
            return this.value.name;
        else
            return 'function';
    }
    else if (this.value === undefined)
    {
        return 'undef';
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
    return (num_instance(this.value) === true);
};

/**
Test if a constant is an integer
*/
ConstValue.prototype.isInt = function ()
{
    return (this.isNumber() && num_integer(this.value));
};

/**
Test if a constant is a float
*/
ConstValue.prototype.isFloat = function ()
{
    return (this.isNumber() && !num_integer(this.value));
};

/**
Test if a constant is a boxed integer
*/
ConstValue.prototype.isBoxInt = function (params)
{
    assert (
        params instanceof CompParams,
        'expected compilation parameters'
    );

    var BOX_NUM_BITS_INT = params.staticEnv.getBinding('BOX_NUM_BITS_INT').value;

    return (
        this.type === IRType.box &&
        this.isInt() && 
        this.type.valInRange(this.value, params)
    );
};

/**
Test if a constant is a string
*/
ConstValue.prototype.isString = function ()
{
    return (typeof this.value === 'string');
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

    if (typeof this.value === 'string')
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
    assert (
        params instanceof CompParams,
        'expected compilation parameters'
    );

    if (this.isBoxInt(params))
    {
        return num_shift(this.value, params.staticEnv.getBinding('TAG_NUM_BITS_INT').value);
    }

    if (this.type.isInt() && this.type !== IRType.box)
    {
        return this.value;
    }

    if (this.type === IRType.rptr)
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

    error('cannot get immediate bits for: ' + this);
};

/**
Hash function to be used for the constant map.
*/
ConstValue.hashFunc = function (val)
{
    if (bignum_instance(val))
        return defHashFunc(bignum_to_string(val));
    else
        return defHashFunc(val);
};

/**
Equality function to be used for the constant map.
*/
ConstValue.equalFunc = function (key1, key2)
{
    if (num_instance(key1) && num_instance(key2))
        return num_eq(key1, key2);
    else
        return defEqualFunc(key1, key2);
};

/**
Map of values to maps of types to IR constants
*/
ConstValue.constMap = new HashMap(ConstValue.hashFunc, ConstValue.equalFunc);

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

