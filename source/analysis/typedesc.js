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
Implementation of type descriptors for the type analysis.

@author
Maxime Chevalier-Boisvert
*/

/**
@namespace Type descriptor flags namespace
*/
TypeFlags = {};

// Possible type descriptor flags
TypeFlags.UNDEF    = 1 << 0; // May be undefined
TypeFlags.NULL     = 1 << 1; // May be null
TypeFlags.TRUE     = 1 << 2; // May be true
TypeFlags.FALSE    = 1 << 3; // May be false
TypeFlags.FLOAT    = 1 << 4; // May be floating-point
TypeFlags.INT      = 1 << 5; // May be integer
TypeFlags.STRING   = 1 << 6; // May be string
TypeFlags.OBJECT   = 1 << 7; // May be string
TypeFlags.ARRAY    = 1 << 8; // May be string
TypeFlags.FUNCTION = 1 << 9; // May be string

// Unknown/any type flag
TypeFlags.ANY =
    TypeFlags.UNDEF    |
    TypeFlags.NULL     |
    TypeFlags.TRUE     |
    TypeFlags.FALSE    |
    TypeFlags.INT      |
    TypeFlags.FLOAT    |
    TypeFlags.STRING   |
    TypeFlags.OBJECT   |
    TypeFlags.ARRAY    |
    TypeFlags.FUNCTION;

// Uninferred type flag (before analysis)
TypeFlags.NOINF = 0;

/**
@class Describes variable or temporary types in the type propagation analysis.
*/
function TypeDesc(
    flags,
    minVal,
    maxVal,
    strVal,
    mapSet
)
{
    // Empty type descriptors have the uninferred type
    if (flags === undefined)
        flags = TypeFlags.NOINF;

    // By default, the numerical ranges are unbounded.
    // Otherwise, restrict them to a fixed integer range
    if (minVal === undefined)
        minVal = -Infinity;
    else if (minVal < TypeDesc.MIN_NUM_RANGE)
        minVal = -Infinity;
    if (maxVal === undefined)
        maxVal = Infinity;
    else if (maxVal > TypeDesc.MAX_NUM_RANGE)
        maxVal = Infinity;

    // Limit the string length to force convergence
    if (strVal !== undefined && strVal.length > TypeDesc.MAX_STR_LEN)
        strVal = undefined;

    // By default, the map set is empty
    if (mapSet === undefined)
        mapSet = [];

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
    String constant value. Undefined if unknown.
    */
    this.strVal = strVal;

    /**
    Set of possible maps, for object types
    */
    this.mapSet = mapSet;
}

/**
Generate a type descriptor for a constant value
*/
TypeDesc.constant = function (value)
{
    if (value instanceof IRConst)
        value = value.value;

    if (value === undefined)
    {
        return new TypeDesc(TypeFlags.UNDEF);
    }

    else if (value === null)
    {
        return new TypeDesc(TypeFlags.NULL);
    }

    else if (value === true)
    {
        return new TypeDesc(TypeFlags.TRUE);
    }

    else if (value === false)
    {
        return new TypeDesc(TypeFlags.FALSE);
    }

    else if (isInt(value) === true)
    {
        return new TypeDesc(TypeFlags.INT, value, value);
    }

    else if (typeof value === 'number')
    {
        return new TypeDesc(TypeFlags.FLOAT, value, value);
    }

    else if (typeof value === 'string')
    {
        return new TypeDesc(TypeFlags.STRING, undefined, undefined, value);
    }

    // By default, return the unknown type
    return TypeDesc.any;
}

/**
Produce a string representation of a type descriptor
*/
TypeDesc.prototype.toString = function (longForm)
{
    if (this.flags === TypeFlags.NOINF)
        return "noinf";

    if (this.flags === TypeFlags.ANY)
        return "any";

    var str = "";

    // Add the flags
    for (flagName in TypeFlags)
    {
        var flagVal = TypeFlags[flagName];

        if (flagVal === TypeFlags.ANY)
            continue;

        if (this.flags & flagVal)
        {
            if (str != "")
                str += ",";

            str += flagName.toLowerCase();
        }
    }

    // If range information is present
    if (this.minVal !== -Infinity || this.maxVal !== Infinity)
    {
        if (this.minVal === this.maxVal)
            str += " " + this.minVal;
        else if (this.minVal !== -Infinity && this.maxVal !== Infinity)
            str += " [" + this.minVal + ", " + this.maxVal + "]";
        else if (this.minVal === -Infinity)
            str += " ]-inf, " + this.maxVal + "]";
        else
            str += " [" + this.minVal + ", +inf[";
    }

    // If a string constant is defined
    if (this.strVal !== undefined)
    {
        str += ' "' + this.strVal + '"';
    }

    // If possible object types are defined
    if (this.mapSet.length !== 0)
    {
        str += " {";

        // Print the map types
        for (var i = 0; i < this.mapSet.length; ++i)
        {
            if (i > 0)
                str += " ";

            str += this.mapSet[i].toString(longForm);
        }

        str += "}";
    }

    return str;
}

/**
Type descriptor union (OR) function.
*/
TypeDesc.prototype.union = function (that)
{
    // If the other object is the uninferred type
    if (that.flags === TypeFlags.NOINF)
    {
        return this;
    }

    // If this object is the uninferred type
    else if (this.flags === TypeFlags.NOINF)
    {
        return that;
    }

    // If both type descriptors are the same, return this one
    else if (this === that)
    {
        return this;
    }

    // If both objects have meaningful type values
    else
    {
        var flags = this.flags | that.flags;

        // Merge the min range value
        var minVal;
        if (this.minVal === that.minVal)
        {
            minVal = this.minVal;
        }
        else if (this.minVal === -Infinity || that.minVal === -Infinity)
        {
            minVal = -Infinity;   
        }
        else if (this.minVal < 0 || that.minVal < 0)
        {
            var minMin = Math.abs(Math.min(this.minVal, that.minVal));
            
            if (isPowerOf2(minMin) === true)
                minVal = -minMin;
            else
                minVal = -nextPowerOf2(minMin);
        }
        else
        {
            minVal = Math.min(this.minVal, that.minVal);
            minVal = lowestBit(minVal);
        }
        assert (
            minVal <= this.minVal && minVal <= that.minVal,
            'invalid min value'
        );

        // Merge the max range value
        var maxVal;
        if (this.maxVal === that.maxVal)
        {
            maxVal = this.maxVal;
        }
        else if (this.maxVal === Infinity || that.maxVal === Infinity)
        {
            maxVal = Infinity;   
        }
        else if (this.maxVal > 0 || that.maxVal > 0)
        {
            var maxMax = Math.max(this.maxVal, that.maxVal);
            
            if (isPowerOf2(maxMax) === true)
                maxVal = maxMax;
            else
                maxVal = nextPowerOf2(maxMax);
        }
        else
        {
            maxVal = Math.max(this.maxVal, that.maxVal);
            maxVal = -lowestBit(Math.abs(maxVal));
        }
        assert (
            maxVal >= this.maxVal && maxVal >= that.maxVal,
            'invalid max value'
        );

        var strVal =
            (this.strVal === that.strVal)?
            this.strVal:undefined;

        // Copy the first map set
        var mapSet = this.mapSet.slice(0);

        // For each map in the second set
        for (var i = 0; i < that.mapSet.length; ++i)
        {
            var m2 = that.mapSet[i];

            var found = false;

            // For each map in the first set
            for (var j = 0; j < mapSet.length; ++j)
            {
                var m1 = mapSet[j];

                // If both classes have the same descriptor
                if (m1.classDesc === m2.classDesc)
                {
                    // Compute the map intersection
                    mapSet[j] = m1.intersect(m2);

                    // Break out of the inner loop
                    found = true;
                    break;
                }
            }

            // No matching class was found, add it to the set
            if (found === false)
                mapSet.push(m2);
        }

        // Create and return a new type descriptor and return it
        return new TypeDesc(
            flags,
            minVal,
            maxVal,
            strVal,
            mapSet
        );
    }
}

/**
Restrict a type descriptor based on possible type flags
*/
TypeDesc.prototype.restrict = function (flags)
{
    var flags = this.flags & flags;

    // If the flags are unchanged, return this descriptor
    if (flags === this.flags)
        return this;

    // Test whether the new type can be a number, string, function or object
    var canBeNum  = (flags & (TypeFlags.INT | TypeFlags.FLOAT)) !== 0;
    var canBeStr  = (flags & TypeFlags.STRING) !== 0;
    var canBeFunc = (flags & TypeFlags.FUNCTION) !== 0;
    var canBeObj  = (flags & (TypeFlags.OBJECT | TypeFlags.ARRAY)) !== 0;

    // If the new value can't be a number, remove the range info
    if (canBeNum === true)
    {
        var minVal = this.minVal;
        var maxVal = this.maxVal;
    }
    else
    {
        var minVal = -Infinity;
        var maxVal = Infinity;
    }

    var strVal = (canBeStr === true)? this.strVal:undefined;

    // If the type can be either a function or an object
    if (canBeFunc === true && canBeObj === true)
    {
        // Leave the map set unchanged
        var mapSet = this.mapSet.slice(0);
    }
    else
    {
        var mapSet = [];

        // For each item in the map set
        for (var i = 0; i < this.mapSet.length; ++i)
        {
            var map = this.mapSet[i];
            var classDesc = map.classDesc;

            var isFunc = (classDesc.origin instanceof IRFunction);

            if (isFunc == true && canBeFunc === false)
                continue;

            if (isFunc === false && canBeObj === false)
                continue;

            mapSet.push(map);
        }
    }

    // Create and return a new type descriptor and return it
    return new TypeDesc(
        flags,
        minVal,
        maxVal,
        strVal,
        mapSet
    );
}

/**
Type descriptor equality test
*/
TypeDesc.prototype.equal = function (that)
{
    // If the descriptors are the same, they are equal
    if (this === that)
        return true;

    if (this.flags !== that.flags)
        return false;

    if (this.minVal !== that.minVal ||
        this.maxVal !== that.maxVal)
        return false;

    if (this.strVal !== that.strVal)
        return false;

    if (arraySetEqual(this.mapSet, that.mapSet) === false)
        return false;

    // The type descriptors are equal
    return true;
}

/**
Try to evaluate this type descriptor as a string value
*/
TypeDesc.prototype.stringVal = function ()
{
    if (this.flags === TypeFlags.STRING)
        return String(this.strVal);

    if (this.flags === TypeFlags.INT && this.minVal === this.maxVal)
        return String(this.minVal);

    return undefined;
}

/**
Create an updated type descriptor to simulate a property set
*/
TypeDesc.prototype.putProp = function (propName, valType)
{
    assert (
        valType instanceof TypeDesc,
        'invalid type descriptor'
    );

    var mapSet = [];

    // Update the map descriptors
    for (var i = 0; i < this.mapSet.length; ++i)
        mapSet.push(this.mapSet[i].putProp(propName, valType));

    return new TypeDesc(
        this.flags,
        this.minVal,
        this.maxVal,
        this.strVal,
        mapSet
    );
}

/**
Minimum numerical range value inferred
*/
TypeDesc.MIN_NUM_RANGE = getIntMin(16);

/**
Maximum numerical range value inferred
*/
TypeDesc.MAX_NUM_RANGE = getIntMax(16);

/**
Max string length usable string values
*/
TypeDesc.MAX_STR_LEN = 256;

/**
Uninferred type descriptor
*/
TypeDesc.noinf = new TypeDesc(TypeFlags.NOINF);

/**
Unknown/any type descriptor
*/
TypeDesc.any = new TypeDesc(TypeFlags.ANY);

/**
Undefined type descriptor
*/
TypeDesc.undef = new TypeDesc(TypeFlags.UNDEF);

/**
Boolean type descriptor
*/
TypeDesc.bool = new TypeDesc(TypeFlags.TRUE | TypeFlags.FALSE);

/**
True type descriptor
*/
TypeDesc.true = new TypeDesc(TypeFlags.TRUE);

/**
False type descriptor
*/
TypeDesc.false = new TypeDesc(TypeFlags.FALSE);

/**
String type descriptor
*/
TypeDesc.string = new TypeDesc(TypeFlags.STRING);

/**
@class Object property map descriptor
*/
function MapDesc(classDesc)
{
    assert (
        classDesc instanceof ClassDesc,
        'invalid class descriptor'
    );

    /**
    Class of the object
    */
    this.classDesc = classDesc;

    /**
    Map of properties stored
    */
    this.propMap = {};

    /**
    Number of properties
    */
    this.numProps = 0;

    /**
    Transitions to other maps when adding properties
    */
    this.propTrans = {};

    // If there is already a map descriptor for this class, return it
    var cacheDesc = MapDesc.mapSet.get(this);
    if (cacheDesc !== HashMap.NOT_FOUND)
        return cacheDesc;

    // Cache this map descriptor
    MapDesc.mapSet.set(this, this);
}

/**
Hash function for maps
*/
MapDesc.mapHash = function (map)
{
    var hash = map.classDesc.classIdx;

    for (propName in map.propMap)
        hash += defHashFunc(propName);

    return hash;
}

/**
Equality function for maps
*/
MapDesc.mapEq = function (map1, map2)
{
    if (map1.classDesc !== map2.classDesc)
        return false;

    if (map1.numProps !== map2.numProps)
        return false;

    for (propName in map1.propMap)
        if (map2.propMap[propName] === undefined)
            return false;

    for (propName in map2.propMap)
        if (map1.propMap[propName] === undefined)
            return false;

    return true;
}

/**
Set of all existing maps
*/
MapDesc.mapSet = new HashMap(MapDesc.mapHash, MapDesc.mapEq);

/**
Produce a string representation of this map
*/
MapDesc.prototype.toString = function (longForm)
{
    var str = '';

    // Add the class name
    str += this.classDesc.getName();

    if (longForm === true)
    {
        // TODO!
    }
    else
    {
        // Add the number of guaranteed fields over the total number of fields
        if (this.classDesc.numProps !== 0)
            str += '(' + this.numProps + '/' + this.classDesc.numProps + ')';
    }

    return str;
}

/**
Simulate a property addition
*/
MapDesc.prototype.putProp = function (propName, valType)
{
    // Update the class descriptor
    this.classDesc.putProp(propName, valType);

    // If the property is already present, do nothing
    if (propName in this.propMap)
        return this;

    // If a property descriptor is cached for this addition, return it
    if (this.propTrans[propName] !== undefined)
        return this.propTrans[propName];

    // Create a new descriptor with the new property
    var desc = new MapDesc(this.classDesc);
    for (n in this.propMap)
        desc.propMap[n] = true;
    for (t in this.propTrans)
        desc.propTrans[t] = this.propTrans[t];
    desc.propMap[propName] = true;
    desc.numProps = this.numProps + 1;

    // If this descriptor already exists, use the existing one
    var cacheDesc = MapDesc.mapSet.get(desc);
    if (cacheDesc !== HashMap.NOT_FOUND)
        desc = cacheDesc;
    else
        MapDesc.mapSet.set(desc, desc);

    // Cache the property addition transition
    this.propTrans[propName] = desc;

    // Return the new descriptor
    return desc;
}

/**
Get the type of a property
*/
MapDesc.prototype.getPropType = function (propName)
{
    // Get the class-level type for the property
    var type = this.classDesc.getPropType(propName);

    // If this map does not have the type, union it with undefined
    if ((propName in this.propMap) === false)
        type = type.union(TypeDesc.undef);

    return type;
}

/**
Compute the intersection of two maps
*/
MapDesc.prototype.intersect = function (that)
{
    // If the two descriptors are equal, return this
    if (this === that)
        return this;

    // Create a new descriptor
    var desc = new MapDesc(this.classDesc);

    // Compute the property intersection
    for (n in this.propMap)
        if (n in that.propMap)
            desc.propMap[n] = true;

    // If this descriptor already exists, use the existing one
    var cacheDesc = MapDesc.mapSet.get(desc);
    if (cacheDesc !== HashMap.NOT_FOUND)
        desc = cacheDesc;
    else
        MapDesc.mapSet.set(desc, desc);

    // Return the new descriptor
    return desc;
}

/**
@class Object pseudo-class descriptor
*/
function ClassDesc(origin, protoType)
{
    assert (
        origin instanceof IRFunction ||
        origin instanceof IRInstr ||
        origin === 'global',
        'invalid class origin'
    );

    // If there is already a class descriptor for this origin, return it
    var cacheDesc = ClassDesc.classMap.get(origin);
    if (cacheDesc !== HashMap.NOT_FOUND)
        return cacheDesc;

    /**
    Unique class identifier
    */
    this.classIdx = ClassDesc.nextClassIdx++;

    /**
    Class origin. May be an IR value or 'global' for the global object.
    */
    this.origin = origin;

    /**
    Prototype type descriptor
    */
    this.protoType = protoType;

    /**
    Field descriptors, the order of field addition is not represented
    */
    this.propTypes = {};

    /**
    Number of properties
    */
    this.numProps = 0;

    /**
    Array field type descriptor
    */
    this.arrayType = new TypeDesc();

    // Cache the new class descriptor
    ClassDesc.classMap.set(origin, this);
}

/**
Map of origin values to class descriptors
*/
ClassDesc.classMap = new HashMap();

/**
Next class idx to assign
*/
ClassDesc.nextClassIdx = 0;

/**
Get the name of this class descriptor
*/
ClassDesc.prototype.getName = function ()
{
    if (this.origin === 'global')
        return this.origin;
    else
        return 'c' + this.classIdx;
}

/**
Produce a string representation of this class descriptor
*/
ClassDesc.prototype.toString = function ()
{
    var str = "class " + this.getName();

    if (this.origin instanceof IRFunction)
        str += ' (function "' + this.origin.funcName + '")';

    str += " {\n";

    // Output the field names and types
    for (propName in this.propTypes)
        str += '  "' + propName + '" : ' + this.propTypes[propName] + '\n';

    str += "}";

    return str;
}

/**
Update the class according to a property set
*/
ClassDesc.prototype.putProp = function (propName, valType)
{
    assert (
        valType instanceof TypeDesc,
        'invalid type descriptor'
    );

    var curType = this.propTypes[propName];

    // If this is a new property
    if (curType === undefined)
    {
        // Increment the number of properties
        ++this.numProps;

        // Use the incoming type directly
        var newType = valType;
    }
    else
    {
        // Compute the updated type
        var newType = curType.union(valType);
    }

    // Update the property type
    this.propTypes[propName] = newType;

    //
    // TODO: if change, notify blocks touching this class
    //
}

/**
Update the array type descriptor by unioning it with another type
*/
ClassDesc.prototype.arrayUnion = function (type)
{
    this.arrayType.union(type);
}

/**
Delete a field from the class
*/
ClassDesc.prototype.delProp = function (propName)
{
    assert (
        this.propTypes[propName] !== undefined,
        'field not found: "' + propName + '"'
    );

    // Set the undefined flag on the field type (the field may be undefined)
    this.propTypes[propName].union(TypeDesc.undef);
}

/**
Get the type descriptor for a given field
*/
ClassDesc.prototype.getPropType = function (propName)
{
    if ((propName in this.propTypes) === false)
        return TypeDesc.undef;

    return this.propTypes[propName];
}

