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
TypeDesc.flags = {};

// Possible type descriptor flags
TypeDesc.flags.UNDEF    = 1 << 0; // May be undefined
TypeDesc.flags.NULL     = 1 << 1; // May be null
TypeDesc.flags.TRUE     = 1 << 2; // May be true
TypeDesc.flags.FALSE    = 1 << 3; // May be false
TypeDesc.flags.FLOAT    = 1 << 4; // May be floating-point
TypeDesc.flags.INT      = 1 << 5; // May be integer
TypeDesc.flags.STRING   = 1 << 6; // May be string
TypeDesc.flags.OBJECT   = 1 << 7; // May be string
TypeDesc.flags.ARRAY    = 1 << 8; // May be string
TypeDesc.flags.FUNCTION = 1 << 9; // May be string

// Unknown/any type flag
TypeDesc.flags.ANY =
    TypeDesc.flags.UNDEF    |
    TypeDesc.flags.NULL     |
    TypeDesc.flags.TRUE     |
    TypeDesc.flags.FALSE    |
    TypeDesc.flags.INT      |
    TypeDesc.flags.FLOAT    |
    TypeDesc.flags.STRING   |
    TypeDesc.flags.OBJECT   |
    TypeDesc.flags.ARRAY    |
    TypeDesc.flags.FUNCTION;

// Uninferred type flag (before analysis)
TypeDesc.flags.NOINF = 0;

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
        flags = TypeDesc.flags.NOINF;

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
    // TODO: handle IRFunction constants

    if (value instanceof IRConst)
        value = value.value;

    if (value === undefined)
    {
        return new TypeDesc(TypeDesc.flags.UNDEF);
    }

    else if (value === null)
    {
        return new TypeDesc(TypeDesc.flags.NULL);
    }

    else if (value === true)
    {
        return new TypeDesc(TypeDesc.flags.TRUE);
    }

    else if (value === false)
    {
        return new TypeDesc(TypeDesc.flags.FALSE);
    }

    else if (isInt(value) === true)
    {
        return new TypeDesc(TypeDesc.flags.INT, value, value);
    }

    else if (typeof value === 'number')
    {
        return new TypeDesc(TypeDesc.flags.FLOAT, value, value);
    }

    else if (typeof value === 'string')
    {
        return new TypeDesc(TypeDesc.flags.STRING, undefined, undefined, value);
    }

    // By default, return the unknown type
    return TypeDesc.any;
}

/**
Produce a string representation of a type descriptor
*/
TypeDesc.prototype.toString = function (longForm)
{
    if (this.flags === TypeDesc.flags.NOINF)
        return "noinf";

    if (this.flags === TypeDesc.flags.ANY)
        return "any";

    var str = "";

    // Add the flags
    for (flagName in TypeDesc.flags)
    {
        var flagVal = TypeDesc.flags[flagName];

        if (flagVal === TypeDesc.flags.ANY)
            continue;

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
    if (that.flags === TypeDesc.flags.NOINF)
    {
        // This type remains unchanged
    }

    // If this object is the uninferred type
    else if (this.flags === TypeDesc.flags.NOINF)
    {
        return that;
    }

    // If both objects have meaningful type values
    else
    {
        var flags = this.flags | that.flags;

        var minVal =
            (this.minVal !== undefined && that.minVal !== undefined)?
            Math.min(this.minVal, that.minVal):undefined;

        var maxVal =
            (this.maxVal !== undefined && that.maxVal !== undefined)?
            Math.max(this.maxVal, that.maxVal):undefined;

        var strVal =
            (this.strVal === that.strVal)?
            this.strVal:undefined;

        var mapSet = arraySetUnion(this.mapSet, that.mapSet);

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
    if (this.flags === TypeDesc.flags.STRING)
        return String(this.strVal);

    if (this.flags === TypeDesc.flags.INT && this.minVal === this.maxVal)
        return String(this.minVal);

    return undefined;
}

/**
Create an updated type descriptor to simulate a property set
*/
TypeDesc.prototype.putProp = function (propName, valType)
{
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
Uninferred type descriptor
*/
TypeDesc.noinf = new TypeDesc(TypeDesc.flags.NOINF);

/**
Unknown/any type descriptor
*/
TypeDesc.any = new TypeDesc(TypeDesc.flags.ANY);

/**
Boolean type descriptor
*/
TypeDesc.bool = new TypeDesc(TypeDesc.flags.TRUE | TypeDesc.flags.FALSE);

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
    Set of property names stored
    */
    this.propNames = {};

    /**
    Transitions to other maps when adding properties
    */
    this.propTrans = {};
}

/**
Hash function for maps
*/
MapDesc.mapHash = function (map)
{
    var hash = map.classDesc.classIdx;

    for (propName in map.propNames)
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

    for (propName in map1.propNames)
        if (map2.propNames[propName] === undefined)
            return false;

    for (propName in map2.propNames)
        if (map1.propNames[propName] === undefined)
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
    if (longForm === true)
    {
        // TODO!
    }
    else
    {
        if (this.classDesc.globalClass === true)
            return 'global';
        else
            return String(this.classDesc.classIdx);
    }
}

/**
Simulate a property addition
*/
MapDesc.prototype.putProp = function (propName, valType)
{
    // TODO
    // TODO: update the class descriptor
    //this.classDesc.putProp(propName, valType);

    // If the property is already present, do nothing
    if (propName in this.propNames)
        return this;

    // If a property descriptor is cached for this addition, return it
    if (this.propTrans[propName] !== undefined)
        return this.propTrans[propName];

    // Create a new descriptor with the new property
    var desc = new MapDesc(this.classDesc);
    for (n in this.propNames)
        desc.propNames[n] = true;
    for (t in this.propTrans)
        desc.propTrans[t] = this.propTrans[t];
    desc.propNames[propName] = true;

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
@class Object pseudo-class descriptor
*/
function ClassDesc()
{
    /**
    Unique class identifier
    */
    this.classIdx = ClassDesc.nextClassIdx++;


    // TODO: class origin descriptor, source code location, IR instruction***?
    this.origin = undefined;


    // TODO: prototype type descriptor
    /**
    Prototype type descriptor
    */
    this.protoType = undefined;


    /**
    Field descriptors, the order of field addition is not represented
    */
    this.fieldTypes = {};

    /**
    Array field type descriptor
    */
    this.arrayType = new TypeDesc();

    /**
    Global object class flag
    */
    this.globalClass = false;
}

/**
Next class idx to assign
*/
ClassDesc.nextClassIdx = 0;

/**
Produce a string representation of this class descriptor
*/
ClassDesc.prototype.toString = function ()
{
    var str = "class " + this.classIdx + "{\n";

    // Output the field names and types
    for (fieldName in this.fieldTypes)
        str += '\t"' + fieldName + '" : ' + this.fieldTypes[fieldname] + '\n';

    str += "}";

    return str;
}

/**
Get the class descriptor resulting from a field addition
*/
ClassDesc.prototype.addField = function (fieldName)
{
    assert (
        this.fieldTypes[fieldName] === undefined,
        'field already present: "' + fieldName + '"'
    );


    //
    // TODO: rewrite this
    //






}

/**
Update the type descriptor for a field by unioning it with another type
*/
ClassDesc.prototype.fieldUnion = function (fieldName, type)
{
    assert (
        this.fieldTypes[fieldName] !== undefined,
        'field not found: "' + fieldName + '"'
    );

    // Perform the type union
    this.fieldTypes[fieldName].union(type);
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
ClassDesc.prototype.delField = function (fieldName)
{
    assert (
        this.fieldTypes[fieldName] !== undefined,
        'field not found: "' + fieldName + '"'
    );

    // Set the undefined flag on the field type (the field may be undefined)
    this.fieldTypes[fieldName].union(new TypeDesc.constant(undefined));
}

/**
Get the type descriptor for a given field
*/
ClassDesc.prototype.getFieldType = function (fieldName)
{
    assert (
        this.fieldTypes[fieldName] !== undefined,
        'field not found: "' + fieldName + '"'
    );

    return this.fieldTypes[fieldname];
}

