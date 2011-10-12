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
    classDesc
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
    Pseudo-class descriptor. Undefined if unknown.
    */
    this.classDesc = classDesc;
}

/**
Generate a type descriptor for a constant value
*/
TypeDesc.constant = function (value)
{
    if (value instanceof IRConst)
        value = IRConst.value;

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
        return TypeDesc.string;
    }

    // TODO: handle other types
    return TypeDesc.any;
}

/**
Produce a string representation of a type descriptor
*/
TypeDesc.prototype.toString = function ()
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
    if (this.classDesc !== undefined)
    {
        str += " class:" + this.classDesc.classIdx;
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
    // If the other object is the uninferred type
    if (that.flags === TypeDesc.flags.NOINF)
    {
        return this;
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

        var classDesc =
            (this.classDesc === that.classDesc)?
            this.classDesc:undefined;

        // Create and return a new type descriptor and return it
        return new TypeDesc(
            flags,
            minVal,
            maxVal,
            classDesc
        );
    }
}

// Any type descriptor
TypeDesc.any = new TypeDesc(TypeDesc.flags.ANY);

// Uninferred type descriptor
TypeDesc.noinf = new TypeDesc(TypeDesc.flags.NOINF);

// Generic undefined type descriptor
TypeDesc.undef = new TypeDesc(TypeDesc.flags.UNDEF);

// Generic number type descriptor
TypeDesc.number = new TypeDesc(TypeDesc.flags.INT | TypeDesc.flags.FLOAT);

// Generic string type descriptor
TypeDesc.string = new TypeDesc(TypeDesc.flags.STRING);

// Generic object type descriptor
TypeDesc.object = new TypeDesc(TypeDesc.flags.OBJECT);

/**
@class Object pseudo-class descriptor
*/
function ClassDesc()
{
    /**
    Unique class identifier
    */
    this.classIdx = ClassDesc.nextClassIdx++;

    /**
    Field descriptors, the order of field addition is not represented
    */
    this.fieldTypes = {};

    /**
    References to other classes produced by adding fields
    */
    this.trans = {};
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

    // If a transition already exists, return the corresponding class
    if (this.trans[fieldName] !== undefined)
        return this.trans[fieldName];

    // Create a descriptor for the new class
    var newClass = new ClassDesc();

    // Copy the existing field types
    for (field in this.fieldTypes)
        newClass.fieldTypes[field] = this.fieldTypes[field];

    // Add the new field
    this.fieldTypes[fieldName] = TypeDesc.Noinf;

    // Store the new class for future reference
    this.trans[fieldName] = newClass;

    // Return the new class
    return newClass;
}

/**
Update the type descriptor for a field by unioning it with another
*/
ClassDesc.prototype.fieldUnion = function (fieldName, type)
{
    assert (
        this.fieldTypes[fieldName] !== undefined,
        'field not found: "' + fieldName + '"'
    );

    // Perform the type union
    this.fieldTypes[fieldName] = this.fieldTypes[fieldName].union(type);
}

//
// TODO: field deletion handling
//

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

// Empty object class descriptor
ClassDesc.emptyClass = new ClassDesc();




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
- For now, could store this structure globally?

Can't actually know the global object map at all times. Fields may be deleted.
May want something more flexible than just a global object map. Special handling.

Otherwise, may want a map that can account for potentially deleted fields.
Set them to the any type? How do regular maps handle deletion****

If a field is deleted, and we know that for certain, we can set it to the
undefined value type descriptor.
*/



// TODO: type analysis function for CFG/function









