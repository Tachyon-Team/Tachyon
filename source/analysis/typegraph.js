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
@class Represents a variable in the type graph
*/
function TGVariable(name, parent)
{
    this.name = name;

    this.parent = parent;
}

/**
Produce a string representation of this variable
*/
TGVariable.prototype.toString = function ()
{
    return this.name;
}

/**
@class Represents an object property in the type graph
@extends TGVariable
*/
function TGProperty(name, parent)
{
    this.name = name;

    this.parent = parent;
}
TGProperty.prototype = new TGVariable();

/**
@class Object value in a type graph.
*/
function TGObject(origin, flags, numClosVars)
{
    assert (
        flags === TypeFlags.OBJECT  ||
        flags === TypeFlags.ARRAY   ||
        flags === TypeFlags.FUNCTION,
        'invalid type flags for object'
    );

    /**
    Origin (creation) site of the object.
    */
    this.origin = origin;

    // TODO: creation context

    // If the object is already in the map, return it
    var obj = TGObject.objMap.get(this);
    if (obj !== HashMap.NOT_FOUND)
        return obj;

    /**
    Type flags for this object
    */
    this.flags = flags;

    /**
    Prototype of this object
    */
    this.proto = new TGVariable('proto', this);

    /**
    Map of property names to corresponding variable nodes
    */
    this.props = {};

    /**
    Property node for all indexed/array properties
    */
    this.idxProp = new TGVariable('idx', this);

    /**
    Closure variables, for function objects
    */
    this.closVars = new Array(numClosVars);
    for (var i = 0; i < numClosVars; ++i)
        this.closVars[i] = new TGVariable('clos_' + i, this);

    // Add the object to the map
    TGObject.objMap.set(this, this);
}

/**
Map of existing objects
*/
TGObject.objMap = new HashMap(
    function hash(o)
    {
        return defHashFunc(o.origin);
    },
    function eq(o1, o2)
    {
        if (o1.origin !== o2.origin)
            return false;

        return true;
    }
);

/**
Get a printable name of an object
*/
TGObject.prototype.getName = function ()
{
    if (typeof this.origin === 'string')
        return this.origin;
    else if (this.origin instanceof IRFunction)
        return 'func:"' + this.origin.funcName + '"';
    else
        return this.origin.getValName();
}

/**
Produce a string representation of this object
*/
TGObject.prototype.toString = function ()
{
    return '<' + this.getName() + '>';
}

/**
Get the value node for a given property
*/
TGObject.prototype.getPropNode = function (name)
{
    // If the property doesn't exist, create it
    if (Object.prototype.hasOwnProperty.call(this.props, name) === false)
        this.props[name] = new TGProperty(name, this);

    return this.props[name];
}

/**
Test if this object is a singleton instance
*/
TGObject.prototype.isSingleton = function ()
{
    return (typeof this.origin === 'string');
}

/**
@class Represents a closure cell in the type graph
*/
function TGClosCell(origin)
{
    /**
    Origin (creation) site of the cell.
    */
    this.origin = origin;

    // If the object is already in the map, return it
    var obj = TGObject.objMap.get(this);
    if (obj !== HashMap.NOT_FOUND)
        return obj;

    /**
    Closure cell value variable
    */
    this.value = new TGVariable('cell_val', this);

    // Add the cell to the map
    TGClosCell.cellMap.set(this, this);
}

/**
Map of closure cells
*/
TGClosCell.cellMap = new HashMap(
    function hash(o)
    {
        return defHashFunc(o.origin);
    },
    function eq(o1, o2)
    {
        if (o1.origin !== o2.origin)
            return false;

        return true;
    }
);

/**
Produce a string representation of this object
*/
TGClosCell.prototype.toString = function ()
{
    return '<' + this.origin.getValName() + '>';
}

/**
@namespace Type descriptor flags namespace
*/
TypeFlags = {};

// Possible type descriptor flags
TypeFlags.UNDEF    = 1 << 0;    // May be undefined
TypeFlags.MISSING  = 1 << 1;    // Missing property
TypeFlags.NULL     = 1 << 2;    // May be null
TypeFlags.TRUE     = 1 << 3;    // May be true
TypeFlags.FALSE    = 1 << 4;    // May be false
TypeFlags.FLOAT    = 1 << 5;    // May be floating-point
TypeFlags.INT      = 1 << 6;    // May be integer
TypeFlags.STRING   = 1 << 7;    // May be string
TypeFlags.OBJECT   = 1 << 8;    // May be string
TypeFlags.ARRAY    = 1 << 9;    // May be string
TypeFlags.FUNCTION = 1 << 10;   // May be string
TypeFlags.CELL     = 1 << 11;   // May be closure cell

// Extended object (object or array or function)
TypeFlags.OBJEXT =
    TypeFlags.OBJECT    |
    TypeFlags.ARRAY     |
    TypeFlags.FUNCTION;

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
    TypeFlags.FUNCTION |
    TypeFlags.CELL;

// Empty/uninferred type flag (before analysis)
TypeFlags.EMPTY = 0;

/**
@class Set of value nodes representing possible variable types
*/
function TypeSet(
    flags,
    rangeMin,
    rangeMax,
    strVal,
    objSet
)
{
    // Empty type descriptors have the uninferred type
    if (flags === undefined)
        flags = TypeFlags.EMPTY;

    // By default, the numerical ranges are unbounded.
    // Otherwise, restrict them to a fixed integer range
    if (rangeMin === undefined)
        rangeMin = -Infinity;
    else if (rangeMin < TypeSet.MIN_NUM_RANGE)
        rangeMin = -Infinity;
    if (rangeMax === undefined)
        rangeMax = Infinity;
    else if (rangeMax > TypeSet.MAX_NUM_RANGE)
        rangeMax = Infinity;

    // Limit the string length to force convergence
    if (strVal !== undefined && strVal.length > TypeSet.MAX_STR_LEN)
        strVal = undefined;

    assert (
        objSet === undefined ||
        objSet instanceof HashSet
    );

    /**
    Type flags
    */
    this.flags = flags;

    /**
    Numerical range minimum
    */
    this.rangeMin = rangeMin;

    /**
    Numerical range maximum
    */
    this.rangeMax = rangeMax;

    /**
    String value
    */
    this.strVal = strVal;

    /**
    Object set
    */
    this.objSet = objSet;
}

/**
Generate a type set for a constant value
*/
TypeSet.constant = function (value)
{
    if (value instanceof IRConst)
        value = value.value;

    if (value === undefined)
    {
        return new TypeSet(TypeFlags.UNDEF);
    }

    else if (value === null)
    {
        return new TypeSet(TypeFlags.NULL);
    }

    else if (value === true)
    {
        return new TypeSet(TypeFlags.TRUE);
    }

    else if (value === false)
    {
        return new TypeSet(TypeFlags.FALSE);
    }

    else if (isInt(value) === true)
    {
        return new TypeSet(TypeFlags.INT, value, value);
    }

    else if (typeof value === 'number')
    {
        return new TypeSet(TypeFlags.FLOAT, value, value);
    }

    else if (typeof value === 'string')
    {
        return new TypeSet(TypeFlags.STRING, undefined, undefined, value);
    }

    // By default, return the unknown type
    return TypeSet.any;
}

/**
Produce a string representation of this type set
*/
TypeSet.prototype.toString = function ()
{
    if (this.flags === TypeFlags.EMPTY)
        return '{}';

    if (this.flags === TypeFlags.ANY)
        return '{any}';

    var str = '{';

    function addType(tstr)
    {
        if (str !== '{')
            str += ','

        str += tstr;
    }

    if (this.flags & TypeFlags.UNDEF)
        addType('undef');
    if (this.flags & TypeFlags.MISSING)
        addType('missing');
    if (this.flags & TypeFlags.NULL)
        addType('null');
    if (this.flags & TypeFlags.TRUE)
        addType('true');
    if (this.flags & TypeFlags.FALSE)
        addType('false');
    if (this.flags & TypeFlags.INT)
        addType('int');
    if (this.flags & TypeFlags.FLOAT)
        addType('float');
    if (this.flags & TypeFlags.STRING)
        addType('string');
    if (this.flags & TypeFlags.OBJECT)
        addType('object');
    if (this.flags & TypeFlags.ARRAY)
        addType('array');
    if (this.flags & TypeFlags.FUNCTION)
        addType('function');
    if (this.flags & TypeFlags.CELL)
        addType('cell');

    // If a range is defined
    if (this.rangeMin !== -Infinity || this.rangeMax !== Infinity)
    {
        var numStr = '';

        if (this.rangeMin === this.rangeMax)
        {
            numStr += this.rangeMin;
        }
        else
        {
            if (this.rangeMin === -Infinity)
                numStr += ']-inf,';
            else
                numStr += '[' + this.rangeMin + ',';

            if (this.rangeMax === Infinity)
                numStr += "+inf[";
            else
                numStr += this.rangeMax + "]";
        }

        addType(numStr);
    }

    // If a string constant is defined
    if (this.strVal !== undefined)
    {
        addType('"' + this.strVal + '"');
    }

    // If the object set is not empty
    if (this.objSet !== undefined && this.objSet.length !== 0)
    {
        // For each possible object
        for (var itr = this.objSet.getItr(); itr.valid(); itr.next())
        {
            var obj = itr.get();
            addType(obj.toString());
        }
    }

    str += "}";

    return str;
}

/**
Type set equality test
*/
TypeSet.prototype.equal = function (that)
{
    if (this === that)
        return true;

    if (this.flags !== that.flags)
        return false;

    if (this.rangeMin !== that.rangeMin ||
        this.rangeMax !== that.rangeMax)
        return false;

    if (this.strVal !== that.strVal)
        return false;

    if (this.objSet === undefined && that.objSet !== undefined ||
        that.objSet === undefined && this.objSet !== undefined ||        
        this.objSet !== undefined && this.objSet.equal(that.objSet) === false)
        return false;

    return true;
}

/**
Type set union function
*/
TypeSet.prototype.union = function (that)
{
    // If the other object is the uninferred type
    if (that.flags === TypeFlags.EMPTY)
    {
        return this;
    }

    // If this object is the uninferred type
    else if (this.flags === TypeFlags.EMPTY)
    {
        return that;
    }

    // If both type sets are the any set
    else if (this.flags === TypeFlags.ANY || that.flags === TypeFlags.ANY)
    {
        return TypeSet.any;
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

        var thisNum = (this.flags & (TypeFlags.INT | TypeFlags.FLOAT)) !== 0;
        var thatNum = (that.flags & (TypeFlags.INT | TypeFlags.FLOAT)) !== 0;

        var rangeMin;
        var rangeMax;

        if (thisNum === true && thatNum === false)
        {
            rangeMin = this.rangeMin;
            rangeMax = this.rangeMax
        }
        else if (thisNum === false && thatNum === true)
        {
            rangeMin = that.rangeMin;
            rangeMax = that.rangeMax;
        }
        else
        {
            if (this.rangeMin === that.rangeMin)
            {
                rangeMin = this.rangeMin;
            }
            else if (this.rangeMin === -Infinity || that.rangeMin === -Infinity)
            {
                rangeMin = -Infinity;   
            }
            else if (this.rangeMin < 0 || that.rangeMin < 0)
            {
                var minMin = Math.abs(Math.min(this.rangeMin, that.rangeMin));
                
                if (isPowerOf2(minMin) === true)
                    rangeMin = -minMin;
                else
                    rangeMin = -nextPowerOf2(minMin);
            }
            else
            {
                rangeMin = Math.min(this.rangeMin, that.rangeMin);
                rangeMin = lowestBit(rangeMin);
            }

            if (this.rangeMax === that.rangeMax)
            {
                rangeMax = this.rangeMax;
            }
            else if (this.rangeMax === Infinity || that.rangeMax === Infinity)
            {
                rangeMax = Infinity;   
            }
            else if (this.rangeMax > 0 || that.rangeMax > 0)
            {
                var maxMax = Math.max(this.rangeMax, that.rangeMax);
                
                if (isPowerOf2(maxMax) === true)
                    rangeMax = maxMax;
                else
                    rangeMax = nextPowerOf2(maxMax);
            }
            else
            {
                rangeMax = Math.max(this.rangeMax, that.rangeMax);
                rangeMax = -lowestBit(Math.abs(rangeMax));
            }

            assert (
                rangeMin <= this.rangeMin && rangeMin <= that.rangeMin,
                'invalid min value'
            );

            assert (
                rangeMax >= this.rangeMax && rangeMax >= that.rangeMax,
                'invalid max value'
            );
        }

        // Merge the string values
        var strVal;
        if ((this.flags & TypeFlags.STRING) && (that.flags & TypeFlags.STRING))
            strVal = (this.strVal === that.strVal)? this.strVal:undefined;
        if (this.flags & TypeFlags.STRING)
            strVal = this.strVal;
        else
            strVal = that.strVal;

        // Compute the union of the object sets
        var objSet;
        if (this.objSet === undefined && that.objSet === undefined)
            objSet = undefined;
        else if (this.objSet === undefined)
            objSet = that.objSet.copy();
        else if (that.objSet === undefined)
            objSet = this.objSet.copy();
        else
            objSet = this.objSet.copy().union(that.objSet);

        // Create and return a new type descriptor and return it
        return new TypeSet(
            flags,
            rangeMin,
            rangeMax,
            strVal,
            objSet
        );
    }
}

/**
Restrict a type set based on possible type flags
*/
TypeSet.prototype.restrict = function (flags)
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
        var rangeMin = this.rangeMin;
        var rangeMax = this.rangeMax;
    }
    else
    {
        var rangeMin = -Infinity;
        var rangeMax = Infinity;
    }

    var strVal = (canBeStr === true)? this.strVal:undefined;

    // If the type can be either a function or an object
    if (canBeFunc === true && canBeObj === true)
    {
        // Leave the object set unchanged
        var objSet = this.objSet? this.objSet.copy():undefined;
    }
    else
    {
        var objSet = new HashSet();

        // For each item in the object set
        for (var itr = this.getObjItr(); itr.valid(); itr.next())
        {
            var obj = itr.get()

            // If the flags don't match, skip this object
            if ((obj.flags & flags) === 0)
                continue;

            objSet.add(obj);
        }
    }

    // Create and return a new type set and return it
    return new TypeSet(
        flags,
        rangeMin,
        rangeMax,
        strVal,
        objSet
    );
}

/**
Get an iterator to the object set
*/
TypeSet.prototype.getObjItr = function ()
{
    assert (
        this !== TypeSet.any,
        'cannot get object iterator of any set'
    );

    if (this.objSet === undefined)
    {
        return {
            valid: function () { return false; }
        };
    }

    return this.objSet.getItr();
}

/**
Minimum numerical range value inferred
*/
TypeSet.MIN_NUM_RANGE = getIntMin(16);

/**
Maximum numerical range value inferred
*/
TypeSet.MAX_NUM_RANGE = getIntMax(16);

/**
Max string length usable string values
*/
TypeSet.MAX_STR_LEN = 256;

/**
Empty/uninferred type set (bottom element)
*/
TypeSet.empty = new TypeSet(TypeFlags.EMPTY);

/**
Unknown/any type set (top element)
*/
TypeSet.any = new TypeSet(TypeFlags.ANY);

/**
Undefined type descriptor
*/
TypeSet.undef = new TypeSet(TypeFlags.UNDEF);

/**
Missing property type descriptor
*/
TypeSet.missing = new TypeSet(TypeFlags.MISSING);

/**
Null type descriptor
*/
TypeSet.null = new TypeSet(TypeFlags.NULL);

/**
True type descriptor
*/
TypeSet.true = new TypeSet(TypeFlags.TRUE);

/**
False type descriptor
*/
TypeSet.false = new TypeSet(TypeFlags.FALSE);

/**
Boolean type descriptor
*/
TypeSet.bool = new TypeSet(TypeFlags.TRUE | TypeFlags.FALSE);

/**
Integer type descriptor
*/
TypeSet.integer = new TypeSet(TypeFlags.INT);

/**
Positive integer type descriptor
*/
TypeSet.posInt = new TypeSet(TypeFlags.INT, 0);

/**
String type descriptor
*/
TypeSet.string = new TypeSet(TypeFlags.STRING);

/**
@class Type graph. Contains edges between nodes and values
*/
function TypeGraph()
{
    /**
    Map of variables nodes to type sets
    */
    this.varMap = new HashMap();
}

/**
Set the type set for a variable node
*/
TypeGraph.prototype.assignType = function (varNode, typeSet)
{
    assert (
        varNode instanceof TGVariable ||
        varNode instanceof IRInstr,
        'invalid var node: ' + varNode
    );

    assert (
        typeSet instanceof TypeSet,
        'invalid type set'
    );

    this.varMap.set(varNode, typeSet);
}

/**
Union the type set of a variable with another type set
*/
TypeGraph.prototype.unionType = function (varNode, typeSet)
{
    assert (
        varNode instanceof TGVariable ||
        varNode instanceof IRInstr,
        'invalid var node: ' + varNode
    );

    assert (
        typeSet instanceof TypeSet,
        'invalid type set: ' + typeSet
    );

    var curSet = this.getType(varNode);

    var unionSet = curSet.union(typeSet);

    assert (
        unionSet instanceof TypeSet,
        'invalid union set'
    );

    this.varMap.set(varNode, unionSet);
}

/**
Remove a variable node from the graph
*/
TypeGraph.prototype.remVar = function (varNode)
{
    assert (
        varNode instanceof TGVariable ||
        varNode instanceof IRInstr,
        'invalid var node: ' + varNode
    );

    this.varMap.rem(varNode);
}

/**
Copy a graph instance
*/
TypeGraph.prototype.copy = function ()
{
    var newGraph = new TypeGraph();

    for (var nodeItr = this.varMap.getItr(); nodeItr.valid(); nodeItr.next())
    {
        var pair = nodeItr.get();
        var node = pair.key;
        var typeSet = pair.value;

        newGraph.assignType(node, typeSet);
    } 

    return newGraph;
}

/**
Merge another graph into this one.
*/
TypeGraph.prototype.merge = function (that)
{
    var newGraph = this.copy();

    //print('graph merge');

    for (nodeItr = that.varMap.getItr(); nodeItr.valid(); nodeItr.next())
    {
        var edge = nodeItr.get();
        var node = edge.key;

        var thatSet = that.getType(node);

        newGraph.unionType(node, thatSet);
    }

    //print('graph merge done');

    return newGraph;
}

/**
Compare this graph for equality with another.
Equality means both graphs have the same edges.
*/
TypeGraph.prototype.equal = function (that)
{
    if (this.varMap.numItems !== that.varMap.numItems)
        return false;

    for (nodeItr = that.varMap.getItr(); nodeItr.valid(); nodeItr.next())
    {
        var edge = nodeItr.get();
        var node = edge.key;

        var thisSet = this.getType(node);

        var thatSet = that.getType(node);

        if (thisSet.equal(thatSet) === false)
            return false;
    }

    return true;
}

/**
Create a new object in the type graph
*/
TypeGraph.prototype.newObject = function (origin, protoSet, flags, numClosVars)
{
    // By default, the prototype is null
    if (protoSet === undefined)
        protoSet = TypeSet.null;

    // By default, this is a regular object
    if (flags === undefined)
        flags = TypeFlags.OBJECT;

    // By default, no closure variables
    if (numClosVars === undefined)
        numClosVars = 0;

    assert (
        protoSet.flags !== TypeFlags.EMPTY,
        'invalid proto set flags'
    )

    var obj = new TGObject(origin, flags, numClosVars);

    this.assignType(obj.proto, protoSet);

    var objSet = new HashSet();
    objSet.add(obj);

    return new TypeSet(
        flags, 
        undefined, 
        undefined, 
        undefined, 
        objSet,
        numClosVars
    );
}

/**
Get the type set for an IR value or variable node
*/
TypeGraph.prototype.getType = function (value)
{
    // If this is a variable node or IR instruction
    if (value instanceof TGVariable ||
        value instanceof IRInstr)
    {
        var typeSet = this.varMap.get(value);

        // If a type set was found
        if (typeSet !== HashMap.NOT_FOUND)
        {
            return typeSet;
        }
        else
        {
            // If this is a property node and there is no type set,
            // the type of the property is the special missing type
            if (value instanceof TGProperty)
            {
                //print('MISSING TYPE FOR PROP: ' + value);
                return TypeSet.missing;
            }

            // Return the empty type set
            return TypeSet.empty;
        }
    }

    // If this is a constant
    else
    {
        assert (
            value instanceof IRConst,
            'invalid value'
        );

        // Create a type set for the constant
        return TypeSet.constant(value);
    }
}

