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


/*
TODO: type graph design, start simple!

TODO: How do we express the any type? Special node?
- No need for it at this moment!

TODO: true, false nodes, boolean edges (true or false)

For each basic block, maintain an associated graph at the entry.
If a graph changes after merge, queue the block.

Issue: need graph comparison to do the merge and update if changed.
If we keep the nodes (variables, atomic types, object instances) common
among all graphs, then graphs are just hash maps of edges. Easy to compare.


Variable nodes for all SSA temps
Variable nodes for object fields

Singleton value nodes for constants

Singleton value nodes for objects
- Can have multiple instances for one creation site

What about objects?
- Can't be unique nodes because they can have different properties?
- Could have node for object with sub-nodes for the fields?
  - The object field is a variable***
  - Variables point to possible values***


Compression idea****
- Could reuse whole edge sets for variable nodes
- Copy outgoing edge sets on modification
- Store only changed edge sets?
- Could eventually have singleton edge sets

Compression observations:
1. Most nodes are variables. Most only need one edge set through their entire lifetime.
- Nodes that only have one possible edge set need only be encoded in one location, never copied?

*/





/**
@class Represents a variable or object property in type graph.
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
@class Base class for all type graph value nodes.
*/
function TGValue()
{
}

/**
@class Constant value in a type graph.
*/
function TGConst(value)
{
    if ((value instanceof IRConst) === false)
        value = IRConst.getConst(null);

    var node = TGConst.constMap.get(value);
    if (node !== HashMap.NOT_FOUND)
        return node;

    this.value = value;

    TGConst.constMap.set(value, this);
}
TGConst.prototype = new TGValue();

/**
Map of IR constants to type graph nodes.
*/
TGConst.constMap = new HashMap();

/**
Produce a string representation of this node
*/
TGConst.prototype.toString = function ()
{
    return this.value.toString();
}

/**
@class Object value in a type graph.
*/
function TGObject(origin)
{
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
    Prototype of this object
    */
    this.proto = new TGVariable('proto', this);

    /**
    Map of property names to corresponding variable nodes
    */
    this.props = {};

    // Add the object to the map
    TGObject.objMap.set(this, this);
}
TGObject.prototype = new TGValue();

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
Produce a string representation of this object
*/
TGObject.prototype.toString = function ()
{
    var str = '<';

    if (typeof this.origin === 'string')
        str += this.origin;
    else if (this.origin instanceof IRFunction)
        str += 'func:"' + this.origin.funcName + '"';
    else
        str += this.origin.getValName();

    str += '>';

    return str;
}

/**
Get the value node for a given property
*/
TGObject.prototype.getPropNode = function (name)
{
    // If the property doesn't exist, create it
    if (this.props[name] === undefined)
        this.props[name] = new TGVariable(name, this);

    return this.props[name];
}

/**
@class Set of value nodes representing possible variable types
*/
function TypeSet()
{
    // Run the hash set constructor
    HashSet.apply(this);

    // Add the arguments to the set
    for (var i = 0; i < arguments.length; ++i)
        this.add(arguments[i]);
}
TypeSet.prototype = new HashSet();

/**
Produce a string representation of this type set
*/
TypeSet.prototype.toString = function ()
{
    if (this === TypeSet.any)
        return '{any}';

    var str = '{';


    for (var itr = this.getItr(); itr.valid(); itr.next())
    {
        var val = itr.get();

        if (str !== '{')
            str += ',';

        str += val.toString();
    }

    str += '}';

    return str;
}

/**
Type set equality test
*/
TypeSet.prototype.equal = function (set)
{
    if (this === set)
        return true;

    if (this === TypeSet.anySet || set === TypeSet.anySet)
        return false;

    return HashSet.prototype.equal.call(this, set);
}

/**
Type set union function
*/
TypeSet.prototype.union = function (set)
{
    if (this === TypeSet.any || set === TypeSet.any)
        return TypeSet.any;

    return HashSet.prototype.union.call(this, set);
}

/**
Type set add element function
*/
TypeSet.prototype.add = function (val)
{
    if (this === TypeSet.any)
        return TypeSet.any;

    return HashSet.prototype.add.call(this, val);
}

/**
Type set has element function
*/
TypeSet.prototype.has = function (val)
{
    if (this === TypeSet.any)
        return true;

    return HashSet.prototype.has.call(this, val);
}

/**
Empty type set (bottom element)
*/
TypeSet.emptySet = new TypeSet();

/**
Any/full type set (top element)
*/
TypeSet.anySet = new TypeSet();

/**
Undefined constant value
*/
TypeSet.undefSet = new TypeSet(new TGConst(undefined));

/**
Null constant value
*/
TypeSet.nullSet = new TypeSet(new TGConst(null));

/**
True constant value
*/
TypeSet.trueSet = new TypeSet(new TGConst(true));

/**
False constant value
*/
TypeSet.falseSet = new TypeSet(new TGConst(false));

/**
Boolean set (true or false)
*/
TypeSet.boolSet = new TypeSet(new TGConst(true), new TGConst(false));

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
Set the possible types for a variable node
*/
TypeGraph.prototype.assignTypes = function (varNode, typeSet)
{
    assert (
        varNode instanceof TGVariable ||
        varNode instanceof IRInstr,
        'invalid var node'
    );

    if (typeSet instanceof TGValue)
        typeSet = new TypeSet(typeSet);

    assert (
        typeSet instanceof TypeSet,
        'invalid type set'
    );

    this.varMap.set(varNode, typeSet);
}

/**
Union the types of a variable with another type set
*/
TypeGraph.prototype.unionTypes = function (varNode, typeSet)
{
    assert (
        varNode instanceof TGVariable ||
        varNode instanceof IRInstr,
        'invalid var node'
    );

    if (typeSet instanceof TGValue)
        typeSet = new TypeSet(typeSet);

    assert (
        typeSet instanceof TypeSet,
        'invalid type set'
    );

    var edgeSet = this.varMap.get(varNode)

    if (edgeSet === HashMap.NOT_FOUND)
    {
        var edgeSet = new TypeSet();
        this.varMap.set(varNode, edgeSet);
    }

    edgeSet.union(typeSet);
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
        var node = pair.first;
        var edgeSet = pair.second;

        newGraph.unionTypes(edgeSet);
    } 

    return newGraph;
}

/**
Merge another graph into this one.
*/
TypeGraph.prototype.merge = function (other)
{
    for (nodeItr = other.varMap.getItr(); nodeItr.valid(); nodeItr.next())
    {
        var edge = nodeItr.get();
        var node = edge.first;
        var edgeSet = edge.second;

        var localSet = this.varMap.get(node);

        if (localSet === HashMap.NOT_FOUND)
        {
            localSet = edgeSet.copy();
            this.varMap.set(node, localSet);
        }
        else
        {
            localSet.union(edgeSet);
        }
    }
}

/**
Compare this graph for equality with another.
Equality means both graphs have the same edges.
*/
TypeGraph.prototype.equal = function (other)
{
    if (this.varMap.numItems !== other.varMap.numItems)
        return false;

    for (nodeItr = other.varMap.getItr(); nodeItr.valid(); nodeItr.next())
    {
        var edge = nodeItr.get();

        var node = edge.first;
        var edgeSet = edge.second;

        var localSet = this.varMap.get(node);

        if (localSet.equal(edgeSet) === false)
            return false;
    }

    return true;
}

/**
Create a new object in the type graph
*/
TypeGraph.prototype.newObject = function (origin, protoSet)
{
    // By default, the prototype is null
    if (protoSet === undefined)
        protoSet = TypeSet.undefSet;

    var obj = new TGObject(origin);

    this.unionTypes(obj.proto, protoSet);

    return obj;
}

/**
Get the set of value nodes for an IR value
*/
TypeGraph.prototype.getTypeSet = function (value)
{
    // If this is a variable node or IR instruction
    if (value instanceof TGVariable ||
        value instanceof IRInstr)
    {
        var typeSet = this.varMap.get(value);
        if (typeSet !== HashMap.NOT_FOUND)
            return typeSet;

        return TypeSet.emptySet;
    }

    // If this is a constant
    else
    {
        assert (
            value instanceof IRConst,
            'invalid value'
        );

        return new TypeSet(new TGConst(value));
    }
}

