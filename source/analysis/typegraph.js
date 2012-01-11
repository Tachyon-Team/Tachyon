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
Any value, no type information available
*/
TGValue.anyVal = Object.create(new TGValue());

/**
String representation of the any value
*/
TGValue.anyVal.toString = function () { return 'any'; };

/**
Undefined constant value
*/
TGValue.undefVal = new TGConst(undefined);

/**
Null constant value
*/
TGValue.nullVal = new TGConst(null);

/**
True constant value
*/
TGValue.trueVal = new TGConst(true);

/**
False constant value
*/
TGValue.falseVal = new TGConst(false);

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
    var str = '';

    if (typeof this.origin === 'string')
        return this.origin;
    else
        this.origin.getValName();

    str += ' (' + this.serial + ')';

    return str;
}

/**
@class Type graph. Contains edges between nodes and values
*/
function TypeGraph()
{
    /**
    Map of variables to sets of outgoing edges to values
    */
    this.varMap = new HashMap();
}

/**
Add an edge to the graph
*/
TypeGraph.prototype.addEdge = function (varNode, valNode)
{
    assert (
        varNode instanceof TGVariable ||
        varNode instanceof IRInstr,
        'invalid var node'
    );

    assert (
        valNode instanceof TGValue,
        'invalid val node'
    );

    var edgeSet = this.varMap.get(varNode)

    if (edgeSet === HashMap.NOT_FOUND)
    {
        var edgeSet = new HashSet();
        this.varMap.set(varNode, edgeSet);
    }

    edgeSet.add(valNode);
}

/**
Copy a graph instance
*/
TypeGraph.prototype.copy = function ()
{
    var newGraph = new TypeGraph();

    for (var nodeItr = this.varMap.getItr(); nodeItr.valid(); nodeItr.next())
    {
        var edge = nodeItr.get();

        var node = edge.first;
        var edgeSet = edge.second;

        for (var edgeItr = edgeSet.getItr(); edgeItr.valid(); edgeItr.next())
            newGraph.addEdge(node, edgeItr.get());
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
            for (var edgeItr = edgeSet.getItr(); edgeItr.valid(); edgeItr.next())
                localSet.add(edeItr.get());
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

        if (localSet.length !== edgeSet.length)
            return false

        for (var edgeItr = edgeSet.getItr(); edgeItr.valid(); edgeItr.next())
            if (localSet.has(edgeItr.get()) === false)
                return false;
    }

    return true;
}

/**
Create a new object in the type graph
*/
TypeGraph.prototype.newObject = function (origin, protoVal)
{
    // By default, the prototype is null
    if (protoVal === undefined)
        protoVal = TGValue.nullVal;

    var obj = new TGObject(origin);

    this.addEdge(obj.proto, protoVal);

    return obj;
}

