/**
@fileOverview

Graph implementation with corresponding algorithms.

@copyright
Copyright (c) 2010 Tachyon Javascript Engine, All Rights Reserved
*/

/** @namespace */
var graph = graph || {};

/** 
    Returns a graph object with an adjency list internal representation.

    Node objects must have a toString method returning a different value
    for each individual object.
*/
graph.adjencyList = function ()
{
    var that = Object.create(graph.adjencyList.prototype);

    /** Contains the internal nodes as integer values. */
    that.nodes = [];

    /** Contains the original objects used */
    that.objectLookup = [];
    /** Contains the associated node number for a given object */
    that.nodeLookup = {};
    return that;
};

/** @private */
graph.adjencyList.prototype.node = function (id)
{
    var that = Object.create(this.node.prototype);
    that.incoming = [];
    that.outgoing = [];
    that.id = id;
    return that;
};

/** @private */
graph.adjencyList.prototype.node.prototype.copy = function ()
{
    var that = Object.create(this);
    that.incoming = this.incoming.slice(0);
    that.outgoing = this.outgoing.slice(0);
    that.id = this.id;
    return that;
};

/** @private */
graph.adjencyList.prototype.node.prototype.toString = function ()
{
    return "node { incoming:" + this.incoming + " outgoing:" + this.outgoing +
           " id:" + this.id + "}";
};

/** 
    Add an edge to the graph.  If the edge refers to unknown nodes, 
    corresponding nodes will be added.

    Amortized O(1).
*/
graph.adjencyList.prototype.addEdge = function (from, to)
{
    var fromPos, toPos;

    this.addNode(from);
    this.addNode(to);
    
    fromPos = this.nodeLookup[from];
    toPos = this.nodeLookup[to];

    this.nodes[fromPos].outgoing.push(toPos); 
    this.nodes[toPos].incoming.push(fromPos); 
};

/** 
    Add a node to the graph. Does nothing if the node is already present.
*/
graph.adjencyList.prototype.addNode = function (node)
{
    
    var nodePos = this.nodeLookup[node];
    if (nodePos === undefined)
    {
        nodePos = this.nodes.length;
        this.nodeLookup[node] = nodePos;
        this.nodes.push(this.node(nodePos));
        this.objectLookup.push(node);
    }
};

/** @private returns the node numbers in a topological order */
graph.adjencyList.prototype.topologicalSort = function ()
{
    // Make a copy of all nodes
    var nodes = arrayMap(this.nodes, function (node) { return node.copy(); });
    var sorted = [];
    var tovisit = [];
    var nodeIt;
    var pred;
    var succ;

    function noIncomingEdge(node)
    {
        return node.incoming.length === 0;
    }
    function hasEdge(node)
    {
        return node.incoming.length !== 0 ||
               node.outgoing.length !== 0;
    }

    // Initialize tovisit with every node with no incoming edge
    for (nodeIt = arrayIterator(nodes, noIncomingEdge); 
         !nodeIt.end(); 
         nodeIt.next())
    {
        tovisit.push(nodeIt.get());
    }


    while (tovisit.length !== 0)
    {
        pred = tovisit.pop(); 
        sorted.push(pred.id);

        for(succ = nodes[pred.outgoing.pop()];
            succ !== undefined;
            succ = nodes[pred.outgoing.pop()])
        {
            arraySetRem(succ.incoming, pred.id);
            if (succ.incoming.length === 0)
            {
                tovisit.push(succ);
            }
        }
    }

    // Ensure the graph contains no cycle
    for (nodeIt = arrayIterator(nodes, hasEdge); 
         !nodeIt.end(); 
         nodeIt.next())
    {
        error("Graph contains a cycle");
    }
    

    return sorted;
};

/** 
    Returns an iterator to the nodes. Complexity depends on 
    the type of the order.

    "topologicalSort"
    O(|E| + |N|) where |E| is the number of edges 
    and |N| is the number of nodes.


*/
graph.adjencyList.prototype.getNodeIterator = function (type)
{
    if (type === undefined)
    {
        type = "topologicalSort";
    }

    assert(type === "topologicalSort", 
           "unsupported iterator type:'" + type + "'");
    var it = Object.create(this.getNodeIterator.prototype); 
    it.index = -1;
    it.nodes = this.topologicalSort();
    it.graph = this;
    it.next();
    return it;
};

/** Tells whether all items have been visited */
graph.adjencyList.prototype.getNodeIterator.prototype.end = function ()
{
    return this.index >= this.nodes.length;
};
/** Move iterator to the next item */
graph.adjencyList.prototype.getNodeIterator.prototype.next = function ()
{
    this.index++;
};
/** Return the current item */
graph.adjencyList.prototype.getNodeIterator.prototype.get = function ()
{
    return this.graph.objectLookup[this.nodes[this.index]]; 
};
