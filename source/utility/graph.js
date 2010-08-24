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

    /** Contains the internal nodes. */
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
graph.adjencyList.prototype._topologicalSort = function ()
{
    // Make a copy of all nodes
    var sorted = [];
    var tovisit = [];
    var nodeIt;
    var pred;
    var succ;
    var nodes = this.nodes.map(function (node) 
                               {
                                    if (node !== undefined ) return node.copy();
                                    return node;
                               });

    function noIncomingEdge(node)
    {
        return node !== undefined && node.incoming.length === 0;
    }
    function hasEdge(node)
    {
        return node !== undefined && 
               (node.incoming.length !== 0 ||
               node.outgoing.length !== 0);
    }

    // Initialize tovisit with every node with no incoming edge
    for (nodeIt = new FilterIterator(new ArrayIterator(nodes), noIncomingEdge); 
         nodeIt.valid(); 
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
    for (nodeIt = new FilterIterator(new ArrayIterator(nodes), hasEdge); 
         nodeIt.valid(); 
         nodeIt.next())
    {
        error("Graph contains a cycle");
    }
    

    return sorted;
};

/**
    Remove all the edges from an unspecified cycle. Deconnected nodes from the 
    cycle are removed too.
    Returns the nodes part of the cycle in an array or null if no cycle could
    be found.

    In case of nested cycles like:
    A -> B, B -> C, C -> B, C -> A,
    which of the cycle is removed first is unspecified.

    forceNodeRemoval: Remove nodes part of the cycle even if there is still
                      edges between them and other non-cycle nodes.
*/
graph.adjencyList.prototype.removeCycle = function (forceNodeRemoval)
{
    var nodes = [];
    nodes.length = this.nodes.length;
    var cycle = [];
    var stack = [];
    var that = this;
    var frame, lastFrame;
    var itNb = 0;
    var succ;
    var head;
    var current;
    var i;

    if (forceNodeRemoval === undefined)
    {
        forceNodeRemoval = false;
    }

    // The general idea is to start successively from all the nodes of the graph
    // and perform a DFS until either we have visited all children or we found a
    // cycle.
    //
    // To make things faster, a different iteration number is assigned for each walk
    // from one of the nodes.  A cycle is discovered if we stumble upon a node 
    // with the same iteration number.  If we stumple upon a node with a lesser
    // iteration number, we know it is not part of a cycle since it would have
    // been discovered in the previous walk. Therefore, we explore each subgraph
    // only once.
    // 
    // pred is the predecessor frame to be able to walk the graph in the
    //      opposite direction once we found a cycle
    // node is the node being visited in that frame
    this.nodes.map(function (node) 
                   { 
                       if (node !== undefined) 
                       { 
                           stack.push({pred:null, node:node}); 
                       }
                   });

    while (stack.length > 0)
    {
        frame = stack.pop();

        if (frame.pred === null)
        {
           // Increment the iteration number, we are doing a different walk 
           itNb++;
        }

        if (nodes[frame.node.id] === itNb)
        {
            // We found a cycle, travel back to the cycle 'head'

            // !forceNodeRemoval:
            // Start from the predecessor, since we will visit
            // the current node at the end of the cycle,
            // add each node to the cycle.  As we go along,
            // remove each edge from the cycle and remove
            // resulting disconnected nodes.
            //
            // forceNodeRemoval:
            // Remove each node as we visit them
            head = frame.node;
            succ = head;
            frame = frame.pred;
            while ( frame !== null)
            {
                current = frame.node;
                

                cycle.push(this.objectLookup[current.id]);

                if (forceNodeRemoval)
                {
                    // When forced to remove nodes, we remove
                    // the predecessor
                    this._removeNode(current);
                } else
                {
                    this._removeEdge(current, succ);
                    if (!this._isConnected(succ))
                    {
                        // This node is not connected anymore,
                        // we can remove it
                        this._removeNode(succ);
                    }
                }

                succ = current;
                frame = frame.pred;

                // If we found the head, stop, the possibly remaining
                // nodes are not part of the cycle 
                if (current === head)
                {
                    break;
                }
            }

            // We removed all edges from the cycle, 
            // check for the final node
            if (!forceNodeRemoval && !this._isConnected(succ))
            {
                this._removeNode(succ);
            }

            break;
        } else if (nodes[frame.node.id] !== undefined)
        {
            // This node was explored in a previous iteration but
            // was not part of a cycle, let's skip it.
            continue;
        } else
        {
            // This part of the graph has not been explored
            nodes[frame.node.id] = itNb;

            for (i=0; i < frame.node.outgoing.length; ++i)
            {
                succ = this.nodes[frame.node.outgoing[i]];
                stack.push({pred:frame, node:succ});
            }

        }
    }
   
    if (cycle.length > 0)
    {
        return cycle;
    } else
    {
        return null;
    }
};

/** @private remove an edge from the inner representation of the graph */
graph.adjencyList.prototype._removeEdge = function (from, to)
{
    var res = arraySetRem(from.outgoing, to.id) !== null &&
              arraySetRem(to.incoming, from.id) !== null;

    if (res)
    {
        return {from:from, to:to};
    } else 
    {
        return null;
    }
};

/**
    Remove an edge between 'from' and 'to' nodes if it exists,
    leaving the nodes in place.

    Returns the edge removed if it exists, otherwise returns null.
*/
graph.adjencyList.prototype.removeEdge = function (from, to)
{
     

};

/** 
    @private remove a node from the graph given its inner representation.
    Also, remove the given client node.
*/
graph.adjencyList.prototype._removeNode = function (node)
{
    var it;

    // Remove the node from the mapping
    var clientNode = this.objectLookup[node.id];
    this.objectLookup[node.id] = undefined;

    if (clientNode !== undefined)
    {
        this.nodeLookup[clientNode] = undefined;
    }



    // Remove the internal node
    this.nodes[node.id] = undefined;

    for (it = new ArrayIterator(node.incoming.slice(0));
         it.valid();
         it.next())
    {
        this._removeEdge(this.nodes[it.get()], node);
    }

    for (it = new ArrayIterator(node.outgoing.slice(0));
         it.valid();
         it.next())
    {
        this._removeEdge(node, this.nodes[it.get()]);
    }
    return clientNode || null;
};

/**
    Remove a node and its incoming and outgoing edges if it exists.

    Returns the removed node if it exists, otherwise returns null.
*/
graph.adjencyList.prototype.removeNode = function (node)
{


};

/** 
    @private Tells if a given node has incoming or outgoing edge given
    its inner representation index.
*/
graph.adjencyList.prototype._isConnected = function (node)
{
    return node.incoming.length > 0 || node.outgoing.length > 0;
};

/**
    Tells if a given node has incoming or outgoing edges.
*/
graph.adjencyList.prototype.isConnected = function (node)
{

};

/** 
    Returns an iterator to the nodes. Complexity depends on 
    the type of the order.

    "topologicalSort"
    O(|E| + |N|) where |E| is the number of edges 
    and |N| is the number of nodes.


*/
graph.adjencyList.prototype.getNodeItr = function (type)
{
    if (type === undefined)
    {
        type = "topologicalSort";
    }

    assert(type === "topologicalSort", 
           "unsupported iterator type:'" + type + "'");
    var it = Object.create(this.getNodeItr.prototype); 
    it.it = new ArrayIterator(this._topologicalSort());
    it.graph = this;
    return it;
};

graph.adjencyList.prototype.getNodeItr.prototype = new Iterator();

/** Tells whether all items have been visited */
graph.adjencyList.prototype.getNodeItr.prototype.valid = function ()
{
    return this.it.valid();
};
/** Move iterator to the next item */
graph.adjencyList.prototype.getNodeItr.prototype.next = function ()
{
    return this.it.next();
};
/** Return the current item */
graph.adjencyList.prototype.getNodeItr.prototype.get = function ()
{
    return this.graph.objectLookup[this.it.get()]; 
};
