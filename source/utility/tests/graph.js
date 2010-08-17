/**
@fileOverview

Tests for graph data structure.

@copyright
Copyright (c) 2010 Tachyon Javascript Engine, All Rights Reserved
*/

(function () { // local namespace
    tests.graph = tests.testSuite();
    tests.graph.adjencyList = tests.testSuite();
    var t = tests.graph.adjencyList;

    t.setup = function ()
    {
        this.g = graph.adjencyList();
    };

    t.simpleTopologicalSort = function ()
    {
        var expectedOrder = [3,1,2];
        var nodeIt;

        this.g.addEdge(3, 1);
        this.g.addEdge(1, 2);

        expectedOrder.reverse();
        for (nodeIt = this.g.getNodeIterator("topologicalSort");
             !nodeIt.end();
             nodeIt.next())
        {
           assert(expectedOrder.pop() === nodeIt.get()); 
        }

        assert(expectedOrder.length === 0);
    };

    t.simpleStringTopologicalSort = function ()
    {
        var expectedOrder = ["C","A","B"];
        var nodeIt;

        this.g.addEdge("C", "A");
        this.g.addEdge("A", "B");

        expectedOrder.reverse();
        for (nodeIt = this.g.getNodeIterator("topologicalSort");
             !nodeIt.end();
             nodeIt.next())
        {
           assert(expectedOrder.pop() === nodeIt.get()); 
        }

        assert(expectedOrder.length === 0);
    };
})(); // end of local namespace 
