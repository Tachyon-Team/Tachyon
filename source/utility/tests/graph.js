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
        for (nodeIt = this.g.getNodeItr("topologicalSort");
             nodeIt.valid();
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
        for (nodeIt = this.g.getNodeItr("topologicalSort");
             nodeIt.valid();
             nodeIt.next())
        {
           assert(expectedOrder.pop() === nodeIt.get()); 
        }

        assert(expectedOrder.length === 0);
    };

    t.removeCycleEmpty = function ()
    {
        var res = this.g.removeCycle();

        assert(res === null);
    };

    t.removeCycleNoCycle = function ()
    {
        this.g.addEdge("A", "B");
        this.g.addEdge("B", "C");

        var res = this.g.removeCycle();

        assert(res === null);
    };

    t.removeCycleOneNodeCycle = function ()
    {
        this.g.addEdge("A", "A");

        var res = this.g.removeCycle();

        assert(res.length === 1 && res[0] === "A");

        res = this.g.removeCycle();
        assert(res === null);
    };

    t.removeCycleTwoNodeCycle = function ()
    {
        var expectedCycle = ["A", "B"];

        this.g.addEdge("A", "B");
        this.g.addEdge("B", "A");

        var res = this.g.removeCycle();

        assert(res.length === 2);

        arraySetRem(expectedCycle, res[0]);
        arraySetRem(expectedCycle, res[1]);

        assert(expectedCycle.length === 0);

        res = this.g.removeCycle();
        assert(res === null);

    };

    t.removeCycleDelayedCycle = function ()
    {
        var expectedCycle = ["A", "B"];

        this.g.addEdge("C", "A");
        this.g.addEdge("A", "B");
        this.g.addEdge("B", "A");

        var res = this.g.removeCycle();

        assert(res.length === 2);

        arraySetRem(expectedCycle, res[0]);
        arraySetRem(expectedCycle, res[1]);

        assert(expectedCycle.length === 0);

        res = this.g.removeCycle();
        assert(res === null);
    };

    t.removeTwoOneNodeCycle = function ()
    {
        this.g.addEdge("A", "A");
        this.g.addEdge("B", "B");

        var res = this.g.removeCycle();
        assert(res.length === 1);

        res = this.g.removeCycle();
        assert(res.length === 1);

        res = this.g.removeCycle();
        assert(res === null);
    };

    t.topologicalSortWithCycle = function ()
    {
        var expectedCycle = ["A", "B"];
        var expectedOrder = ["A", "C", "D"];
        var nodeIt;

        this.g.addEdge("A", "B");
        this.g.addEdge("B", "A");

        this.g.addEdge("A", "C");
        this.g.addEdge("C", "D");

        var cycle = this.g.removeCycle();
        assert(cycle.length === 2);

        arraySetRem(expectedCycle, cycle[0]);
        arraySetRem(expectedCycle, cycle[1]);

        assert(expectedCycle.length === 0);

        var cycle2 = this.g.removeCycle();
        assert(cycle2 === null);

        expectedOrder.reverse();
        for (nodeIt = this.g.getNodeItr("topologicalSort");
             nodeIt.valid();
             nodeIt.next())
        {
           assert(expectedOrder.pop() === nodeIt.get()); 
        }

        assert(expectedOrder.length === 0);
    };

    t.topologicalSortWithCycleForceNodeRemoval = function ()
    {
        var expectedCycle = ["A", "B"];
        var expectedOrder = ["C", "D"];
        var nodeIt;

        this.g.addEdge("A", "B");
        this.g.addEdge("B", "A");

        this.g.addEdge("A", "C");
        this.g.addEdge("C", "D");

        var cycle = this.g.removeCycle(true);
        assert(cycle.length === 2);

        arraySetRem(expectedCycle, cycle[0]);
        arraySetRem(expectedCycle, cycle[1]);

        assert(expectedCycle.length === 0);

        var cycle2 = this.g.removeCycle();
        assert(cycle2 === null);

        expectedOrder.reverse();
        for (nodeIt = this.g.getNodeItr("topologicalSort");
             nodeIt.valid();
             nodeIt.next())
        {
           assert(expectedOrder.pop() === nodeIt.get()); 
        }

        assert(expectedOrder.length === 0);
    };

    t.topologicalSortWithCycleForceNodeRemovalReverse = function ()
    {
        var expectedCycle = ["A", "B"];
        var expectedOrder = ["D", "C"];
        var nodeIt;

        this.g.addEdge("A", "B");
        this.g.addEdge("B", "A");

        this.g.addEdge("C", "A");
        this.g.addEdge("D", "C");

        var cycle = this.g.removeCycle(true);
        assert(cycle.length === 2);

        arraySetRem(expectedCycle, cycle[0]);
        arraySetRem(expectedCycle, cycle[1]);

        assert(expectedCycle.length === 0);

        var cycle2 = this.g.removeCycle();
        assert(cycle2 === null);

        expectedOrder.reverse();
        for (nodeIt = this.g.getNodeItr("topologicalSort");
             nodeIt.valid();
             nodeIt.next())
        {
           assert(expectedOrder.pop() === nodeIt.get()); 
        }

        assert(expectedOrder.length === 0);
    };
})(); // end of local namespace 
