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

function test()
{
    //
    // simpleTopologicalSort
    //

    var g = graph.adjencyList();

    var expectedOrder = [3,1,2];
    var nodeIt;

    g.addEdge(3, 1);
    g.addEdge(1, 2);

    expectedOrder.reverse();
    for (nodeIt = g.getNodeItr("topologicalSort"); nodeIt.valid(); nodeIt.next())
    {
        if (expectedOrder.pop() !== nodeIt.get())
            return 1;
    }

    if (expectedOrder.length !== 0)
        return 2;

    //
    // removeCycleTwoNodeCycle
    //

    var g = graph.adjencyList();

    var expectedCycle = ["A", "B"];

    g.addEdge("A", "B");
    g.addEdge("B", "A");

    var res = g.removeCycle();

    if (res.length !== 2)
        return 3;

    arraySetRem(expectedCycle, res[0]);
    arraySetRem(expectedCycle, res[1]);

    if (expectedCycle.length !== 0)
        return 4;

    res = g.removeCycle();

    if (res !== null)
        return 5;

    //
    // topologicalSortWithCycle
    //

    var g = graph.adjencyList();
    
    var expectedCycle = ["A", "B"];
    var expectedOrder = ["A", "C", "D"];
    var nodeIt;

    g.addEdge("A", "B");
    g.addEdge("B", "A");

    g.addEdge("A", "C");
    g.addEdge("C", "D");

    var cycle = g.removeCycle();
    if (cycle.length !== 2)
        return 6;

    arraySetRem(expectedCycle, cycle[0]);
    arraySetRem(expectedCycle, cycle[1]);

    if (expectedCycle.length !== 0)
        return 7;

    var cycle2 = g.removeCycle();
    assert(cycle2 === null);

    expectedOrder.reverse();
    for (nodeIt = g.getNodeItr("topologicalSort");
         nodeIt.valid();
         nodeIt.next())
    {
        if (expectedOrder.pop() !== nodeIt.get())
            return 8;
    }

    if (expectedOrder.length !== 0)
        return 9;

    return 0;
}

