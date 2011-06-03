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

function REGraph (rootGroup, head)
{
    this.rootGroup = rootGroup;
    this.head = head;
}

function REAstToGraph() {}

REAstToGraph.prototype.compile = function (
    ast
)
{
    var head = new RENode();
    var rootCap = new RECapture();
    this.rootGroup = new REGroup(rootCap);
    this.groupParents = [this.rootGroup];
    this.groupDisjunction = [];

    head.add(this.compileDisjunction(ast, [], this.rootGroup));
    return new REGraph(this.rootGroup, head);
}

REAstToGraph.prototype.compileDisjunction = function (
    astNode,
    outEdges,
    group
)
{
    var node1 = new RENode();
    var node2 = new RENode();
    var node3 = new RENode(outEdges.length == 0);
    var edge1 = new REGroupOpenEdge(node1, group);
    var edge2 = new RENullEdge(node2);
    var edge3 = new REGroupCloseEdge(node3, group);
    var altEdges = [];

    for (var i = astNode.alternatives.length; i > 0; --i)
        altEdges.push(this.compileAlternative(astNode.alternatives[i - 1], [edge2]));

    for (var i = altEdges.length; i > 0; --i)
        node1.add(altEdges[i - 1]);
    node2.add([edge3]);
    node3.add(outEdges);
    return [edge1];
}

REAstToGraph.prototype.compileAlternative = function (
    astNode,
    outEdges
)
{
    var edges = outEdges;

    for (var i = astNode.terms.length; i > 0; --i)
        edges = this.compileTerm(astNode.terms[i - 1], edges);
    return edges;
}

REAstToGraph.prototype.compileTerm = function (
    astNode,
    outEdges
)
{
    if (astNode.prefix instanceof RegExpAtom)
    {
        var min = astNode.quantifier.min;
        var max = astNode.quantifier.max;
        var greedy = astNode.quantifier.greedy;
        var atom = astNode.prefix;
        var edges = outEdges;
        var group = undefined;
        var newGroup = false;

        if (astNode.prefix.value instanceof RegExpDisjunction)
        {
            group = this.groupDisjunction[astNode.prefix.value.captureIndex];

            if (group == undefined)
            {
                if (astNode.prefix.value.type == 0)
                    group = new REGroup(new RECapture());
                else
                    group = new REGroup();

                var groupParent = this.groupParents[this.groupParents.length - 1];
                groupParent.subgroups.push(group);
                this.groupParents.push(group);
                this.groupDisjunction[astNode.prefix.value.captureIndex] = group;
                newGroup = true;
            }
        }

        if (max < 0)
        {
            // Loop.
            var node = new RENode();
            var loopEdge = new RELoopEdge(node);
            var atomEdges = this.compileAtom(astNode.prefix, [loopEdge], group);

            if (greedy)
                node.add(atomEdges.concat(edges));
            else
                node.add(edges.concat(atomEdges));
            edges = [loopEdge];

            for (; min > 0; --min)
                edges = this.compileAtom(astNode.prefix, edges, group);
        }
        else if (min < max)
        {
            max = max - min;
            for (; max > 0; --max)
                edges = this.compileAtom(astNode.prefix, greedy ? edges.concat(outEdges) : outEdges.concat(edges), group);
            if (greedy)
                edges = edges.concat(outEdges);
            else
                edges = outEdges.concat(edges);

            for (; min > 0; --min)
                edges = this.compileAtom(astNode.prefix, edges, group);
        }
        else
        {
            for (; min > 0; --min)
                edges = this.compileAtom(astNode.prefix, edges, group);
        }

        if (newGroup)
            this.groupParents.pop();
        return edges;
    }
    else if (astNode.prefix instanceof RegExpAssertion)
    {
        return this.compileAssertion(astNode.prefix, outEdges);
    }
}

REAstToGraph.prototype.compileAtom = function (
    astNode,
    outEdges,
    group
)
{
    if (astNode.value instanceof RegExpPatternCharacter)
    {
        var node = new RENode();
        var edge = new RECharMatchEdge(node, astNode.value.value);
        node.add(outEdges);
        return [edge];
    }
    else if (astNode.value instanceof RegExpCharacterClass)
    {
        var node = new RENode();
        var edge;
        var ranges = [];

        for (var i = 0; i < astNode.value.classAtoms.length; ++i)
        {
            var ca = astNode.value.classAtoms[i];
            ranges.push(ca.max == undefined ? ca.min.value : [ca.min.value, ca.max.value]);
        }
        if (astNode.value.type == 1)
            edge = new REExclCharSetMatchEdge(node, ranges);
        else
            edge = new RECharSetMatchEdge(node, ranges);
        node.add(outEdges);
        return [edge];
    }
    else if (astNode.value instanceof RegExpDisjunction)
    {
        return this.compileDisjunction(astNode.value, outEdges, group);
    }
    else if (astNode.value instanceof RegExpBackReference)
    {
        var node = new RENode();
        var edge = new REBackRefMatchEdge(node, astNode.value.index);
        node.add(outEdges);
        return [edge];
    }
}

REAstToGraph.prototype.compileAssertion = function (
    astNode,
    outEdges
)
{
    var node = new RENode();
    var edge;

    if (astNode.value == 94) // '^'
    {
        edge = new REBOLAssertEdge(node);
    }
    else if (astNode.value == 36) // '$'
    {
        edge = new REEOLAssertEdge(node);
    }
    node.add(outEdges);
    return [edge];
}

function printArray(array)
{
    var str = "[";
    for (var i = 0; i < array.length; ++i)
        str += array[i] + ",";
    if (array.length > 0)
        str = str.substring(0, str.length - 1) + "]";
    else
        str += "]";
    print(str);
}

