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
    ast,
    global,
    ignoreCase,
    multiline
)
{
    var head = new RENode();
    var rootCap = new RECapture();
    this.rootGroup = new REGroup(rootCap);
    this.groupParents = [this.rootGroup];
    this.groupDisjunction = [];
    this.global = global;
    this.ignoreCase = ignoreCase;
    this.multiline = multiline;
    
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

    if (altEdges.length == 0)
        // Empty alternative set.
        node1.add([edge2]);

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
    if (astNode.prefix instanceof REAtom)
    {
        var min = astNode.quantifier.min;
        var max = astNode.quantifier.max;
        var greedy = astNode.quantifier.greedy;
        var atom = astNode.prefix;
        var edges = outEdges;
        var group = undefined;
        var newGroup = false;

        if (astNode.prefix.value instanceof REDisjunction)
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
            // Build atom loop. 
            var loopContext = new RELoopContext();
            var loopNode = new RENode();
            var prefixNode = new RENode();
            var exitNode = new RENode();

            var enterEdge = new RENullEdge(loopNode);
            var prefixEdge = new RELoopPrefixEdge(prefixNode, loopContext);
            var loopEdge = new RELoopEdge(loopNode, loopContext);
            var exitEdge = new RELoopExitEdge(exitNode, loopContext);
            var atomEdges = this.compileAtom(astNode.prefix, [loopEdge], group);

            if (greedy)
                atomEdges.push(exitEdge);
            else
                atomEdges.unshift(exitEdge);

            prefixNode.add(atomEdges);
            loopNode.add([prefixEdge]);
            exitNode.add(edges);
            edges = [enterEdge];

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
    else if (astNode.prefix instanceof REAssertion)
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
    if (astNode.value instanceof REPatternCharacter)
    {
        var node = new RENode();
        var edge, charCode = astNode.value.value;
        
        if (this.ignoreCase)
        {
            if (charCode >= 97 && charCode <= 122)
                egde = new RECharMatchEdge(node, [charCode - 32, charCode]);
            else if (charCode >= 65 && charCode <= 90)
                egde = new RECharMatchEdge(node, [charCode, charCode + 32]);
            else 
                edge = new RECharMatchEdge(node, charCode);
        }
        else
        {
            edge = new RECharMatchEdge(node, charCode);
        }
        
        node.add(outEdges);
        return [edge];
    }
    else if (astNode.value instanceof RECharacterClass)
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
    else if (astNode.value instanceof REDisjunction)
    {
        return this.compileDisjunction(astNode.value, outEdges, group);
    }
    else if (astNode.value instanceof REBackReference)
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
    if (astNode.value instanceof REDisjunction)
    {
        var group = new REGroup();
        var groupParent = this.groupParents[this.groupParents.length - 1];
        groupParent.subgroups.push(group);
        this.groupParents.push(group);
        this.groupDisjunction[astNode.value.captureIndex] = group;
        var openEdge;

        if (astNode.positive)
        {
            var assertContext = new REAssertContext();
            var openNode = new RENode();
            var closeNode = new RENode();
            
            openEdge = new REAssertOpenEdge(openNode, assertContext);        
            var closeEdge = new REAssertCloseEdge(closeNode, assertContext);        
            var assertEdges = this.compileDisjunction(astNode.value, [closeEdge], group);

            openNode.add(assertEdges);
            closeNode.add(outEdges);
        }
        else
        {
            var negAssertContext = new RENegAssertContext();
            var choiceNode = new RENode();
            var openNode = new RENode();
            var outNode = new RENode();

            openEdge = new RENegAssertOpenEdge(choiceNode, negAssertContext);
            var assertOpenEdge = new REAssertOpenEdge(openNode, new REAssertContext());
            var negMatchEdge = new RENegAssertMatchEdge(negAssertContext);
            var negOutEdge = new RENegAssertOutEdge(outNode, negAssertContext);
            var assertEdges = this.compileDisjunction(astNode.value, [negMatchEdge], group);

            assertEdges.push(negOutEdge);
            choiceNode.add([assertOpenEdge, negOutEdge]);
            openNode.add(assertEdges);
            outNode.add(outEdges);
        }

        this.groupParents.pop();
        return [openEdge];
    }
    else
    {
        var node = new RENode();
        var edge;

        if (astNode.value == 94) // '^'
        {
            if (this.multiline)
                edge = new REMultilineBOLAssertEdge(node);
            else
                edge = new REBOLAssertEdge(node);
        }
        else if (astNode.value == 36) // '$'
        {
            if (this.multiline)
                edge = new REMultilineEOLAssertEdge(node);
            else
                edge = new REEOLAssertEdge(node);
        }
        else if (astNode.value == 98) // 'b' | 'B'
        {
            edge = new REWordBoundary(node, astNode.positive);
        }

        node.add(outEdges);
        return [edge];
    }
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

