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

var DEBUG = false;

function REContext (input, rootGroup)
{
    this.input = input;
    this.index = 0;
    this.rootGroup = rootGroup;
    this.btstack = [];
    this.activeCaps = [];
}

REContext.prototype.current = function ()
{
    return this.input.charCodeAt(this.index);    
}

REContext.prototype.eol = function ()
{
    return this.index >= this.input.length;
}

REContext.prototype.consume = function ()
{
    if (DEBUG)
        print("### consuming " + this.input[this.index]);
    for (var i = 0; i < this.activeCaps.length; ++i)
        this.activeCaps[i].add(this.index);
    this.index++;
}

REContext.prototype.dump = function ()
{
    var activeCapsCopy = [];
    var groupCopy = this.rootGroup.dump();

    for (var i = 0; i < this.activeCaps.length; ++i)
        activeCapsCopy.push(this.activeCaps[i]);
    return [this.index, groupCopy, activeCapsCopy];
}

REContext.prototype.restore = function (copy)
{
    this.index = copy[0];
    this.rootGroup.restore(copy[1]);
    this.activeCaps = copy[2];
}

REContext.prototype.extractCaptures = function (input)
{
    var matches = [];

    this.rootGroup.extractCaptures(input, matches);
    if (matches.length == 1)
        return matches[0];
    return matches;
}

REContext.prototype.getCaptureByIndex = function (index)
{
    var i = {};
    i.index = index;
    return this.rootGroup.getCaptureByIndex(i);
}

function REGroup (capture)
{
    this.capture = capture;
    this.subgroups = [];
}

REGroup.prototype.add = function (group)
{
    this.subgroups.push(group);
}

REGroup.prototype.clearCaps = function ()
{
    if (this.capture != undefined)
        this.capture.clear();
    for (var i = 0; i < this.subgroups.length; ++i)
        this.subgroups[i].clearCaps();
}

REGroup.prototype.dump = function ()
{
    var capCopy;
    if (this.capture != undefined)
        capCopy = this.capture.dump();
    var copy = new REGroup(capCopy);

    for (var i = 0; i < this.subgroups.length; ++i)
        copy.subgroups.push(this.subgroups[i].dump());
    return copy;
}

REGroup.prototype.restore = function (copy)
{
    if (copy.capture != undefined)
        this.capture.restore(copy.capture);
    for (var i = 0; i < copy.subgroups.length; ++i)
        this.subgroups[i].restore(copy.subgroups[i]);
}

REGroup.prototype.extractCaptures = function (input, matches)
{
    if (this.capture != undefined)
        matches.push(this.capture.extract(input));
    for (var i = this.subgroups.length; i > 0; --i)
        this.subgroups[i - 1].extractCaptures(input, matches);
}

REGroup.prototype.getCaptureByIndex = function (i)
{
    if (this.capture != undefined)
    {
        if (i.index == 0) 
            return this.capture;
        i.index--;
    }

    for (var j = this.subgroups.length; j > 0; --j)
    {
        var cap = this.subgroups[j - 1].getCaptureByIndex(i);
        if (cap instanceof RECapture)
            return cap;
    }
    return null;
}

function RECapture ()
{
    this.start = null;
    this.end = null;
    this.activated = false;
}

RECapture.prototype.add = function (index)
{
    if (this.start == null)
        this.start = index;
    this.end = index;
}

RECapture.prototype.clear = function ()
{
    this.activated = false;
    this.start = this.end = null;
}

RECapture.prototype.extract = function (input)
{
    if (this.start != null)
        return input.substring(this.start, this.end + 1);
    else if (this.activated)
        return "";
    else
        return undefined;
        
}

RECapture.prototype.dump = function ()
{
    var copy = new RECapture();
    copy.start = this.start;
    copy.end = this.end;
    copy.activated = this.activated;
    return copy;
}

RECapture.prototype.restore = function (cap)
{
    this.start = cap.start;
    this.end = cap.end;
    this.activated = cap.activated;
}

function RENode (_final)
{
    this._final = _final == undefined ? false : _final;
    this.edges = [];
}

RENode.prototype.add = function (edges)
{
    for (var i = 0; i < edges.length; ++i)
        this.edges.push(edges[i]);
}

function RELoopEdge (dest)
{
    this.dest = dest;
}

RELoopEdge.prototype.exec = function (context)
{
    return this.dest;
}

function RENullEdge (dest)
{
    this.dest = dest;
}

RENullEdge.prototype.exec = function (context)
{
    if (DEBUG)
        print(context.index + ": exec null edge");
    return this.dest;
}

function REGroupOpenEdge (dest, group)
{
    this.dest = dest;
    this.group = group;
}

REGroupOpenEdge.prototype.exec = function (context)
{
    if (DEBUG)
        print(context.index + ": exec group open edge");
    this.group.clearCaps();

    if (this.group.capture != undefined)
    {
        this.group.capture.activated = true;
        context.activeCaps.push(this.group.capture);
    }
    return this.dest;
}

function REGroupCloseEdge (dest, group)
{
    this.dest = dest;
    this.group = group;
}

REGroupCloseEdge.prototype.exec = function (context)
{
    if (DEBUG)
        print(context.index + ": exec group close edge");
    if (this.group.capture != undefined)
    {
        for (var i = 0; i < context.activeCaps.length; ++i)
            if (context.activeCaps[i] === this.group.capture)
            {
                context.activeCaps.splice(i, 1);
                break;
            }
    }
    return this.dest;
}

function RECharMatchEdge (dest, charCode) 
{
    this.dest = dest;
    this.charCode = charCode;
}

RECharMatchEdge.prototype.exec = function (context)
{
    if (DEBUG)
        print(context.index + ": exec char match edge");
    if (context.current() == this.charCode)
    {
        context.consume();
        return this.dest;
    }
    return null;
}

function RECharSetMatchEdge (dest, ranges)
{
    this.dest = dest;
    this.ranges = ranges;
}

RECharSetMatchEdge.prototype.exec = function (context)
{
    if (DEBUG)
        print(context.index + ": exec char set match edge");

    if (context.eol())
        return null;

    var c = context.current();

    for (var i = 0; i < this.ranges.length; ++i)
    {
        if (typeof this.ranges[i] === "number")
        {
            if (this.ranges[i] == c)
            {
                context.consume();
                return this.dest;
            }
        }
        else
        {
            if (c >= this.ranges[i][0] && c <= this.ranges[i][1])
            {
                context.consume();
                return this.dest;
            }
        }
    }
    return null;
}

function REExclCharSetMatchEdge (dest, ranges)
{
    this.dest = dest;
    this.ranges = ranges;
}

REExclCharSetMatchEdge.prototype.exec = function (context)
{
    if (DEBUG)
        print(context.index + ": exec exclusive char set match edge");

    if (context.eol())
        return null;

    var c = context.current();

    for (var i = 0; i < this.ranges.length; ++i)
    {
        if (typeof this.ranges[i] === "number")
        {
            if (this.ranges[i] == c)
                return null;
        }
        else
        {
            if (c >= this.ranges[i][0] && c <= this.ranges[i][1])
                return null;
        }
    }
    context.consume();
    return this.dest;
}

function REBackRefMatchEdge (dest, captureIndex)
{
    this.dest = dest;
    this.captureIndex = captureIndex;
}

REBackRefMatchEdge.prototype.exec = function (context)
{
    if (DEBUG)
        print(context.index + ": exec back reference match edge");

    var capture = context.getCaptureByIndex(this.captureIndex);

    if (capture == null)
        return null;

    if (capture.start == null)
    {
        if (capture.activated)
            return this.dest;
        else
            return null;
    }

    for (var i = capture.start; i <= capture.end; ++i)
    {
        if (context.eol() || context.current() != context.input.charCodeAt(i))
            return null;
        context.consume();
    }
    return this.dest;
}

function REBOLAssertEdge (dest)
{
    this.dest = dest;
}

REBOLAssertEdge.prototype.exec = function (context)
{
    if (DEBUG)
        print(context.index + ": exec beginning of line assertion");

    if (context.index == 0)
        return this.dest;
    return null;
}

function REEOLAssertEdge (dest)
{
    this.dest = dest;
}

REEOLAssertEdge.prototype.exec = function (context)
{
    if (DEBUG)
        print(context.index + ": exec end of line assertion");
    
    if (context.index == context.input.length)
        return this.dest;
    return null;
}

function REAssertOpenEdge (dest)
{
    this.dest = dest;
}

REAssertOpenEdge.prototype.exec = function (context)
{
    return this.dest;
}

function REAssertCloseEdge (dest)
{
    this.dest = dest;
}

REAssertCloseEdge.prototype.exec = function (context)
{
    return this.dest;
}

function execre (input, graph)
{
    var context = new REContext(input, graph.rootGroup);
    var cursor = graph.head, next = graph.head;
    var padding = 0, i = 0;

    while (next != null || padding < input.length || context.btstack.length > 0)
    {
        next = null;

        for (; i < cursor.edges.length; ++i)
        {
            var contextSave = context.dump();

            next = cursor.edges[i].exec(context);

            if (next instanceof RENode)
            {
                if (i < cursor.edges.length - 1)
                    context.btstack.push([cursor, i, contextSave]);
                cursor = next;
                i = 0;
                break;
            }
        }

        if (next == null)
        {
            if (cursor._final)
                return context.extractCaptures(input);

            if (context.btstack.length > 0)
            {
                if (DEBUG)
                    print("### backtracking ...");
                var btinfo = context.btstack.pop();

                cursor = btinfo[0];
                i = btinfo[1] + 1;
                context.restore(btinfo[2]);
            }
            else
            {
                i = 0;
                cursor = graph.head;
                context.index = ++padding;
                context.activeCaps = [];
            }
        }
    } 
    return null;
}

