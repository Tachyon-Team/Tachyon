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
    RegExp graph types.

    @author
    Olivier Matz
*/

var DEBUG = false;

/**
    RegExp execution context.
    @params: {String} input
    @params: {REGroup} rootGroup
*/ 
function REContext (
    input,
    rootGroup
)
{
    this.input = input;
    this.index = 0;
    this.rootGroup = rootGroup;
    this.btstack = [];
    this.activeCaps = [];
    this.btactive = 0;
}

/**
    Returns current character code.
*/
REContext.prototype.current = function ()
{
    return this.input.charCodeAt(this.index);    
}

/**
    Returns character code at current index + n.
    @params: {Integer} n
*/ 
REContext.prototype.lookahead = function (
    n
)
{
    return this.input.charCodeAt(this.index + n);
}

/**
    Returns true if at end of input, false otherwise.
*/ 
REContext.prototype.eol = function ()
{
    return this.index >= this.input.length;
}

/**
    Consume current character (add to each active captures) and
    increment current index.
*/ 
REContext.prototype.consume = function ()
{
    if (DEBUG)
        print("### consuming " + this.input[this.index]);
    for (var i = 0; i < this.activeCaps.length; ++i)
        this.activeCaps[i].add(this.index);
    this.index++;
}

/**
    Returns an object that stores the current state of the context.
*/ 
REContext.prototype.dump = function ()
{
    var activeCapsCopy = [];
    var groupCopy = this.rootGroup.dump();

    for (var i = 0; i < this.activeCaps.length; ++i)
        activeCapsCopy.push(this.activeCaps[i]);
    return [this.index, groupCopy, activeCapsCopy];
}

/**
    Restore the context object with data previously given by dump().
    @params: {Object} copy, data from dump().
*/ 
REContext.prototype.restore = function (
    copy
)
{
    this.index = copy[0];
    this.rootGroup.restore(copy[1]);
    this.activeCaps = copy[2];
}

/**
    Returns an array of all capture extraction.
    @params: {String} input
*/ 
REContext.prototype.extractCaptures = function (
    input
)
{
    var matches = [];

    this.rootGroup.extractCaptures(input, matches);
    if (matches.length == 1)
        return matches[0];
    return matches;
}

/**
    Returns the RECapture object at the given index.    
    @params: {Integer} index
*/ 
REContext.prototype.getCaptureByIndex = function (
    index
)
{
    var i = {};
    i.index = index;
    return this.rootGroup.getCaptureByIndex(i);
}

/**
*/ 
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

/**
    Capture class.
*/ 
function RECapture ()
{
    this.start = null;
    this.end = null;
    this.activated = false;
}

/**
    Add index to the capture. 
*/
RECapture.prototype.add = function (
    index
)
{
    if (this.start == null)
        this.start = index;
    this.end = index;
}

/**
    Clears the capture.
*/ 
RECapture.prototype.clear = function ()
{
    this.activated = false;
    this.start = this.end = null;
}

/**
    Extract capture substring from input.
*/ 
RECapture.prototype.extract = function (
    input
)
{
    if (this.start != null)
        return input.substring(this.start, this.end + 1);
    else if (this.activated)
        return "";
    else
        return undefined;
        
}

/**
    Returns an object that stores the current state of the capture.
*/ 
RECapture.prototype.dump = function ()
{
    var copy = new RECapture();
    copy.start = this.start;
    copy.end = this.end;
    copy.activated = this.activated;
    return copy;
}

/**
    Restore the capture object with data previously given by dump().
    @params: {Object} cap, data from dump().
*/
RECapture.prototype.restore = function (
    cap
)
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

function RELoopContext ()
{
    this.lastIndex = -1;
}

function RELoopPrefixEdge (dest, loopContext)
{
    this.dest = dest;
    this.loopContext = loopContext;
}

RELoopPrefixEdge.prototype.exec = function (context)
{
    if (DEBUG)
        print(context.index + ": exec loop prefix edge");
    this.loopContext.lastIndex = context.index;
    return this.dest;
}

function RELoopEdge (dest, loopContext)
{
    this.dest = dest;
    this.loopContext = loopContext;
}

RELoopEdge.prototype.exec = function (context)
{
    if (DEBUG)
        print(context.index + ": exec loop edge");

    if (context.index == this.loopContext.lastIndex)
        return null;
    return this.dest;
}

function RELoopExitEdge (dest, loopContext)
{
    this.dest = dest;
    this.loopContext = loopContext;
}

RELoopExitEdge.prototype.exec = function (context)
{
    if (DEBUG)
        print(context.index + ": exec loop exit edge");
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
        return this.dest;

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

function REMultilineBOLAssertEdge (dest)
{
    this.dest = dest;
}

REMultilineBOLAssertEdge.prototype.exec = function (context)
{
    if (DEBUG)
        print(context.index + ": exec multiline beginning of line assertion");

    if (context.index == 0)
        return this.dest;

    if (context.lookahead(-1) == 10 || context.lookahead(-1) == 13 ||
        context.lookahead(-1) == 8232 || context.lookahead(-1) == 8233)
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

function REMultilineEOLAssertEdge (dest)
{
    this.dest = dest;
}

REMultilineEOLAssertEdge.prototype.exec = function (context)
{
    if (DEBUG)
        print(context.index + ": exec multiline beginning of line assertion");

    if (context.index == context.input.length)
        return this.dest;

    // Is current character a LineTerminator ?
    if (context.current() == 10 || context.current() == 13 ||
        context.current() == 8232 || context.current() == 8233)
        return this.dest;

    return null;
}

function REWordBoundary (dest, positive)
{
    this.dest = dest;
    this.positive = positive;
}

REWordBoundary.prototype.exec = function (context)
{
    var a = this.isWordChar(context.lookahead(-1));
    var b = this.isWordChar(context.current());
    if ((a != b) && this.positive)
        return this.dest;
    else if ((a == b) && !this.positive)
        return this.dest;
    return null;
}

REWordBoundary.prototype.isWordChar = function (charCode)
{
    return ((charCode >= 97 && charCode <= 122) ||
            (charCode >= 65 && charCode <= 90) ||
            (charCode >= 48 && charCode <= 57) ||
            charCode == 95);
}

function REAssertContext ()
{
    this.indexSave = -1;
    this.activeCaps = [];
}

function REAssertOpenEdge (dest, assertContext)
{
    this.dest = dest;
    this.assertContext = assertContext;
}

REAssertOpenEdge.prototype.exec = function (context)
{
    if (DEBUG)
        print(context.index + ": exec assert open edge");

    context.btactive++;
    // Desactivate and save captures.
    while (context.activeCaps.length > 0)
        this.assertContext.activeCaps.push(context.activeCaps.pop());
    this.assertContext.indexSave = context.index;
    return this.dest;
}

function REAssertCloseEdge (dest, assertContext)
{
    this.dest = dest;
    this.assertContext = assertContext;
}

REAssertCloseEdge.prototype.exec = function (context)
{
    if (DEBUG)
        print(context.index + ": exec assert close edge");

    // Restore active captures.
    for (var i = 0; i < this.assertContext.activeCaps.length; ++i)
        context.activeCaps.push(this.assertContext.activeCaps[i]);
    context.index = this.assertContext.indexSave;
    context.btactive--; 
    return this.dest;
}

function RENegAssertContext ()
{
    this.matchSuccess = false;
}

function RENegAssertOpenEdge (dest, assertContext)
{
    this.dest = dest;
    this.assertContext = assertContext;
}

RENegAssertOpenEdge.prototype.exec = function (context)
{
    if (DEBUG)
        print(context.index + ": exec neg assert open edge");

    this.assertContext.matchSuccess = false;
    return this.dest;
}

function RENegAssertOutEdge (dest, assertContext)
{
    this.dest = dest;
    this.assertContext = assertContext;
}

RENegAssertOutEdge.prototype.exec = function (context)
{
    if (DEBUG)
        print(context.index + ": exec neg assert out edge");

    if (this.assertContext.matchSuccess)
        return null;
    return this.dest;
}

function RENegAssertMatchEdge (assertContext)
{
    this.assertContext = assertContext;
}

RENegAssertMatchEdge.prototype.exec = function (context)
{
    if (DEBUG)
        print(context.index + ": exec neg assert match edge");

    this.assertContext.matchSuccess = true;
    return null;
}

