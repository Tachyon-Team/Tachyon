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

var REDEBUG = false;

function RegExp (
    pattern,
    flags
)
{
    this.source = pattern === undefined ? "" : pattern;
    this.global = false;
    this.ignoreCase = false;
    this.multiline = false;
    this.lastIndex = 0;

    // TODO: SyntaxError throwing as defined in ECMA-262 15.10.4.1
    if (typeof flags === "string")
    {
        for (var i = 0; i < flags.length; ++i)
            if (flags.charCodeAt(i) === 103) // 'g'
                this.global = true;
            else if (flags.charCodeAt(i) === 105) // 'i'
                this.ignoreCase = true;
            else if (flags.charCodeAt(i) === 109) // 'm'
                this.multiline = true;
    }

    var ast = new REParser().parse(pattern); 
    this.graph = new REAstToGraph().compile(ast, this.global, this.ignoreCase,
                                            this.multiline);
}

/**
    Anonymous function to initialize this library
*/
(function ()
{
    // Get a reference to the context
    var ctx = iir.get_ctx();

    // Set the regexp prototype object in the context
    set_ctx_regexp(ctx, RegExp);
})();

RegExp.prototype.exec = function (
    input
)
{
    var context = new REContext(input, this.graph.rootGroup);
    var cursor = this.graph.head;
    var padding = this.lastIndex;
    var i = 0;
    var next = cursor;

    context.index = this.lastIndex;
    while (next !== null ||
           padding < input.length || context.btstack.length > 0)
    {
        next = null;

        for (; i < cursor.edges.length; ++i)
        {
            var contextSave;
            if (cursor.edges.length > 1)
                contextSave = context.dump();
            var btactive = context.btactive;

            next = cursor.edges[i].exec(context);

            if (next instanceof RENode)
            {
                if (i < cursor.edges.length - 1 && btactive === 0)
                    context.btstack.push([cursor, i, contextSave]);
                cursor = next;
                i = 0;
                break;
            }
        } 

        if (next === null)
        {
            if (cursor._final)
            {
                if (this.global)
                    this.lastIndex = context.index;
                return context.extractCaptures(input);
            }

            if (context.btstack.length > 0)
            {
                if (REDEBUG)
                    print("### backtracking ...");
                var btinfo = context.btstack.pop();

                cursor = btinfo[0];
                next = cursor;
                i = btinfo[1] + 1;
                context.restore(btinfo[2]);
                context.btactive = 0;
            }
            else
            {
                i = 0;
                cursor = this.graph.head;
                padding++;
                context.index = padding;
                context.activeCaps = [];
            }
        }
    } 
    this.lastIndex = 0;
    return null; 
}

