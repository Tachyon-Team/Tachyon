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
Parser implementation.

@author
Maxime Chevalier-Boisvert
*/

// TODO: option to keep comments?

// TODO: look at grammar productions for kinds of expressions, statements, program-level

// TODO: automatic semicolon unit test

/**
@class JavaScript parser
*/
function JSParser(str, fileName)
{
    this.str = str;

    this.fileName = fileName;

    this.curPos = 0;

    this.keepComments = false;
}

JSParser.parseFile = function (fileName)
{
    // TODO
    //return JSParser.parseString();
}

JSParser.parseString = function (str, fileName)
{
    if (fileName === undefined)
        fileName = '';

    var parser = new Parser(str, fileName);

    // TODO
}

JSParser.prototype.parseProgram = function ()
{
    // TODO
}

JSParser.prototype.parseFuncDecl = function ()
{
    // TODO
}

JSParser.prototype.parseStatement = function ()
{
    // TODO
}

JSParser.prototype.parseExpression = function ()
{
    // TODO
}

// consume vs peek?


// TODO: standard way of generating operator/expression parsing?
// priority, keywords







