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
Test if a character code is whitespace
*/
function charIsWS(charCode)
{
    switch (charCode)
    {
        case 9:     // Tab
        case 10:    // Line feed
        case 11:    // Vertical tab
        case 12:    // Form feed
        case 13:    // Carriage return
        case 32:    // Space
        case 160:   // Non-breaking space
        case 65279: // Byte-order mark
        return true;

        default:
        return false;
    }
}

/**
Test if a character code is a digit
*/
function charIsDigit(charCode)
{
    return (charCode >= 48 && charCode <= 57);
}

/**
Test if a character code is a hexadecimal digit
*/
function charIsHexDigit(charCode)
{
    return (
        (charCode >= 48 && charCode <= 57)  ||  // 0-9
        (charCode >= 97 && charCode <= 122) ||  // a-z
        (charCode >= 64 && charCode <= 90)      // A-Z
    );
}

/**
@class JavaScript parser
*/
function JSParser(str, fileName)
{
    /**
    Code string to parse
    */
    this.str = str;

    /**
    File being parsed
    */
    this.fileName = fileName;

    /**
    Current character index
    */
    this.curIdx = 0;

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

JSParser.prototype.peekCh = function (offset)
{
    if (offset === undefined)
        offset = 0;

    return this.str.charCodeAt(this.curIdx + offset);
}

JSParser.prototype.readCh = function ()
{
    return this.str.charCodeAt(this.curIdx++);
}

JSParser.prototype.match = function (token)
{
    // TODO
}

JSParser.prototype.mustMatch = function (token)
{
    // TODO: if no match, throw error
}

JSParser.prototype.parseProgram = function ()
{
    // TODO
}

JSParser.prototype.parseFuncDecl = function ()
{
    // TODO
}

JSParser.prototype.parseStmt = function ()
{
    // TODO
}

JSParser.prototype.parseExpr = function ()
{
    // TODO
}



// TODO: standard way of generating operator/expression parsing?
// priority, keywords







