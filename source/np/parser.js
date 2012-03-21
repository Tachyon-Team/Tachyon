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

/**
Parse a JavaScript source file
*/
function parseFile(fileName)
{
    var str = readFile(fileName);

    return parseString(str, fileName);
}

/**
Parse a JavaScript source string
*/
function parseString(str, fileName)
{
    if (fileName === undefined)
        fileName = '';

    // Create a lexer for this file
    var lexer = new Lexer(str, fileName);

    return parseProgram(lexer);
}

/**
Parse a source code unit or program
*/
function parseProgram(lexer)
{
    var program = new ASTProgram();
 
    // For each program element
    for (;;)
    {
        // TODO: peekToken() vs readToken()?
        // probably want this

        var l = lexer.getToken();



        if (l.type === 'eof')
        {
            break;
        }

        else if (l.type === 'function')
        {
            var node = parseFuncDecl();
            program.addChild(node);
        }

        else
        {
            // TODO
            //var node = parseStmt();
            //program.addChild(node);
        }
    }

    // Set the program's source position
    if (program.hasChildren())
        program.pos = SrcPos.merge(program.firstChild().pos, program.lastChild().pos);
    else
        program.pos = l.pos;

    return program;
}

function parseFuncDecl()
{
    // TODO
}

function parseStmt()
{


    // TODO: variable declaration


    // TODO






    // TODO: by default, expression statement
    //return parseExpr();
}

function parseExpr()
{
    // TODO
}



// TODO: standard way of generating operator/expression parsing?
// priority, keywords







