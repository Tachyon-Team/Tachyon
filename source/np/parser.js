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



// TODO: refactor parser as a class, many useful methods, no need to pass lexer argument?



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
Handle a parse error
*/
function parseError(text, token)
{
    error('parse error: ' + text + ' ' + token.pos);
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
        // Peek at the current token
        var t = lexer.peekToken();

        print('top-level token: "' + t.type + '"');

        // End of file
        if (t.type === 'eof')
        {
            break;
        }

        // Function declaration
        else if (t.type === 'function')
        {
            var node = parseFuncDecl();
            program.addChild(node);
        }

        // Statement
        else
        {
            var node = parseStmt(lexer);

            // If this is an empty statement, discard it
            if (node instanceof ASTEmpty)
                continue;

            program.addChild(node);
        }
    }

    // Set the program's source position
    if (program.hasChildren())
        program.calcPos();
    else
        program.pos = t.pos;

    return program;
}

function parseFuncDecl(lexer)
{
    // TODO
}

function parseStmt(lexer)
{
    var t = lexer.peekToken();

    print('statement: "' + t.type + '"');

    // Switch on the token type
    switch (t.type)
    {
        // Variable declaration
        // var identifier = expr, ...
        case 'var':
        {
            print('var statement');

            var varNode = new ASTVar();

            do
            {
                // Read the var keyword or preceding comma
                lexer.readToken();

                // Read the identifier
                var it = lexer.readToken();

                if (it.type !== 'ident')
                    parseError('unexpected token', it);

                var expr = new ASTIdent(it.value);
                expr.pos = it.pos;

                // Peek at the next token
                var nt = lexer.peekToken();

                // If there is an equal sign
                if (nt.type === '=')
                {
                    lexer.readToken();

                    var rhs = parseAssign(lexer);

                    var assign = new ASTAssign();
                    assign.addChild(expr);
                    assign.addChild(rhs);
                    assign.calcPos(t, rhs);

                    expr = assign;
                }

                varNode.addChild(expr);

                // Peek for a comma
                var ct = lexer.peekToken();

            // Repeat while comma
            } while (ct.type === ',')

            varNode.calcPos(t);

            return varNode;
        }

        // TODO: const

        // TODO: block statement {

        // Empty statement
        case 'newline':
        case ';':
        {
            print('empty statement');
            lexer.readToken();
            return ASTNode.make(ASTEmpty, t);
        }
        break;

        // TODO: return

        // TODO: if

        // TODO: while
        // TODO: do
        // TODO: for
        // TODO: switch
        // TODO: break
        // TODO: continue

        // TODO: try
        // TODO: catch
        // TODO: finally

        // TODO: with

        // Debugger breakpoint statement
        case 'debugger':
        {
            print('debugger statement');
            lexer.readToken();
            return ASTNode.make(ASTDebugger, t);
        }
        break;

        // By default, expression statement
        default:     
        return parseExpr();
    }
}

/**
Parse any kind of expression, including comma expressions.
*/
function parseExpr(lexer)
{
    // TODO
}

/**
Parse the right-hand side of an assignment expression. 
Excludes the comma expression (lowest priority).
*/
function parseAssign(lexer)
{
    /*
    TODO: standard way of generating operator/expression parsing?
    operator priority?

    Binary expressions
    + - * / %
    && ||
    & | ^
    << >> >>>
    < <= > >= == ===
    = += -= *= /= %= <<= <<<= >>=
    instanceof
    in
    , comma
    . period

    Unary expressions
    ~ !
    ++ --
    typeof new

    Ternary expression
    ? :

    Brackets and parentheses
    [x] (x)

    Function call


    Idea: parsing an expression from left to right, need to keep track of the
    current topmost node of the expression subtree

    When we encounter an operator, we create a new matching node, make the current
    node the corresponding child.

    May need some context for the ? expression (condExpr)

    Parsing an expression, if we see each operator, take appropriate response.
    Need to be able to know priority of each operator.

    Priority mapping?

    */

    print('assign expr');


    // TODO



    var t = lexer.peekToken();

    var expr = undefined;

    if (t.type === 'number' ||
        t.type === 'string')
    {    
        print('constant expr');
        lexer.readToken();
        expr = new ASTConst(t.value);
        expr.pos = t.pos;
    }

    else if (
        t.type === 'true'   ||
        t.type === 'false'  ||
        t.type === 'null')
    {
        var val;
        if (t.type === 'true')
            val = true;
        else if (t.type === 'false')
            val = false;
        else
            val === 'null';

        print('constant expr');
        lexer.readToken();
        expr = new ASTConst(val);
        expr.pos = t.pos;
    }

    else if (t.type === 'ident')
    {
        print('ident expr');
        lexer.readToken();
        expr = new ASTIdent(t.value);
        expr.pos = t.pos;
    }







    return expr;
}

