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
                    parseError('unexpected token ' + it.type, it);

                var expr = new ASTIdent(it.value);
                expr.pos = it.pos;

                // Peek at the next token
                var nt = lexer.peekToken();

                // If there is an equal sign
                if (nt.type === '=')
                {
                    lexer.readToken();

                    var rhs = parseAssign(lexer);

                    var assign = new ASTAssign(nt.type);
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
        return parseExpr(lexer);
    }
}

/**
Parse any kind of expression, including comma expressions.
*/
function parseExpr(lexer)
{
    // TODO
    return parseAssign(lexer);
}

/**
Parse the right-hand side of an assignment expression. 
Excludes the comma expression (lowest priority).
*/
function parseAssign(lexer)
{
    // TODO: addExpr(expr)
    // - add a constant, a an ident, or a unary operator
    // - top down descent from root expression
    // - updates acceptUnary, acceptBinary?

    // TODO: parentheses
    // - parse recursively, ASTParen protects priority

    // TODO: function call
    // - like a binary operator
    // - must bind to the right base
    // - finding current base depends on priority of call
    //   - can't steal from .

    print('assign expr');

    // Current root expression
    var rootExpr = undefined;

    // Current rightmost expression
    //var rightExpr = undefined;

    var acceptUnary = true;
    var acceptBinary = false;

    // TODO: add more checks

    // TODO: addUnary vs addBinary ************************

    /**
    Add an expression on the right side of the tree
    */
    function addExpr(tree, expr, parent)
    {
        function complete(expr)
        {
            return (
                (expr instanceof ASTUnOp   && expr.numChildren() === 1) ||
                (expr instanceof ASTBinOp  && expr.numChildren() === 2) ||
                (expr instanceof ASTAssign && expr.numChildren() === 2)
            );
        }

        //print('**** adding expr: ' + expr);

        // Identifier
        // Constant
        // Unary operator
        // Parenthesized binary operator
        if (
            expr instanceof ASTIdent ||
            expr instanceof ASTConst ||
            expr instanceof ASTUnOp  ||
            (expr instanceof ASTBinOp && expr.numChildren() === 2))
        {
            print('*** adding unary');

            if (complete(tree) === true)
            {
                addExpr(tree.lastChild(), expr, tree);
            }
            else
            {
                if (rootExpr === undefined)
                {
                    rootExpr = expr;
                }
                else
                {
                    // Make this a child of the tree node
                    tree.addChild(expr);

                    // Recompute the tree expression position
                    tree.calcPos();
                }

                if (expr instanceof ASTUnOp)
                {
                    acceptUnary = true;
                    acceptBinary = false;
                }
                else
                {
                    acceptUnary = false;
                    acceptBinary = true;
                }
            }
        }

        else if (expr instanceof ASTBinOp)
        {
            if (!(tree instanceof ASTUnOp) &&
                complete(tree) === true    && 
                complete(tree.lastChild()) === true)
            {
                addExpr(tree.lastChild(), expr, tree);
            }
            else
            {
                var treePri = tree.priority();            
                var exprPri = expr.priority();

                // If this expression has higher priority than the tree node
                if (exprPri > treePri)
                {
                    //if (rightExpr.numChildren() < 2)
                    //    parseError('malformed expression', t);

                    // Take the tree node's last child and
                    // place the new node under the tree node
                    expr.addChild(tree.lastChild());
                    tree.children[tree.children.length-1] = expr;
                }
                else
                {
                    // Make the tree node the first child of this node
                    expr.addChild(tree);
         
                    // Replace the top node by the new expression
                    if (parent)
                        parent.children[parent.children.length-1] = expr;
                    else
                        rootExpr = expr;
                }

                acceptUnary = true;
                acceptBinary = false;
            }
        }

        else if (expr instanceof ASTAssign)
        {
            if (tree.numChildren() > 0)
            {
                addExpr(tree.lastChild(), expr, tree);
            }
            else
            {
                expr.addChild(tree);

                if (parent)
                    parent.children[parent.children.length-1] = expr;
                else
                    rootExpr = expr;
            }

            acceptUnary = true;
            acceptBinary = false;
        }

        else
        {
            error('unsupported expression');
        }
    }











    // Until the expression is parsed
    for (;;)
    {
        // Peek at the current token
        var t = lexer.peekToken();

        // Terminal subexpression
        if (acceptUnary === true &&
            (
                t.type === 'number' ||
                t.type === 'string' ||
                t.type === 'true'   ||
                t.type === 'false'  ||
                t.type === 'null'   ||
                t.type === 'ident'
            ))
        {   
            print('terminal expr');

            var expr = undefined;

            lexer.readToken();
          
            // Number or string constant
            if (t.type === 'number' ||
                t.type === 'string')
            {
                expr = new ASTConst(t.value);
            }

            // true/false/null constant
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

                expr = new ASTConst(val);
            }

            // Identifier
            else if (t.type === 'ident')
            {
                expr = new ASTIdent(t.value);
            }

            // Set the expression position
            expr.pos = t.pos;

            /*
            // If there is no rightmost expression
            if (rightExpr === undefined)
            {
                rightExpr = expr;
                rootExpr = expr;
            }
            else
            {
                // Make this a child of the right expression
                rightExpr.addChild(expr);

                // Recompute the right expression position
                rightExpr.calcPos();
            }
            */

            addExpr(rootExpr, expr);
        }

        // Unary operator
        else if (
            acceptUnary === true &&
            t.type in ASTUnOp.ops)
        {
            print('unary op');

            lexer.readToken();

            var expr = new ASTUnOp(t.type);

            expr.pos = t.pos;

            /*
            // TODO: factor this common code
            // If there is no rightmost expression
            if (rightExpr === undefined)
            {
                rightExpr = expr;
                rootExpr = expr;
            }
            else
            {
                // TODO: does this now become the right expression????

                // Make this a child of the right expression
                rightExpr.addChild(expr);

                // Recompute the right expression position
                rightExpr.calcPos();
            }
            */

            addExpr(rootExpr, expr);
        }

        // Binary operator
        else if (
            acceptBinary === true &&
            t.type in ASTBinOp.ops)
        {
            if (rootExpr === undefined)
                parseError('unexpected operator', t);

            lexer.readToken();

            var binExpr = new ASTBinOp(t.type);

            /*
            // If this operator has higher priority than the
            // current expression operator
            if (rightExpr instanceof ASTBinOp &&
                ASTBinOp.ops[t.type] > ASTBinOp.ops[rightExpr.op])
            {
                if (rightExpr.numChildren() < 2)
                    parseError('malformed expression', t);

                binExpr.addChild(rightExpr.children[1]);

                rightExpr.children[1] = binExpr;
                rightExpr = binExpr;
            }
            else
            {
                binExpr.addChild(rightExpr);

                rightExpr = binExpr;
                rootExpr = binExpr;
            }
            */

            addExpr(rootExpr, binExpr);
        }

        // TODO
        // Assignment expression
        else if (
            acceptBinary === true &&
            t.type in ASTAssign.ops)
        {
            if (rootExpr === undefined)
                parseError('unexpected operator', t);

            lexer.readToken();

            var expr = new ASTAssign(t.type);

            addExpr(rootExpr, expr);
        }

        // Start of a parenthesized expression
        else if (
            acceptUnary === true &&
            t.type === '(')
        {
            //print('paren expr*******');

            lexer.readToken();

            var expr = parseExpr(lexer);

            //print('sub expr: ' + expr);

            var lt = lexer.readToken();

            if (lt.type !== ')')
                parseError('expected closing parenthesis', lt);

            /*
            // If there is no rightmost expression
            if (rightExpr === undefined)
            {
                rightExpr = expr;
                rootExpr = expr;
            }
            else
            {
                // Make this a child of the right expression
                rightExpr.addChild(expr);

                // Recompute the top expression position
                rightExpr.calcPos();
            }
            */

            addExpr(rootExpr, expr);
        }

        // TODO
        // TODO: call expression
        // TODO

        // Unrecognized token
        else
        {
            print('end of expression');

            // Stop the expression parsing
            break;
        }
    }

    if (rootExpr === undefined)
        parseError('expression parsing failed', t);

    return rootExpr;
}

