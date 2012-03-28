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
Abstract Syntax Tree (AST) class hierarchy.

@author
Maxime Chevalier-Boisvert
*/

/**
@class Source code position
*/
function SrcPos(
    fileName,
    startLine,
    startCol,
    endLine,
    endCol
)
{
    this.fileName = fileName;

    this.startLine = startLine;

    this.startCol = startCol;

    this.endLine = endLine;

    this.endCol = endCol;
}

SrcPos.merge = function (startPos, endPos)
{
    assert (
        startPos.fileName === endPos.fileName,
        'file names do not match:\n' +
        '"' + startPos.fileName + '"\n' +
        '"' + endPos.fileName + '"'
    );

    return new SrcPos(
        startPos.fileName,
        startPos.startLine,
        startPos.startCol,
        endPos.endLine,
        endPos.endCol
    );
}

SrcPos.prototype.toString = function ()
{
    return (
        '"' + this.fileName + '"@' +
        this.startLine + '.' + this.startCol + '-' +
        this.endLine + '.' + this.endCol
    );
}

/**
@class Base class for AST nodes
*/
function ASTNode()
{
    this.pos = undefined;

    this.children = undefined;
}

/**
Create an AST node and set its position
*/
ASTNode.make = function (ctor, pos)
{
    if (pos instanceof Lexeme)
        pos = pos.pos;

    var node = new ctor();

    node.pos = pos;

    return node;
}

/**
Add a child to this node
*/
ASTNode.prototype.addChild = function (node)
{
    if (this.children === undefined)
        this.children = [];

    this.children.push(node);
}

/**
Test if this node has children
*/
ASTNode.prototype.hasChildren = function ()
{
    return (this.children !== undefined);
}

/**
Get the first child of this node
*/
ASTNode.prototype.firstChild = function ()
{
    assert (
        this.children !== undefined,
        'node has no children'
    );

    return this.children[0];
}

/**
Get the first child of this node
*/
ASTNode.prototype.lastChild = function ()
{
    assert (
        this.children !== undefined,
        'node has no children'
    );

    return this.children[this.children.length - 1];
}

/**
Get an iterator for the children of this node
*/
ASTNode.prototype.getChildItr = function ()
{
    // TODO
}

/**
Calculate a node's position based on its children
*/
ASTNode.prototype.calcPos = function (startPos)
{
    assert (
        this.hasChildren() === true,
        'node has no children'
    );

    if (startPos instanceof Lexeme)
        startPos = startPos.pos;

    this.pos = SrcPos.merge(
        (startPos !== undefined)? startPos:this.firstChild().pos, 
        this.lastChild().pos
    );
}

/**
@class Comment node
@extends ASTNode
*/
function ASTComment(text)
{
    this.text = text;
}
ASTComment.prototype = new ASTNode();

/**
@class Top-level program node (code unit, source file)
@extends ASTNode
*/
function ASTProgram()
{
}
ASTProgram.prototype = new ASTNode();

function ASTFunction(name)
{
    this.name = name;
}
ASTFunction.prototype = new ASTFunction();

/**
@class AST statement node
@extends ASTNode
*/
function ASTStmt()
{
}
ASTStmt.prototype = new ASTNode();

/**
@class Variable delcaration statement
@extends ASTStmt
*/
function ASTVar()
{
}
ASTVar.prototype = new ASTStmt();

/**
@class Empty statement
@extends ASTStmt
*/
function ASTEmpty()
{
}
ASTEmpty.prototype = new ASTStmt();

/**
@class Block statement
@extends ASTStmt
*/
function ASTBlock()
{
}
ASTBlock.prototype = new ASTStmt();

/**
@class Debugger breakpoint statement
@extends ASTStmt
*/
function ASTDebugger()
{
}
ASTDebugger.prototype = new ASTStmt();

// TODO
// TODO: other kinds of statements
// TODO

/**
@class AST expression node
@extends ASTNode
*/
function ASTExpr()
{
}
ASTExpr.prototype = new ASTNode();

/**
@class Constant expression
@extends ASTConst
*/
function ASTConst(value)
{
    this.value = value;
}
ASTConst.prototype = new ASTExpr();

/**
@class Identifier expression
@extends ASTExpr
*/
function ASTIdent(name)
{
    this.name = name;
}
ASTIdent.prototype = new ASTExpr();

/**
@class Assignment expression
@extends ASTExpr
*/
function ASTAssign()
{
}
ASTAssign.prototype = new ASTExpr();

/**
@class Base class for binary expressions
@extends ASTExpr
*/
function ASTBinOp()
{
}
ASTBinOp.prototype = new ASTExpr();

// TODO
// TODO: other kinds of expressions
// TODO
//
// Assign
// UnOp
// BinOp
//
// Or OpExpr, more in line with old parser

