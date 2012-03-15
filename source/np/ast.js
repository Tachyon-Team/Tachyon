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
    this.srcPos = undefined;

    this.children = undefined;

    // TODO: standard traversal, children node array?
}

/**
Add a child to this node
*/
ASTNode.prototype.addChild = function (node)
{
    if (this.children === undefined)
        this.children = [];

    this.childre.push(node);
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

function ASTStmt()
{
}
ASTStmt.prototype = new ASTNode();

function ASTExpr()
{
}
ASTExpr.prototype = new ASTNode();

/**
@class Block statement
@extends ASTStmt
*/
function ASTBlock()
{
}
ASTBlock.prototype = new ASTStmt();

/**
@class Constant expression
@extends ASTConst
*/
function ASTConst(value)
{
    this.value = value;
}
ASTConst.prototype = new ASTExpr();

function ASTBinOp()
{
}
ASTBinOp.prototype = new ASTExpr();

// TODO: kinds of statements



// TODO: kinds of expressions
// Assign
// UnOp
// BinOp
//
// Or OpExpr, more in line with old parser?










