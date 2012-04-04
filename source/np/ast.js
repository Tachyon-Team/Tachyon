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
Get the number of children of this node
*/
ASTNode.prototype.numChildren = function ()
{
    return (this.children !== undefined)? this.children.length:0;
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
    if (this.children === undefined)
        return { valid: function() { return false; } };

    return new ArrayIterator(this.children);
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

ASTProgram.prototype.toString = function ()
{
    var str = '';

    for (var itr = this.getChildItr(); itr.valid(); itr.next())
        str += itr.get() + '\n';

    return str;
}

/**
@class Base class for statements
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

ASTVar.prototype.toString = function ()
{
    var str = 'var ';

    for (var itr = this.getChildItr(); itr.valid(); itr.next())
    {
        if (str !== 'var ')
            str += ', ';

        str += String(itr.get());
    }

    return str + ';';
}

/**
@class Empty statement
@extends ASTStmt
*/
function ASTEmpty()
{
}
ASTEmpty.prototype = new ASTStmt();

ASTEmpty.prototype.toString = function ()
{
    return ';';
}

/**
@class Block statement
@extends ASTStmt
*/
function ASTBlock()
{
}
ASTBlock.prototype = new ASTStmt();

ASTBlock.prototype.toString = function ()
{
    var str = '{\n';

    for (var itr = this.getChildItr(); itr.valid(); itr.next())
        str += itr.get() + '\n';

    return str + '}';
}

/**
@class Debugger breakpoint statement
@extends ASTStmt
*/
function ASTDebugger()
{
}
ASTDebugger.prototype = new ASTStmt();

ASTDebugger.prototype.toString = function ()
{
    return 'debugger;';
}

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

ASTExpr.prototype.priority = function ()
{
    return 100;
}

/**
@class Constant expression
@extends ASTConst
*/
function ASTConst(value)
{
    this.value = value;
}
ASTConst.prototype = new ASTExpr();

ASTConst.prototype.toString = function ()
{
    return String(this.value);
}

/**
@class Identifier expression
@extends ASTExpr
*/
function ASTIdent(name)
{
    this.name = name;
}
ASTIdent.prototype = new ASTExpr();

ASTIdent.prototype.toString = function ()
{
    return this.name;
}

/**
@class Assignment expression
@extends ASTExpr
*/
function ASTAssign(op)
{
    assert (
        op in ASTAssign.ops,
        'invalid assignment operator'
    );

    this.op = op;
}
ASTAssign.prototype = new ASTExpr();

ASTAssign.ops = {
    '='         : 2,
    '+='        : 2,
    '-='        : 2,
    '*='        : 2,
    '/='        : 2,
    '%='        : 2,
    '<<='       : 2,
    '>>='       : 2,
    '>>>='      : 2,
    '&='        : 2,
    '^='        : 2,
    '|='        : 2,
};

ASTAssign.prototype.toString = function ()
{
    return this.children[0] + ' ' + this.op + ' ' + this.children[1];
}

ASTAssign.prototype.priority = function ()
{
    return ASTAssign.ops[this.op];
}

/**
@class Unary operator expressions
@extends ASTExpr
*/
function ASTUnOp(op)
{
    this.op = op;
}
ASTUnOp.prototype = new ASTExpr();

/**
Unary operators and associated priority rankings
*/
ASTUnOp.ops = {
    'new'       : 17,

    '++'        : 15,
    '--'        : 15,

    '!'         : 14,
    '~'         : 14,
    '+'         : 14,
    '-'         : 14,
    'typeof'    : 14,
    'delete'    : 14,
};

ASTUnOp.prototype.toString = function ()
{
    return this.op + this.children[0];
}

ASTUnOp.prototype.priority = function ()
{
    return ASTUnOp.ops[this.op];
}

/**
@class Binary operator expressions
@extends ASTExpr
*/
function ASTBinOp(op)
{
    this.op = op;
}
ASTBinOp.prototype = new ASTExpr();

/**
Binary operators and associated priority rankings
*/
ASTBinOp.ops = {
    '.'         : 17,

    '*'         : 13,
    '/'         : 13,
    '%'         : 13,

    '+'         : 12,
    '-'         : 12,

    '<<'        : 11,
    '>>'        : 11,
    '>>>'       : 11,

    '<'         : 10,
    '<='        : 10,
    '>'         : 10,
    '>='        : 10,
    'in'        : 10,
    'instanceof': 10,

    '=='        : 9,
    '!='        : 9,
    '==='       : 9,
    '!=='       : 9,

    '&'         : 8,
    '^'         : 7,
    '|'         : 6,

    '&&'        : 5,
    '||'        : 4,
};

ASTBinOp.prototype.toString = function ()
{
    return '(' + this.children[0] + ' ' + this.op + ' ' + this.children[1] + ')';
}

ASTBinOp.prototype.priority = function ()
{
    return ASTBinOp.ops[this.op];
}

