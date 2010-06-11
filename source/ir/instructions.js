/**
@fileOverview
Class hierarchy for Intermediate Representation (IR) instructions

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010 Maxime Chevalier-Boisvert, All Rights Reserved
*/

/**
@class Base class for all instructions
@author Maxime Chevalier-Boisvert
*/
function BaseInstr()
{
    /**
    Produce a string representation.
    */
    this.toString = function () {}

    /**
    Test if this instruction's output is read (has uses)
    */
    this.hasDests = function() { return this.reads.length > 0; }

    /**
    List of values used by this instruction
    @field
    */
    this.uses = [];

    /**
    List of instructions reading this instruction's output
    @field
    */
    this.dests = [];
}

/**
@class Base class for constants, these are treated like regular instructions.
@augments BaseInstr
@author Maxime Chevalier-Boisvert
*/
function ConstInstr()
{
}
ConstInstr.prototype = new BaseInstr();

/**
@class Boolean constant value
@augments ConstInstr
@author Maxime Chevalier-Boisvert
*/
function BoolConst(value)
{
    assert (typeof value == 'boolean');
    this.value = value;
}
BoolConst.prototype = new ConstInstr();

/**
@class Integer constant value
@augments ConstInstr
@author Maxime Chevalier-Boisvert
*/
function IntConst(value)
{
    assert (value - math.floor(value) = 0);
    this.value = value;
}
IntConst.prototype = new ConstInstr();

/**
@class Floating-point constant value
@augments ConstInstr
@author Maxime Chevalier-Boisvert
*/
function FPConst(value)
{
    assert (typeof value == 'number');
    this.value = value;
}
IntConst.prototype = new ConstInstr();

/**
@class String constant value
@augments ConstInstr
@author Maxime Chevalier-Boisvert
*/
function StrConst(value)
{
    assert (typeof value == 'string');
    this.value = value;
}
IntConst.prototype = new ConstInstr();

/**
@class Class for arithmetic instructions
@augments BaseInstr
@author Maxime Chevalier-Boisvert
*/
function ArithInstr()
{
}
ArithInstr.prototype = new BaseInstr();

