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
    Produce a string representation of this instruction
    */
    this.toString = function ()
    {
        var output = (this.outName? (this.outName + " = "):"") + this.mnemonic + " ";

        for (ins in uses)
        {
            if (ins instanceof ConstInstr)
            {
                output += ins.toString();
            }            
            else
            {
                output += ins.outName;
            }

            if (ins != this.uses[this.uses.length - 1])            
                output += ", ";
        }

        return output;
    }

    /**
    Test if this instruction's output is read (has uses)
    */
    this.hasDests = function () { return this.reads.length > 0; }

    /**
    Mnemonic name for this instruction    
    @field
    */
    this.mnemonic = "";

    /**
    Name of this instruction's output
    @field
    */
    this.outName = "";

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

    /**
    Parent basic block
    @field
    */
    this.parentBlock = null;
}

/**
@class Base class for constants, these are treated like regular instructions
@augments BaseInstr
@author Maxime Chevalier-Boisvert
*/
function ConstInstr()
{
    /**
    Default toString() implementation for constant instructions
    */
    this.toString = function() { return String(this.value); }
}
ConstInstr.prototype = new BaseInstr();

/**
@class Boolean constant value
@augments ConstInstr
@author Maxime Chevalier-Boisvert
*/
function BoolConst(value)
{
    // assert (typeof value == 'boolean');

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
    // assert (value - Math.floor(value) = 0);

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
    //assert (typeof value == 'number');

    //this.toString = function() { return String(this.value); }

    this.value = value;
}
FPConst.prototype = new ConstInstr();

/**
@class String constant value
@augments ConstInstr
@author Maxime Chevalier-Boisvert
*/
function StrConst(value)
{
    //assert (typeof value == 'string');

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

/**
@class Base class for branching instructions.
@augments BaseInstr
@author Maxime Chevalier-Boisvert
*/
function BranchInstr()
{
}
BranchInstr.prototype = new BaseInstr();

/**
@class Base class for branching instructions.
@augments BaseInstr
@author Maxime Chevalier-Boisvert
*/
function JumpInstr(targetBlock)
{
    /**
    Obtain a string representation
    */
    this.toString = function() { return "jump " + this.targetBlock.label; }

    /**
    Target basic block
    @field
    */
    this.targetBlock = targetBlock;
}
JumpInstr.prototype = new BranchInstr();

