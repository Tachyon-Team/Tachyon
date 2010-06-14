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

        for (i = 0; i < this.uses.length; ++i)
        {
            ins = this.uses[i];

            output += ins.valToString();

            if (ins != this.uses[this.uses.length - 1])            
                output += ", ";
        }

        return output;
    }

    /**
    Get a string representation of an instruction's value/name
    */
    this.valToString = function ()
    {
        // Return the output/temporary name for this instruction
        return this.outName;
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
    Values used/read by this instruction
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

    /**
    Get a string representation of an instruction's value/name.
    Returns the constant's string representation directly.
    */
    this.valToString = toString;
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
Arithmetic operator enumeration
*/
ArithOp =
{
    ADD: 0,
    SUB: 1,
    MUL: 2,
    DIV: 3,
    MOD: 4
};

/**
@class Class for arithmetic instructions
@augments BaseInstr
@author Maxime Chevalier-Boisvert
*/
function ArithInstr(arithOp, leftVal, rightVal)
{
    // Set the mnemonic name for the instruction
    switch (arithOp)
    {
        case ArithOp.ADD: this.mnemonic = "add"; break;
        case ArithOp.SUB: this.mnemonic = "sub"; break;
        case ArithOp.MUL: this.mnemonic = "mod"; break;
        case ArithOp.DIV: this.mnemonic = "div"; break;
        case ArithOp.MOD: this.mnemonic = "mod"; break;
    }

    /**
    Arithmetic operator
    @field
    */
    this.arithOp = arithOp;

    /**
    Arithmetic operands
    @field
    */
    this.uses = [leftVal, rightVal];
}
ArithInstr.prototype = new BaseInstr();

/**
@class Base class for branching instructions.
@augments BaseInstr
@author Maxime Chevalier-Boisvert
*/
function BranchInstr()
{
    /**
    Potential branch target basic blocks
    @field
    */
    this.targets = [];
}
BranchInstr.prototype = new BaseInstr();

/**
@class Unconditional jump instruction
@augments BranchInstr
@author Maxime Chevalier-Boisvert
*/
function JumpInstr(targetBlock)
{
    /**
    Obtain a string representation
    */
    this.toString = function() { return "jump " + this.targets[0].label; }

    /**
    Target basic block
    @field
    */
    this.targets = [targetBlock];
}
JumpInstr.prototype = new BranchInstr();

/**
@class If conditional test instruction
@augments BranchInstr
@author Maxime Chevalier-Boisvert
*/
function IfInstr(testVal, trueBlock, falseBlock)
{
    /**
    Obtain a string representation
    */
    this.toString = function()
    {
        return 
            "if " + this.uses[0].valToString() +
            " then " + this.targets[0].label +
            " else " + this.targets[1].label
        ; 
    }

    /**
    Test value for the branch condition
    @field
    */
    this.uses = [testVal];

    /**
    Branch targets for the true and false cases
    @field
    */
    this.targets = [trueTarget, falseTarget];
}
JumpInstr.prototype = new BranchInstr();

