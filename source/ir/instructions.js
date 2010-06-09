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

    // List of instructions reading this instruction's output
    this.dests = [];
}

/**
@class Class for arithmetic instructions
@augments BaseInstr
@author Maxime Chevalier-Boisvert
*/
function ArithInstr()
{
}
ArithInstr.prototype = new BaseInstr();

