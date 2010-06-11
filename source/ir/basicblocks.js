/**
@fileOverview
Implementation of basic blocks

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010 Maxime Chevalier-Boisvert, All Rights Reserved
*/

/**
@class Class to represent a basic block
@author Maxime Chevalier-Boisvert
*/
BasicBlock = function ()
{
    /**
    Produce a string representation
    */
    this.toString = function() {}

    /**
    Add an instruction at the end of the block
    */
    this.addInstr = function(instr) { this.instr.push(instr); }

    /**
    List of IR instructions
    @field
    */
    this.instrs = [];
}

