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
    @description Produces a string representation.
    */
    this.toString = function () {}
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

