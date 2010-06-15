/**
@fileOverview
Code to represent and manipulate functions in the intermediate representation.

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010 Maxime Chevalier-Boisvert, All Rights Reserved
*/

/**
@class Intermediate representation function
*/
function IRFunction(funcName, argNames, virginIR)
{
    /**
    Produce a string representation of an IR function
    */
    this.toString = function ()
    {
        // TODO
    }

    /**
    Function name
    @field
    */
    this.funcName = funcName;

    /**
    Argument names
    @field
    */
    this.argNames = argNames;

    /**
    Virgin, unoptimized IR
    @field
    */
    this.virginIR = virginIR;
}

