/**
@fileOverview
Implementation of ECMAScript 5 Function methods and prototype.

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010-2011 Tachyon Javascript Engine, All Rights Reserved
*/

/**
15.3.2 The function constructor
*/
function Function()
{
}

/**
15.3.3.1 Function prototype object
*/
Function.prototype = {};

/**
15.3.4.2 Function.prototype.toString ()
*/
Function.prototype.toString = function ()
{
    // TODO: return the function source code
    return "function";
};

/**
15.3.4.3 Function.prototype.apply (thisArg, argArray)
*/
Function.prototype.apply = function (thisArg, argArray)
{
    // TODO
};

/**
15.3.4.4 Function.prototype.call (thisArg [, arg1 [, arg2, … ]])
*/
Function.prototype.call = function (thisArg)
{
    // TODO
};

