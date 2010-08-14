/**
@fileOverview
Utility code to facilitate debugging.

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010 Maxime Chevalier-Boisvert, All Rights Reserved
*/

/** Throw an exception with message and args */
error = function (message)
{
    var err = message;
    for (var i=1; i<arguments.length; ++i)
    {
        err += arguments[i];
    }
    throw err;
};

/** Ensure a boolean condition is met, otherwise throw an exception */
assert = function (bool, message)
{
    if (!bool) 
    { 
        error.apply(null, Array.prototype.slice.call(arguments, 1));
    } 
};

