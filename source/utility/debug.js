/**
@fileOverview
Utility code to facilitate debugging.

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010 Maxime Chevalier-Boisvert, All Rights Reserved
*/

/** Throw an exception with message and args */
function error(message)
{
    var err = message;
    for (var i=1; i<arguments.length; ++i)
    {
        err += arguments[i];
    }
    throw err;
};

/** Ensure a boolean condition is met, otherwise throw an exception */
function assert(bool, message)
{
    if (!bool) 
    { 
        error.apply(null, Array.prototype.slice.call(arguments, 1));
    } 
};

/** 
    Tells if an object corresponds to the global object for the current
    execution context.
*/
function isGlobal(obj)
{
    // Retrieve the current global object 
    var global = (function () { return this; })();

    return obj === global;
}

/** Ensure the new operator has been call for this particular constructor */
function assertNew(obj)
{
    assert(!isGlobal(obj), 
           "Constructor has been called without the new operator");
};

/** 
    Ensure a received exception is equal to an expected exception , 
    otherwise rethrow the exception. If the expected exception is an object,
    it must implements an isEqual method with an exception object parameter.
*/
function assertExceptionEqual(expected, received)
{
    if (typeof expected !== "object")
    {
        if (received !== expected)
        {
            throw received;
        }
    } else
    {
        if (!expected.isEqual(received))
        {
           throw received; 
        }
    }
}

