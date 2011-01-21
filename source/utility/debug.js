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
    var errMsg = '';
    for (var i = 0; i < arguments.length; ++i)
    {
        errMsg += arguments[i];
    }

    var errObj = new Error(errMsg);

    // If we are running within V8, capture a stack trace
    if (Error.captureStackTrace !== undefined)
        Error.captureStackTrace(errObj, error);

    throw errObj;
};

/**
Retrow an error with added information
*/
function rethrowError(exc, message)
{
    if (message === undefined)
        throw error;

    var errMsg = '';
    for (var i=1; i < arguments.length; ++i)
    {
        errMsg += arguments[i];
    }

    errMsg += ':\n' + exc;

    error(errMsg);
}

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

/** 
Ensure the new operator has been called for this particular constructor
*/
function assertNew(obj)
{
    assert(!isGlobal(obj), 
           "Constructor has been called without the new operator");
};

/** 
Ensure a received exception is equal to an expected exception, 
otherwise rethrow the exception.
*/
function assertExceptionEqual(expected, received)
{
    var exMsg = expected.message? expected.message:expected.toString();
    var reMsg = received.message? received.message:received.toString();

    if (exMsg !== reMsg)
    {
        throw received;
    }
}

