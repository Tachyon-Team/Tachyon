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
    // TODO
}

/**
15.3.3.1 Function prototype object
*/
Function.prototype = {};

/**
Anonymous function to initialize this library
*/
(function ()
{
    // Get a reference to the context
    var ctx = iir.get_ctx();

    // Set the function prototype object in the context
    set_ctx_funcproto(ctx, Function.prototype);
})();

//-----------------------------------------------------------------------------

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
    if (boxIsFunc(this) === FALSE_BOOL)
        throw makeError(TypeError, 'apply on non-function');

    if (argArray === null || argArray === UNDEFINED)
        argArray = [];

    if (boxIsArray(argArray) === FALSE_BOOL)
        throw makeError(TypeError, 'invalid arguments array');

    // Get the function pointer for the function
    var funcPtr = get_clos_funcptr(this);

    // Get the arguments table from the array
    var argTable = unboxRef(get_arr_arr(argArray));

    // Get the number of arguments
    var numArgs = iir.icast(IRType.pint, get_arr_len(argArray));

    // Perform the call using the apply instruction
    var retVal = iir.call_apply(funcPtr, this, thisArg, argTable, numArgs);

    return retVal;
};

/**
15.3.4.4 Function.prototype.call (thisArg [, arg1 [, arg2, … ]])
*/
Function.prototype.call = function (thisArg)
{
    var argArray = [];
    for (var i = 1; i < arguments.length; ++i)
        argArray.push(arguments[i]);

    var retVal = this.apply(thisArg, argArray);

    return retVal;
};

