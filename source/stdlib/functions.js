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
    if (argArray === null || argArray === UNDEFINED)
        argArray = [];

    if (boxIsArray(argArray) === FALSE_BOOL)
        throw makeError(TypeError, 'argument array must be an array');

    var funcPtr = get_clos_funcptr(this);

    var argTable = get_arr_arr(argArray);

    var numArgs = iir.icast(IRType.pint, get_arr_len(argArray));

    iir.call_apply(funcPtr, this, thisArg, argTable, numArgs);
};

/**
15.3.4.4 Function.prototype.call (thisArg [, arg1 [, arg2, … ]])
*/
Function.prototype.call = function (thisArg)
{
    var argArray = [];
    for (var i = 1; i < arguments.length; ++i)
        argArray.push(arguments[i]);

    this.apply(thisArg, argArray);
};

