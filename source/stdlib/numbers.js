/**
@fileOverview
Implementation of ECMAScript 5 Number methods and prototype.

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010-2011 Tachyon Javascript Engine, All Rights Reserved
*/

/**
15.7.1 The Number function/constructor
new Number([ value ])
Number([ value ])
*/
function Number(value)
{
    // If this is a constructor call (new Number)
    if (isGlobalObj(this) === false)
    {
        // TODO: convert the value to a number

        // Store the value in the new object
        // TODO: this should be a hidden/internal property
        this.value = value;
    }
    else
    {
        // Convert the value to a number
        return boxToNumber(value);
    }
}

/**
15.7.3.1 Number prototype object
*/
Number.prototype = {};

/**
Anonymous function to initialize this library
*/
(function ()
{
    // Get a reference to the context
    var ctx = iir.get_ctx();

    // Set the number prototype object in the context
    set_ctx_numproto(ctx, Number.prototype);
})();

//-----------------------------------------------------------------------------

// TODO
// 15.7.3.2 Number.MAX_VALUE
// 15.7.3.3 Number.MIN_VALUE
// 15.7.3.4 Number.NaN
// 15.7.3.5 Number.NEGATIVE_INFINITY
// 15.7.3.6 Number.POSITIVE_INFINITY

/**
Internal function to get the number value of a number or number object
*/
function getNumVal(num)
{
    if (boxIsInt(num))
    {
        return num;
    }
    else if (boxIsObj(num))
    {
        return num.value;
    }
}

/**
15.7.4.2 Number.prototype.toString ([ radix ])
*/
Number.prototype.toString = function (radix)
{
    var num = getNumVal(this);

    //FIXME: for now, ignoring the radix

    return boxToString(num);
};

