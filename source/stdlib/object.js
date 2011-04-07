/**
@fileOverview
Implementation of ECMAScript 5 Object methods and prototype.

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010-2011 Tachyon Javascript Engine, All Rights Reserved
*/

/**
@class 15.2.2 The Object Constructor
new Object([value])
Object([value])
*/
function Object(value)
{
    // TODO
}

/**
15.2.3.1 Object prototype object
*/
Object.prototype = {};

/**
15.2.4.1 Object.prototype.constructor
*/
Object.prototype.constructor = Object;

/**
Anonymous function to initialize this library
*/
(function ()
{
    // Get a reference to the context
    var ctx = iir.get_ctx();

    // Set the object prototype object in the context
    set_ctx_objproto(ctx, Object.prototype);

    // Get a reference to the global object
    var globalObj = get_ctx_globalobj(ctx);

    // Set the global object prototype
    set_obj_proto(globalObj, Object.prototype);

    // Set the undefined value on the global object
    globalObj.undefined = UNDEFINED;
})();

//-----------------------------------------------------------------------------

/**
15.2.3.2 Get the prototype of an object
*/
Object.getPrototypeOf = function (obj)
{
    assert (
        boolToBox(boxIsObjExt(obj)),
        'non-object value in getPrototypeOf'
    );

    var proto = get_obj_proto(obj);

    return proto;
};

/**
15.2.3.5 Object.create ( O [, Properties] )
FIXME: for now, we ignore the properties
*/
Object.create = function (obj, props)
{
    if (boolToBox(boxIsObjExt(obj)) === false && obj !== null)
    {
        throw makeError(
            TypeError, 
            'can only create object from object or null prototype'
        );
    }

    var newObj = newObject(obj);

    return newObj;
};

/**
15.2.3.6 Object.defineProperty ( O, P, Attributes )
FIXME: for now, we ignore most attributes
*/
Object.defineProperty = function (obj, prop, attribs)
{
    assert (
        boolToBox(boxIsObjExt(obj)),
        'non-object value in defineProperty'
    );

    if (attribs.hasOwnProperty('value'))
        obj[prop] = attribs.value;
};

/**
15.2.3.12 Object.isFrozen ( O )
FIXME: for now, all objects are extensible
*/
Object.isFrozen = function (obj)
{
    return false;
};

/**
15.2.3.13 Object.isExtensible ( O )
FIXME: for now, all objects are extensible
*/
Object.isExtensible = function (obj)
{
    return true;
};

/**
15.2.4.2 Default object to string conversion function
*/
Object.prototype.toString = function ()
{
    return "object";
};

/**
15.2.4.5 Test that an object has a given property
*/
Object.prototype.hasOwnProperty = function (prop)
{
    return boolToBox(hasOwnPropVal(this, prop));
};

/**
15.2.4.6 Test that an object is the prototype of another
*/
Object.prototype.isPrototypeOf = function (obj)
{
    var proto = Object.getPrototypeOf(obj);

    return (this === proto);
};

