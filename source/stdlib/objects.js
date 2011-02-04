/**
@fileOverview
Implementation of ECMAScript 5 object methods and prototype.

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
}

/**
15.2.3.5 Object.create ( O [, Properties] )
FIXME: for now, we ignore the properties
*/
Object.create = function (obj, props)
{
    var newObj = newObject(obj);

    return newObj;
}

/**
15.2.3.1 Object prototype object
*/
Object.prototype = {};

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
    return (prop in this);
}

/**
15.2.4.6 Test that an object is the prototype of another
*/
Object.prototype.isPrototypeOf = function (obj)
{
    var proto = Object.getPrototypeOf(obj);

    return (this === proto);
}

