/**
@fileOverview
Implementation of JavaScript native error classes.

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010 Maxime Chevalier-Boisvert, All Rights Reserved
*/

// TODO: PROBLEM:
// The [[Class]] internal property of the newly constructed object is set to "Error".
//
// JS provides no way to set this internal property
// Need a custom function to do it (Tachyon-specific)

// TODO: PROBLEM:
// name property of constructors, can we change this somehow? property not writable
// one possible fix is to use eval to generate the constructor...
// Alternative is Tachyon-specific functionality to change name manually

/**
Function to create an error constructor function
*/
function makeErrorCtor(errorName, protoParent)
{
    // Get the global this value
    var globalThis = this;

    // Error constructor function
    function ErrorCtor(message)
    {
        if (this === globalThis)
            var newObj = new ErrorCtor(message);
        else
            var newObj = this;    

        if (message !== undefined)
            this.message = message.toString();

        return newObj;
    }

    // Create the prototype object for this error constructor
    ErrorCtor.prototype = Object.create(protoParent, UNDEFINED);

    // Define the prototype property for the error constructor
    Object.defineProperty(
        ErrorCtor,
        'prototype',
        {
            writable        : false,
            enumerable      : false,
            configurable    : false
        }
    );

    // Set the error name in the error prototype object
    ErrorCtor.prototype.name = errorName;

    // The default error message is the empty string
    ErrorCtor.prototype.message = '';

    // Set the prototype constructor to the error constructor
    ErrorCtor.prototype.constructor = ErrorCtor;

    // Return the new error constructor function
    return ErrorCtor;
}

/**
Constructor function for error objects
*/
var Error = makeErrorCtor(
    'Error',
    Object.prototype
);

/**
ToString function of the error prototype object
*/
Error.prototype.toString = function ()
{
    if (this.message === undefined)
        return undefined;

    var name = (this.name === undefined)? 'Error':this.name;

    return name + ': ' + this.message;
};

/*
@class RangeError
@description
15.11.6.2 RangeError
Indicates a numeric value has exceeded the allowable range. 
*/
var RangeError = makeErrorCtor(
    'RangeError',
    Error.prototype
);

/*
@class ReferenceError
@description
15.11.6.3 ReferenceError
Indicate that an invalid reference value has been detected.
*/
var ReferenceError = makeErrorCtor(
    'ReferenceError',
    Error.prototype
);

/**
@class SyntaxError
@description
15.11.6.4 SyntaxError
Indicates that a parsing error has occurred.
*/
var SyntaxError = makeErrorCtor(
    'SyntaxError',
    Error.prototype
);

/**
@class TypeError
@description
15.11.6.5 TypeError
Indicates the actual type of an operand is different than the expected type.
*/
var TypeError = makeErrorCtor(
    'TypeError',
    Error.prototype
);

/**
@class URIError
@description
15.11.6.6 URIError
Indicates that one of the global URI handling functions was used in a way
that is incompatible with its definition.
*/
var URIError = makeErrorCtor(
    'URIError',
    Error.prototype
);

