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
// Need a custom function to do it

/**
Function to create a native error constructor
*/
function makeNativeError()
{
    var globalThis = this;

    function NativeError(message)
    {
        if (this === globalThis)
            var newObj = new NativeError(message);
        else
            var newObj = this;    

        if (message === undefined)
            message = '';

        this.message = message;

        return newObj;
    }

    return NativeError;
}

/*
15.11.6.2 RangeError
Indicates a numeric value has exceeded the allowable range. 
See 15.4.2.2, 15.4.5.1, 15.7.4.2, 15.7.4.5, 15.7.4.6, and 15.7.4.7, 15.9.5.43.

15.11.6.3 ReferenceError
Indicate that an invalid reference value has been detected.
See 8.7.1, 8.7.2, 10.2.1, 10.2.1.1.4, 10.2.1.2.4, and 11.13.1.

15.11.6.4 SyntaxError
Indicates that a parsing error has occurred. See 11.1.5, 11.3.1, 11.3.2, 11.4.1,
11.4.4, 11.4.5, 11.13.1, 11.13.2, 12.2.1, 12.10.1, 12.14.1, 13.1, 15.1.2.1,
15.3.2.1, 15.10.2.2, 15.10.2.5, 15.10.2.9, 15.10.2.15, 15.10.2.19, 15.10.4.1,
and 15.12.2.

15.11.6.5 TypeError
Indicates the actual type of an operand is different than the expected type.
See 8.6.2, 8.7.2, 8.10.5, 8.12.5, 8.12.7, 8.12.8, 8.12.9, 9.9, 9.10, 10.2.1,
10.2.1.1.3, 10.6, 11.2.2, 11.2.3, 11.4.1, 11.8.6, 11.8.7, 11.3.1, 13.2, 13.2.3, 
15, 15.2.3.2, 15.2.3.3, 15.2.3.4, 15.2.3.5, 15.2.3.6, 15.2.3.7, 15.2.3.8,
15.2.3.9, 15.2.3.10, 15.2.3.11, 15.2.3.12, 15.2.3.13, 15.2.3.14, 15.2.4.3, 
15.3.4.2, 15.3.4.3, 15.3.4.4, 15.3.4.5, 15.3.4.5.2, 15.3.4.5.3, 15.3.5,
15.3.5.3, 15.3.5.4, 15.4.4.3, 15.4.4.11, 15.4.4.16, 15.4.4.17, 15.4.4.18,
15.4.4.19, 15.4.4.20, 15.4.4.21, 15.4.4.22, 15.4.5.1, 15.5.4.2, 15.5.4.3,
15.6.4.2, 15.6.4.3, 15.7.4, 15.7.4.2, 15.7.4.4, 15.7.4.8, 15.9.5, 15.9.5.44,
15.10.4.1, 15.10.6, 15.11.4.4 and 15.12.3.

15.11.6.6 URIError
Indicates that one of the global URI handling functions was used in a way that
is incompatible with its definition. See 15.1.3.
*/

/*
The initial value of NativeError.prototype is a NativeError prototype
object (15.11.7.7). Each NativeError constructor has a separate prototype object.
This property has the attributes 
{ [[Writable]]: false, [[Enumerable]]: false, [[Configurable]]: false }.
*/

/*
15.11.7.8 NativeError.prototype.constructor
The initial value of the constructor property of the prototype for a given NativeError constructor is the NativeError constructor function itself (15.11.7).
15.11.7.9 NativeError.prototype.name
The initial value of the name property of the prototype for a given NativeError constructor is the name of the constructor (the name used instead of NativeError).
15.11.7.10 NativeError.prototype.message
The initial value of the message property of the prototype for a given NativeError constructor is the empty String.

NOTE The prototypes for the NativeError constructors do not themselves provide a
toString function, but instances of errors will inherit it from the Error prototype object.
*/




