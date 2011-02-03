/**
@fileOverview
Implementation of ECMAScript 5 string string routines.

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010-2011 Tachyon Javascript Engine, All Rights Reserved
*/

/*
TODO: implement functionality needed for initial bootstrap

String.fromCharCode([char0 [, char1 [, … ]]])
String.prototype.charAt(pos)
String.prototype.charCodeAt(pos)
String.prototype.slice(start, end)
String.prototype.split(separator, limit)
String.prototype.substring(start, end)
String.prototype.toString()
String.prototype.toUpperCase()
...
*/

/**
15.5.2 String constructor
new String (value)
String (value)
*/
function String(value)
{
}

/**
15.5.3.1 String prototype object
*/
String.prototype = {};

/**
Test function, should be callable on string literals.
*/
String.prototype.foo = function ()
{
    return 1337;
};

