/**
@fileOverview
Implementation of ECMAScript 5 array string routines.

@author
TBD

@copyright
Copyright (c) 2010 Tachyon Javascript Engine, All Rights Reserved
*/

/*
TODO: establish functionality needed for initial bootstrap

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
String constructor
*/
/*
function String(value)
{

}*/

/**
String prototype object
*/
//String.prototype = {};

// Disable functions not needed for the bootstrap so that D8 will warn us
// if we start using them
String.prototype.concat = undefined;
String.prototype.indexOf = undefined;
String.prototype.lastIndexOf = undefined;
String.prototype.localeCompare = undefined;
String.prototype.match = undefined;
String.prototype.replace = undefined;
String.prototype.search = undefined;
String.prototype.toLowerCase = undefined;
String.prototype.toLocaleLowerCase = undefined;
String.prototype.toLocaleUpperCase = undefined;
String.prototype.trim = undefined;

