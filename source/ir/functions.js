/**
@fileOverview
Code to represent and manipulate functions in the intermediate representation.

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010 Maxime Chevalier-Boisvert, All Rights Reserved
*/

/**
@class Intermediate representation function
*/
function IRFunction(funcName, argNames, virginIR)
{
    /**
    Function name
    @field
    */
    this.funcName = funcName;

    /**
    Argument name list
    @field
    */
    this.argNames = argNames;

    /**
    Virgin, unoptimized IR
    @field
    */
    this.virginIR = virginIR;
}
IRFunction.prototype = {};

/**
Produce a string representation of an IR function
*/
IRFunction.prototype.toString = function ()
{
    // TODO: indentText

    var output = 'function ' + this.funcName + '(';

    for (var i = 0; i < this.argNames.length; ++i)
    {
        output += this.argNames[i];

        if (i != this.argNames.length - 1)
            output += ', ';
    }

    output += ')\n{\n';

    output += indentText(this.virginIR.toString(), '    ');

    output += '\n}';

    return output;
};

/**
Get the default number of function arguments
*/
IRFunction.prototype.getNumArgs = function ()
{
    return this.argNames.length;
};

/*
func = new IRFunction('foobar', ['foo', 'bar', 'bif'], 'foo\nbar\nbif');
print(func.getNumArgs());
print(func);
*/

