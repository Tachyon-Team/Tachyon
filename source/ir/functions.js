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
function IRFunction(funcName, argVars, closVars, parentFunc, astNode)
{
    /**
    Function name
    @field
    */
    this.funcName = funcName;

    /**
    Argument variable list
    @field
    */
    this.argVars = argVars;

    /**
    Closure variable name list
    @field
    */
    this.closVars = closVars;

    /**
    AST node corresponding to the function
    */
    this.astNode = astNode;

    /**
    Virgin, unoptimized IR CFG
    @field
    */
    this.virginIR = null;

    /**
    List of child (nested) functions
    @field
    */
    this.childFuncs = [];

    /**
    Parent function, for nested functions
    @field
    */
    this.parentFunc = null;

    /**
    Flag to indicate that this function may use the arguments object
    @field
    */
    this.usesArguments = false;

    /**
    Flag to indicate that this function may use eval
    @field
    */
    this.usesEval = false;
}
IRFunction.prototype = new IRValue();

/**
Produce a string representation of an IR function
*/
IRFunction.prototype.toString = function (blockOrderFn, outFormatFn, inFormatFn)
{
    var output = 'function ' + this.funcName + '(';

    for (var i = 0; i < this.argVars.length; ++i)
    {
        output += this.argVars[i];

        if (i != this.argVars.length - 1)
            output += ', ';
    }

    output += ') ['

    for (var i = 0; i < this.closVars.length; ++i)
    {
        output += this.closVars[i];

        if (i != this.closVars.length - 1)
            output += ', ';
    }

    output += ']\n{\n';

    for (var i = 0; i < this.childFuncs.length; ++i)
    {
        output += indentText(
            this.childFuncs[i].toString(
                blockOrderFn,
                outFormatFn,
                inFormatFn
            ), 
            '    '
        ) + '\n\n';
    }

    output += indentText(
        this.virginIR.toString(
            blockOrderFn,
            outFormatFn,
            inFormatFn
        ),
        '    '
    );

    output += '\n}';

    return output;
};

/**
Return the IR value name for this function
*/
IRFunction.prototype.getValName = function ()
{
    return '<func' + (this.funcName? (' "' + this.funcName + '"'):'') + '>';
}

/**
Create a deep copy of the function
*/
IRFunction.prototype.copy = function ()
{
    var newFunc = new IRFunction(
        this.funcName,
        this.argVars,
        this.closVars,
        this.parentFunc,
        this.astNode
    );

    newFunc.virginIR = this.virginIR.copy();

    this.childFuncs.forEach(
        function (child)
        {
            newFunc.addChildFunc(child.copy());
        }
    );  

    newFunc.usesArguments = this.usesArguments;
    newFunc.usesEval = this.usesEval;

    return newFunc;
}

/**
Get the default number of function arguments
*/
IRFunction.prototype.getNumArgs = function ()
{
    return this.argVars.length;
};

/**
Add a child function
*/
IRFunction.prototype.addChildFunc = function (func)
{
    this.childFuncs.push(func);

    func.parentFunc = this;
};

/** 
Returns a list of all nested functions in posfix order
*/
IRFunction.prototype.getChildrenList = function ()
{
    var list = [];
    this.getChildrenListHelper(list);
    return list;
};

/**
@private
Helper function for getChildrenList
*/
IRFunction.prototype.getChildrenListHelper = function (list)
{
    var i;
    for (i=0; i<this.childFuncs.length; ++i)
    {
        this.childFuncs[i].getChildrenListHelper(list);    
    }
    list.push(this);
};

