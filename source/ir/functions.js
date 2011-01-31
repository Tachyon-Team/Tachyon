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
function IRFunction(
    funcName, 
    argVars, 
    closVars, 
    argTypes, 
    retType, 
    parentFunc, 
    astNode
)
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
    Argument types
    @field
    */
    this.argTypes = argTypes;

    /**
    Return type
    @field
    */
    this.retType = retType;

    /**
    AST node corresponding to the function
    */
    this.astNode = astNode;

    /**
    Virgin, unoptimized CFG
    @field
    */
    this.virginCFG = null;

    /**
    Final, optimized CFG
    */
    this.finalCFG = null;

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
    Indicates that this function is a proxy callable from C
    (uses the C calling convention)
    @field
    */
    this.cProxy = false;

    /**
    Indicates that this function may use the arguments object
    @field
    */
    this.usesArguments = false;

    /**
    Indicates that this function may use eval
    @field
    */
    this.usesEval = false;

    /**
    Indicating that this function should be statically linked
    @field
    */
    this.staticLink = false;

    /**
    Indicates that this function should be inlined
    @field
    */
    this.inline = false;

    /**
    Indicates that this function cannot throw exceptions
    @field
    */
    this.noThrow = false;

    /**
    Indicates that this function cannot do global variable accesses directly
    @field
    */
    this.noGlobal = false;

    /**
    Indicates that the function reads from memory
    @field
    */
    this.readsMem = true;

    /**
    Indicates that the function writes to memory
    @field
    */
    this.writesMem = true;

    /**
    Everything related to linkage information. Will not be copied during
    a deep copy.
    @field
    */
    this.linking = {};

    /**
    Everything related to runtime information. Will not be copied during
    a deep copy.
    @field
    */
    this.runtime = {};

    // If the argument or return types are undefined, make them boxed
    if (this.argTypes === undefined)
    {
        this.argTypes = [];
        for (var i = 0; i < argVars.length; ++i)
            this.argTypes.push(IRType.box);
    }
    if (this.retType === undefined)
    {
        this.retType = IRType.box;
    }
}
IRFunction.prototype = new IRValue();

/**
In the IR, function objects represent function pointers
*/
IRFunction.prototype.type = IRType.rptr;

/**
Produce a string representation of an IR function
*/
IRFunction.prototype.toString = function (blockOrderFn, outFormatFn, inFormatFn)
{
    var output = this.retType + ' function ' + this.funcName + '(';

    for (var i = 0; i < this.argVars.length; ++i)
    {
        output += this.argTypes[i] + ' ' + this.argVars[i];

        if (i !== this.argVars.length - 1)
            output += ', ';
    }

    output += ') [';

    for (var i = 0; i < this.closVars.length; ++i)
    {
        output += this.closVars[i];

        if (i !== this.closVars.length - 1)
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

    var cfg = (this.finalCFG !== null) ? this.finalCFG : this.virginCFG;

    output += indentText(
        cfg.toString(
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
    return '<fn' + (this.funcName? (' "' + this.funcName + '"'):'') + '>';
};

/**
Create a deep copy of the function
*/
IRFunction.prototype.copy = function ()
{
    var newFunc = new IRFunction(
        this.funcName,
        this.argVars,
        this.closVars,
        this.argTypes.slice(0),
        this.retType,
        this.parentFunc,
        this.astNode
    );

    newFunc.virginCFG = this.virginCFG.copy();

    this.childFuncs.forEach(
        function (child)
        {
            newFunc.addChildFunc(child.copy());
        }
    );

    newFunc.usesArguments = this.usesArguments;
    newFunc.usesEval = this.usesEval;
    newFunc.staticLink = this.staticLink;
    newFunc.inline = this.inline;
    newFunc.noThrow = this.noThrow;
    newFunc.noGlobal = this.noGlobal;
    newFunc.readsMem = this.readsMem;
    newFunc.writesMem = this.writesMem;

    return newFunc;
};

/**
Validate the function and its children
*/
IRFunction.prototype.validate = function ()
{
    // Validate the original control-flow graph
    this.virginCFG.validate();

    // Validate the final control-flow graph
    if (this.finalCFG !== null)
        this.finalCFG.validate();

    // Validate the child functions
    this.childFuncs.forEach(
        function (child)
        {
            child.validate();
        }
    );

    // The function is valid
    return true;
};

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

    function getChildren(func)
    {
        func.childFuncs.forEach(
            function (child)
            {
                getChildren(child);
            }
        );
        list.push(func);
    }

    getChildren(this);

    return list;
};

/**
Get a child function by name
*/
IRFunction.prototype.getChild = function (name)
{
    for (var i = 0; i < this.childFuncs.length; ++i)
    {
        if (this.childFuncs[i].funcName === name)
            return this.childFuncs[i];
    }

    assert (
        false,
        'child function not found: "' + name + '"'
    );
};

