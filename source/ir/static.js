/**
@fileOverview
Implementation of static bindings required for compilation

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010 Maxime Chevalier-Boisvert, All Rights Reserved
*/

/**
Environment to store static bindings required for compilation
*/
function StaticEnv()
{
    /**
    Private object to store bindings
    @private
    */
    var bindings = {};

    /**
    Register a new binding
    */
    this.regBinding = function (name, val)
    {
        assert (
            bindings[name] === undefined,
            'static binding already exists: "' + name + '"'
        );

        assert (
            val instanceof IRValue,
            'invalid static binding value for: "' + name + '"'
        );

        bindings[name] = val;
    };

    /**
    Test if a binding exists
    */
    this.hasBinding = function (name)
    {
        return (bindings[name] !== undefined);
    };

    /**
    Get the value of a binding
    */
    this.getBinding = function (name)
    {
        var val = bindings[name];

        assert (
            val !== undefined,
            'static binding not found: "' + name + '"'
        );

        return val;
    };
}
StaticEnv.prototype = {};

/**
Parse static bindings in a code unit
*/
StaticEnv.prototype.parseUnit = function (astUnit)
{
    // Create a function object for the unit
    var unitFunc = getIRFuncObj(
        '',
        null,
        astUnit
    );

    // For each function in the unit
    for (var i = 0; i < astUnit.funcs.length; ++i)
    {
        var func = astUnit.funcs[i];

        // If this is a function declaration
        if (func instanceof FunctionDeclaration)
        {
            // Parse the function
            this.parseFunc(
                func.id.toString(),
                unitFunc,
                func.funct
            );
        }
    }
};

/**
Parse static bindings for function declarations
*/
StaticEnv.prototype.parseFunc = function (funcName, parentFunc, funcExpr)
{
    // Get the function object for the function expression
    var func = getIRFuncObj(
        funcName,
        parentFunc,
        funcExpr
    );

    // If a function has non-boxed inputs/output, it should be statically linked
    var nonBoxed = false;
    for (var i = 0; i < func.argTypes.length; ++i)
        if (func.argTypes[i] !== IRType.box)
            nonBoxed = true;
    if (func.retType !== IRType.box)
        nonBoxed = true;
    if (nonBoxed)
        func.staticLink = true;

    // If the function should be statically linked
    if (func.staticLink)
    {
        // Register a static binding for the function
        this.regBinding(funcName, func);
    }
};

