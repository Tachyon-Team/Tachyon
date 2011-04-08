/* _________________________________________________________________________
 *
 *             Tachyon : A Self-Hosted JavaScript Virtual Machine
 *
 *
 *  This file is part of the Tachyon JavaScript project. Tachyon is
 *  distributed at:
 *  http://github.com/Tachyon-Team/Tachyon
 *
 *
 *  Copyright (c) 2011, Universite de Montreal
 *  All rights reserved.
 *
 *  This software is licensed under the following license (Modified BSD
 *  License):
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions are
 *  met:
 *    * Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *    * Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions and the following disclaimer in the
 *      documentation and/or other materials provided with the distribution.
 *    * Neither the name of the Universite de Montreal nor the names of its
 *      contributors may be used to endorse or promote products derived
 *      from this software without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 *  IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 *  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 *  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL UNIVERSITE DE
 *  MONTREAL BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 *  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 *  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 *  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 *  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 *  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 *  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * _________________________________________________________________________
 */

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
Get the value of a constant binding
*/
StaticEnv.prototype.getValue = function (name)
{
    var val = this.getBinding(name);

    assert (
        val instanceof ConstValue, 
        "val must be an instanceof ConstValue"
    );

    return val.value;
}

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

