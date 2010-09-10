/**
@fileOverview
Implementation of runtime context objects

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010 Maxime Chevalier-Boisvert, All Rights Reserved
*/

/**
Represents a context variable
*/
function ContextVar(name, type, offset)
{
    /**
    Variable name
    @field
    */
    this.name = name;

    /**
    Variable type
    @field
    */
    this.type = type;

    /**
    Memory offset
    @field
    */
    this.offset = offset;
}

/**
Represents the memory layout of context objects
*/
function ContextLayout()
{
    /**
    Current size of context objects
    */
    var curSize = 0;

    /**
    Map of variable names to variable objects
    */
    var varMap = {};

    /**
    Indicates if new variables cannot be registered
    */
    var locked = false;

    /**
    Get the current size of context objects
    */
    this.getSize = function ()
    {
        return curSize;
    }

    /**
    Register a context variable
    */
    this.regVar = function(name, type)
    {
        assert (
            varMap[name] === undefined,
            'context variable already registered: "' + name + '"'
        );

        assert (
            !locked,
            'context layout is locked'
        );

        // TODO: alignment of context variables?
        var offset = curSize;
        curSize += type.size;

        varMap[name] = new ContextVar(name, type, offset);
    }

    /**
    Get a context variable object
    */
    this.getVar = function (name)
    {
        assert (
            varMap[name] !== undefined,
            'context variable not found: "' + name + '"'
        );

        return varMap[name];
    }

    /**
    Lock the context layout so that its size can no longer change
    */
    this.lock = function ()
    {
        locked = true;
    }
}

/**
Global context layout object
*/
var contextLayout = new ContextLayout();

// Global object
contextLayout.regVar(
    'GLOBAL_OBJECT',
    IRType.box
);

// Object prototype object
contextLayout.regVar(
    'OBJECT_PROTOTYPE',
    IRType.box
);

// Function prototype object
contextLayout.regVar(
    'FUNCTION_PROTOTYPE',
    IRType.box
);

// Type error constructor
contextLayout.regVar(
    'TYPE_ERROR_CTOR',
    IRType.box
);

// Lock the context layout
contextLayout.lock();

