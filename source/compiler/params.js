/**
@fileOverview
Definition of compilation parameters.

@author
Maxime Chevalier-Boisvert, Erick Lavoie

@copyright
Copyright (c) 2010-2011 Tachyon Javascript Engine, All Rights Reserved
*/

/*
TODO: Should we include runtime, object representation here?
*/

/**
@class Description/grouping of compilation parameters required to compile a
       unit of source code.
*/
function CompParams(cfgObj)
{
    assert (
        cfgObj.target instanceof Target,
        'invalid target object'
    );

    assert (
        cfgObj.tachyonSrc === true || cfgObj.tachyonSrc === false,
        'invalid tachyon source flag'
    );

    assert (
        cfgObj.debug === true || cfgObj.debug === false,
        'invalid debug flag'
    );

    assert (
        cfgObj.parserWarnings === true || cfgObj.parserWarnings === false,
        'invalid parser warnings flag'
    );

    assert (
        cfgObj.staticEnv instanceof StaticEnv,
        'invalid static environment'
    );

    /**
    Target architecture
    @field
    */
    this.target = cfgObj.target;

    /**
    Flag indicating that we are compiling Tachyon source
    @field
    */
    this.tachyonSrc = cfgObj.tachyonSrc;

    /**
    Enable debug mode flag
    @field
    */
    this.debug = cfgObj.debug;

    /**
    Enable parser warnings flag
    @field
    */
    this.parserWarnings = cfgObj.parserWarnings;

    /**
    Static definitions to be used during compilation
    @field
    */
    this.staticEnv = cfgObj.staticEnv;

    /**
    Map of object memory layouts, by name
    @field
    */
    this.memLayouts = {};

    /**
    FFI functions used by compiler
    @field
    */
    this.ffiFuncs = {};

    /**
    Function to allocate string objects
    @field
    */
    this.getStrObj = null;
}

