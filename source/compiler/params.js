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
    assert (cfgObj.target instanceof Target);

    assert (cfgObj.tachyonSrc === true || cfgObj.tachyonSrc === false);

    assert (cfgObj.debug === true || cfgObj.debug === false);

    assert (cfgObj.parserWarnings === true || cfgObj.parserWarnings === false);

    assert (cfgObj.staticEnv instanceof StaticEnv);

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
    */
    this.ffiFuncs = {};    
}

